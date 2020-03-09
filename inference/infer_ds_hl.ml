(*
 * Copyright 2018-2020 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

open Owl
open Types

module type DS_ll_S = sig
  type pstate = Infer_pf.pstate
  type ('p, 'a) ds_node

  val factor' : Infer_pf.pstate * float -> unit
  val factor : (Infer_pf.pstate * float, unit) Ztypes.cnode
  val value : ('a, 'b) ds_node -> 'b
  val get_distr_kind : ('a, 'b) ds_node -> kdistr
  val get_distr : ('a, 'b) ds_node -> 'b Distribution.t

  val observe_conditional :
    pstate -> ('a, 'b) ds_node -> ('b, 'c) cdistr -> 'c -> unit

  val assume_constant : 'a mdistr -> ('p, 'a) ds_node
  val assume_conditional :
    ('a, 'b) ds_node -> ('b, 'c) cdistr -> ('b, 'c) ds_node

  val shape : ('a, Mat.mat) ds_node -> int
  val is_realized : ('p, 'a) ds_node -> bool

  val copy_node :
    (int, Obj.t) Hashtbl.t -> ('a, 'b) ds_node -> ('a, 'b) ds_graph_node
end

module Make(DS_ll: DS_ll_S) = struct
  open Ztypes

  type pstate = DS_ll.pstate

  (** Delayed sampling expressions *)
  type _ expr_tree =
    | Econst : 'a -> 'a expr_tree
    | Ervar : 'a random_var -> 'a expr_tree
    | Eadd : float expr * float expr -> float expr_tree
    | Emult : float expr * float expr -> float expr_tree
    | Eapp : ('a -> 'b) expr * 'a expr -> 'b expr_tree
    | Epair : 'a expr * 'b expr -> ('a * 'b) expr_tree
    | Earray : 'a expr array -> 'a array expr_tree
    | Ematrix : 'a expr array array -> 'a array array expr_tree
    | Elist : 'a expr list -> 'a list expr_tree
    | Eite : bool expr * 'a expr * 'a expr -> 'a expr_tree
    | Emat_add : Mat.mat expr * Mat.mat expr -> Mat.mat expr_tree
    | Emat_scalar_mul : float expr * Mat.mat expr -> Mat.mat expr_tree
    | Emat_dot : Mat.mat expr * Mat.mat expr -> Mat.mat expr_tree
    | Evec_get : Mat.mat expr * int -> float expr_tree

  and 'a expr =
    { mutable value : 'a expr_tree }

  and 'a random_var =
    | RV : ('b, 'a) DS_ll.ds_node -> 'a random_var

  let factor' = DS_ll.factor'
  let factor = DS_ll.factor

  let const : 'a. 'a -> 'a expr =
    begin fun v ->
      { value = Econst v; }
    end

  let add : float expr * float expr -> float expr =
    begin fun (e1, e2) ->
      begin match e1.value, e2.value with
      | Econst x, Econst y -> { value = Econst (x +. y); }
      | _ -> { value = Eadd (e1, e2); }
      end
    end

  let ( +~ ) x y = add (x, y)

  let mult : float expr * float expr -> float expr =
    begin fun (e1, e2) ->
      begin match e1.value, e2.value with
      | Econst x, Econst y -> { value = Econst (x *. y); }
      | Ervar _, Econst _ -> { value = Emult(e2, e1); }
      | _ -> { value = Emult(e1, e2); }
      end
    end

  let ( *~ ) x y = mult (x, y)

  let app : type t1 t2. (t1 -> t2) expr * t1 expr -> t2 expr =
    begin fun (e1, e2) ->
      begin match e1.value, e2.value with
      | Econst f, Econst x -> { value = Econst (f x); }
      | _ -> { value = Eapp(e1, e2); }
      end
    end

  let ( @@~ ) f e = app (f, e)

  let pair (e1, e2) =
    { value = Epair (e1, e2) }

  let array a =
    { value = Earray a }

  let matrix a =
    { value = Ematrix a }


  let lst l =
    { value = Elist l}

  let ite : type a. bool expr -> a expr -> a expr -> a expr =
    begin fun i t e ->
      { value = Eite (i,t,e) }
    end

  let mat_add : (Mat.mat expr * Mat.mat expr) -> Mat.mat expr =
    begin fun (e1, e2) ->
      begin match e1.value, e2.value with
      | Econst x, Econst y ->  { value = Econst (Mat.add x y) }
      | _ -> { value = Emat_add (e1, e2) }
      end
    end

  let ( +@~ ) x y = mat_add (x, y)

  let mat_scalar_mult : float expr * Mat.mat expr -> Mat.mat expr =
    fun (e1, e2) ->
    begin match e1.value, e2.value with
    | Econst x, Econst y ->  { value = Econst (Mat.scalar_mul x y) }
    | _ -> { value = Emat_scalar_mul (e1, e2) }
    end

  let ( $*~ ) x y = mat_scalar_mult (x, y)

  let mat_dot : Mat.mat expr * Mat.mat expr -> Mat.mat expr =
    fun (e1, e2) ->
    begin match e1.value, e2.value with
    | Econst x, Econst y ->  { value = Econst (Mat.dot x y) }
    | _ -> { value = Emat_dot (e1, e2) }
    end

  let ( *@~ ) x y = mat_dot (x, y)

  let vec_get :  Mat.mat expr * int  -> float expr =
    fun (e ,i) ->
    begin match e.value with
    | Econst x -> { value =  Econst (Mat.get x i 0) }
    | _ -> { value = Evec_get (e, i)}
    end



  let rec eval : type t. t expr -> t =
    begin fun e ->
      begin match e.value with
      | Econst v -> v
      | Ervar (RV x) ->
          let v = DS_ll.value x in
          e.value <- Econst v;
          v
      | Eadd (e1, e2) ->
          let v = eval e1 +. eval e2 in
          e.value <- Econst v;
          v
      | Emult (e1, e2) ->
          let v = eval e1 *. eval e2 in
          e.value <- Econst v;
          v
      | Eapp (e1, e2) ->
          let v = (eval e1) (eval e2) in
          e.value <- Econst v;
          v
      | Epair (e1, e2) ->
          let v = (eval e1, eval e2) in
          e.value <- Econst v;
          v
      | Earray a ->
          Array.map eval a
      | Ematrix a ->
          Array.map (Array.map eval) a
      | Elist l ->
          List.map eval l
      | Eite (i,t,e) ->
          let v =
            if eval i then
              eval t
            else
              eval e
          in
          e.value <- Econst v;
          v
      (* | Emat_add ( { value = Emat_dot (e1, e2) } , e2) -> *)
      (*     let m1 = eval e1 in *)
      (*     let m2 = eval e2 in *)
      (*     let m3 = eval e3 in *)
      (*     let aux = Mat.dot m1 m2 in *)
      (*     Mat.add_ ~out:aux aux m3; *)
      (*     let v = aux in *)
      (*     e.value <- Econst v; *)
      (*     v *)

      | Emat_add (e1, e2) ->
          begin match e1.value with
          | Emat_dot (e11, e12) ->
              let m1 = eval e11 in
              let m2 = eval e12 in
              let m3 = eval e2 in
              let aux = Mat.dot m1 m2 in
              Mat.add_ ~out:aux aux m3;
              let v = aux in
              e.value <- Econst v;
              v
          | _ ->
              let v = Mat.add (eval e1) (eval e2) in
              e.value <- Econst v;
              v
          end
      | Emat_scalar_mul (e1, e2) ->
          let v = Mat.scalar_mul (eval e1) (eval e2) in
          e.value <- Econst v;
          v
      | Emat_dot (e1, e2) ->
          let v = Mat.dot (eval e1) (eval e2) in
          e.value <- Econst v;
          v
      | Evec_get (m, i) ->
          let v = Mat.get (eval m) i 0 in
          e.value <- Econst v;
          v
      end
    end

  (* let rec fval : type t. t expr -> t =
     begin fun e ->
      begin match e.value with
        | Econst v -> v
        | Ervar (RV x) -> DS_ll.fvalue x
        | Eadd (e1, e2) -> fval e1 +. fval e2
        | Emult (e1, e2) -> fval e1 *. fval e2
        | Eapp (e1, e2) -> (fval e1) (fval e2)
        | Epair (e1, e2) -> (fval e1, fval e2)
      end
     end *)

  let rec string_of_expr : float expr -> string =
    fun e ->
    begin match e.value with
    | Econst v -> string_of_float v
    | Ervar (RV _) -> "Random"
    | Eadd (e1, e2) ->
        "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
    | Emult (e1, e2) ->
        "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
    | Eapp (_e1, _e2) -> "App"
    | Evec_get _ -> "Get"
    | Eite (_, t, e) -> "(if ... then " ^ string_of_expr t ^ " else " ^ string_of_expr e ^ ")"
    | Emat_add (_, _) -> assert false
    | Emat_scalar_mul (_, _) -> assert false
    | Emat_dot (_, _) -> assert false

    end

  let string_of_bool_expr : bool expr -> string =
    fun e ->
    begin match e.value with
    | Econst _ -> "Econst"
    | Ervar _ -> "Ervar"
    | Eapp _ -> "App"
    | Eite (_, _, _) -> "Ite"
    | Emat_add (_, _) -> assert false
    | Emat_scalar_mul (_, _) -> assert false
    | Emat_dot (_, _) -> assert false
    end

  (* High level delayed sampling distribution (pdistribution in Haskell) *)
  type 'a ds_distribution =
    { isample : (pstate -> 'a expr);
      iobserve : (pstate * 'a -> unit); }

  let sample =
    let alloc () = () in
    let reset _state = () in
    let copy _src _dst = () in
    let step _state (prob, ds_distr) =
      ds_distr.isample prob
    in
    Cnode { alloc; reset; copy; step; }

  let observe =
    let alloc () = () in
    let reset _state = () in
    let copy _src _dst = () in
    let step _state (prob, (ds_distr, o)) =
      ds_distr.iobserve(prob, o)
    in
    Cnode { alloc; reset; copy; step; }

  let of_distribution d =
    { isample = (fun _prob -> const (Distribution.draw d));
      iobserve = (fun (prob, obs) -> factor' (prob, Distribution.score(d, obs))); }

  let ds_distr_with_fallback d is iobs =
    let dsd =
      let state = ref None in
      (fun () ->
         begin match !state with
         | None ->
             let dsd = of_distribution (d()) in
             state := Some dsd;
             dsd
         | Some dsd -> dsd
         end)
    in
    let is' prob =
      begin match is prob with
      | None -> (dsd()).isample prob
      | Some x -> x
      end
    in
    let iobs' (prob, obs) =
      begin match iobs (prob, obs) with
      | None -> (dsd()).iobserve (prob, obs)
      | Some () -> ()
      end
    in
    { isample = is'; iobserve = iobs'; }


  let type_of_random_var (RV rv) =
    DS_ll.get_distr_kind rv

  (* An affine_expr is either a constant or an affine transformation of a
   * random variable *)
  type 'a affine_expr =
    (* Interpretation (m, x, b) such that the output is m * x + b *)
    | AErvar of 'a * 'a random_var * 'a
    | AEconst of 'a

  let rec affine_of_expr : float expr -> float affine_expr option =
    begin fun expr ->
      begin match expr.value with
      | Econst v -> Some (AEconst v)
      | Ervar var -> Some (AErvar (1., var, 0.))
      | Eadd (e1, e2) ->
          begin match (affine_of_expr e1, affine_of_expr e2) with
          | (Some (AErvar (m, x, b)), Some (AEconst v))
          | (Some (AEconst v), Some (AErvar (m, x, b))) -> Some (AErvar (m, x, b +. v))
          | _ -> None
          end
      | Emult (e1, e2) ->
          begin match (affine_of_expr e1, affine_of_expr e2) with
          | (Some (AErvar (m, x, b)), Some (AEconst v))
          | (Some (AEconst v), Some (AErvar (m, x, b))) -> Some (AErvar (m *. v, x, b *. v))
          | _ -> None
          end
      | Eapp (_, _) -> None
      | Eite (_, _, _) -> None
      | Emat_add (_, _) -> assert false
      | Emat_scalar_mul (_, _) -> assert false
      | Emat_dot (_, _) -> assert false
      | Evec_get _ -> None
      end
    end

  let rec affine_vec_of_vec : Mat.mat expr -> Mat.mat affine_expr option =
    fun expr ->
    begin match expr.value with
    | Econst c -> Some (AEconst c)
    | Ervar (RV var) ->
        let sz = DS_ll.shape var in
        Some (AErvar (Mat.eye sz, (RV var), Mat.zeros sz 1))
    | Emat_add (e1, e2) ->
        begin match affine_vec_of_vec e1, affine_vec_of_vec e2 with
        | (Some (AErvar (m, x, b)), Some (AEconst c))
        | (Some (AEconst c), Some (AErvar (m, x, b))) ->
            Some (AErvar(m, x, Mat.add b c))
        | (Some (AEconst c1), Some (AEconst c2)) ->
            Some (AEconst (Mat.add c1 c2))
        | _ -> None
        end
    | Emat_scalar_mul (e1, e2) ->
        begin match e1.value, affine_vec_of_vec e2 with
        | Econst v1, Some (AErvar (m, x, b)) ->
            Some (AErvar (Mat.scalar_mul v1 m, x, Mat.scalar_mul v1 b))
        | Econst v1, Some (AEconst v2) ->
            Some (AEconst (Owl.Mat.scalar_mul v1 v2))
        | _ -> None
        end
    | Emat_dot (e1, e2) ->
        begin match e1.value, affine_vec_of_vec e2 with
        | Econst m1, Some (AErvar (m, x, b)) ->
            Some (AErvar (Mat.dot m1 m, x, Mat.dot m1 b))
        | Econst m1, Some (AEconst v2) ->
            Some (AEconst (Owl.Mat.dot m1 v2))
        | _ -> None
        end
    | _ -> None
    end

  (** Gaussian distribution (gaussianPD in Haskell) *)
  let gaussian (mu, sigma2) =
    let d () = Distribution.gaussian(eval mu, sigma2) in
    let is _prob =
      begin match affine_of_expr mu with
      | Some (AEconst v) ->
          let rv = DS_ll.assume_constant (Dist_gaussian(v, sigma2)) in
          Some { value = (Ervar (RV rv)) }
      | Some (AErvar (m, RV x, b)) ->
          begin match DS_ll.get_distr_kind x with
          | KGaussian ->
              let rv = DS_ll.assume_conditional x (AffineMeanGaussian(m, b, sigma2)) in
              Some { value = (Ervar (RV rv)) }
          | _ -> None
          end
      | None ->
          begin match mu.value with
          | Evec_get (m, i) ->
              begin match affine_vec_of_vec m with
              | Some (AEconst v) ->
                  let n, _ = Mat.shape v in
                  let mask = Mat.zeros 1 n in Mat.set mask 0 i 1.0;
                  let mu' = Mat.dot mask v in
                  let cov = Mat.create 1 1 sigma2 in
                  let rv =
                    DS_ll.assume_constant
                      (Dist_mv_gaussian(mu', cov, None))
                  in
                  Some { value = Evec_get ({ value = Ervar (RV rv)}, i)}
              | Some (AErvar (m, RV x, b)) ->
                  begin match DS_ll.get_distr_kind x with
                  | KMVGaussian ->
                      let n = DS_ll.shape x in
                      let mask = Mat.zeros 1 n in Mat.set mask 0 i 1.0;
                      let m' = Mat.dot mask m in
                      let b' = Mat.dot mask b in
                      let cov = Mat.create 1 1 sigma2 in
                      let rv =
                        DS_ll.assume_conditional x
                          (AffineMeanGaussianMV(m', b', cov))
                      in
                      Some { value = Evec_get ({ value = Ervar (RV rv)}, i)}
                  | _ -> None
                  end
              | _ -> None
              end
          | _ -> None
          end
      end
    in
    let iobs (prob, obs) =
      begin match affine_of_expr mu with
      | Some (AEconst _) ->
          None
      | Some (AErvar (m, RV x, b)) ->
          begin match DS_ll.get_distr_kind x with
          | KGaussian ->
              Some (DS_ll.observe_conditional prob x (AffineMeanGaussian(m, b, sigma2)) obs)
          | _ -> None
          end
      | None ->
          begin match mu.value with
          | Evec_get (m, i) ->
              begin match affine_vec_of_vec m with
              | Some (AEconst _) -> None
              | Some (AErvar (m, RV x, b)) ->
                  begin match DS_ll.get_distr_kind x with
                  | KMVGaussian ->
                      let n = DS_ll.shape x in
                      let mask = Mat.zeros 1 n in Mat.set mask 0 i 1.0;
                      let m' = Mat.dot mask m in
                      let b' = Mat.dot mask b in
                      let cov = Mat.create 1 1 sigma2 in
                      let obs' = Mat.create 1 1 obs in
                      Some (DS_ll.observe_conditional prob x (AffineMeanGaussianMV(m', b', cov)) obs')
                  | _ -> None
                  end
              | _ -> None
              end
          | _ -> None
          end
      end
    in
    ds_distr_with_fallback d is iobs


  let mv_gaussian : Mat.mat expr * Mat.mat -> Mat.mat ds_distribution =
    begin fun (mu, sigma) ->
      let d () = Distribution.mv_gaussian(eval mu, sigma) in
      let is _prob =
        begin match affine_vec_of_vec mu with
        | Some (AEconst v) ->
            let rv =
              DS_ll.assume_constant
                (Dist_mv_gaussian(v, sigma, None))
            in
            Some { value = (Ervar (RV rv)) }
        | Some (AErvar (m, RV x, b)) ->
            begin match DS_ll.get_distr_kind x with
            | KMVGaussian ->
                let rv =
                  DS_ll.assume_conditional x
                    (AffineMeanGaussianMV(m, b, sigma))
                in
                Some { value = (Ervar (RV rv)) }
            | _ -> None
            end
        | None -> None
        end
      in
      let iobs (prob, obs) =
        begin match affine_vec_of_vec mu with
        | Some (AEconst _) ->
            None
        | Some (AErvar (m, RV x, b)) ->
            begin match DS_ll.get_distr_kind x with
            | KMVGaussian ->
                Some (DS_ll.observe_conditional prob x
                        (AffineMeanGaussianMV(m, b, sigma)) obs)
            | _ -> None
            end
        | None -> None
        end
      in
      ds_distr_with_fallback d is iobs
    end


  (** Beta distribution (betaPD in Haskell) *)
  let beta(a, b) =
    let d () = Distribution.beta(a, b) in
    let is _prob =
      Some { value = Ervar (RV (DS_ll.assume_constant (Dist_beta (a, b)))) }
    in
    let iobs (_pstate, _obs) = None in
    ds_distr_with_fallback d is iobs

  (** Bernoulli distribution (bernoulliPD in Haskell) *)
  let bernoulli p =
    let d () = Distribution.bernoulli (eval p) in
    let is _prob =
      begin match p.value with
      | Ervar (RV par) ->
          begin match DS_ll.get_distr_kind par with
          | KBeta -> Some { value = Ervar (RV (DS_ll.assume_conditional par CBernoulli)) }
          | _ -> None
          end
      | Eite (e_i, e_t, e_f) ->
          begin match e_i.value with
          | Ervar (RV par) ->
              begin match DS_ll.get_distr_kind par with
              | KBernoulli -> 
                  let v_t = eval e_t in
                  let v_f = eval e_f in
                  Some {value = Ervar (RV (DS_ll.assume_conditional par (CBernBern (fun b ->
                      if b then
                        v_t
                      else
                        v_f
                    ))))}
              | _ -> None
              end
          | _ -> None
          end
      | _ ->
          let p_v = eval p in
          Some {value = Ervar (RV (DS_ll.assume_constant (Dist_bernoulli p_v)))}
      end
    in
    let iobs (prob, obs) =
      begin match p.value with
      | Ervar (RV par) ->
          begin match DS_ll.get_distr_kind par with
          | KBeta -> Some (DS_ll.observe_conditional prob par CBernoulli obs)
          | _ -> None
          end
      | Eite (e_i, e_t, e_f) ->
          begin match e_i.value with
          | Ervar (RV par) ->
              begin match DS_ll.get_distr_kind par with
              | KBernoulli -> 
                  let v_t = eval e_t in
                  let v_f = eval e_f in
                  Some (DS_ll.observe_conditional prob par (CBernBern (fun b ->
                      if b then
                        v_t
                      else
                        v_f
                    )) obs)
              | _ -> None
              end
          | _ -> None
          end
      | _ -> None
      end
    in
    ds_distr_with_fallback d is iobs

  (** Inference *)

  exception Stop

  let is_safe_marginal =
    let rec is_safe : type a. int -> a expr -> int =
      fun acc expr ->
        begin match expr.value with
        | Econst _c -> acc
        | Ervar (RV x) ->
            if DS_ll.is_realized x then acc
            else if acc > 0 then raise Stop
            else acc + 1
        | Eadd (e1, e2) ->
            is_safe (is_safe acc e1) e2
        | Emult (e1, e2) ->
            is_safe (is_safe acc e1) e2
        | Eapp (_e1, _e2) ->
            raise Stop
        | Epair (e1, e2) ->
            is_safe (is_safe acc e1) e2
        | Earray a ->
            Array.fold_left (fun acc e -> is_safe acc e) acc a
        | Ematrix m ->
            Array.fold_left
              (fun acc a ->
                 Array.fold_left (fun acc e -> is_safe acc e) acc a)
              acc m
        | Elist l ->
            List.fold_left (fun acc e -> is_safe acc e) acc l
        | Eite (e, e1, e2) ->
            is_safe (is_safe (is_safe acc e1) e2) e
        | Emat_add (e1, e2) ->
            is_safe (is_safe acc e1) e2
        | Emat_scalar_mul (e1, e2) ->
            is_safe (is_safe acc e1) e2
        | Emat_dot (e1, e2) ->
            is_safe (is_safe acc e1) e2
        | Evec_get (e, _) ->
            is_safe acc e
        end
    in
    fun expr ->
      try ignore (is_safe 0 expr); true
      with Stop -> false

  let sample_of_expr : type a. a expr -> a =
    fun expr ->
    let e = Probzelus_utils.copy expr in
    eval e

  let rec marginal_distribution_of_expr : type a. a expr -> a Distribution.t =
    fun expr ->
    begin match expr.value with
    | Econst c -> Dist_support [c, 1.]
    | Ervar (RV x) -> DS_ll.get_distr x
    | Eadd (e1, e2) ->
        Dist_add (marginal_distribution_of_expr e1,
                  marginal_distribution_of_expr e2)
    | Emult (e1, e2) ->
        Dist_mult (marginal_distribution_of_expr e1,
                   marginal_distribution_of_expr e2)
    | Eapp (e1, e2) ->
        Dist_app (marginal_distribution_of_expr e1,
                  marginal_distribution_of_expr e2)
    | Epair (e1, e2) ->
        Dist_pair (marginal_distribution_of_expr e1,
                   marginal_distribution_of_expr e2)
    | Earray a ->
        Dist_array (Array.map marginal_distribution_of_expr a)
    | Ematrix a ->
        Dist_array
          (Array.map
             (fun ai ->
                Dist_array
                  (Array.map marginal_distribution_of_expr ai))
             a)
    | Elist l ->
        Dist_list (List.map marginal_distribution_of_expr l)
    | Eite (_, _, _) -> (* XXX TODO XXX *)
        assert false
    | Emat_add (_, _) ->
        assert false (* XXX TODO XXX *)
    | Emat_scalar_mul (_e1, _e2) ->
        assert false (* XXX TODO XXX *)
    | Emat_dot (_e1, _e2) ->
        assert false (* XXX TODO XXX *)
    | Evec_get _ ->
        assert false (* XXX TODO XXX *)
    end

  let rec joint_distribution_of_expr : type a.
    (int, Obj.t) Hashtbl.t -> a expr -> a joint_distr =
    fun tbl expr ->
    begin match expr.value with
    | Econst c -> JDist_const c
    | Ervar (RV x) ->
        JDist_rvar (RV (DS_ll.copy_node tbl x))
    | Eadd (e1, e2) ->
        JDist_add (joint_distribution_of_expr tbl e1,
                   joint_distribution_of_expr tbl e2)
    | Emult (e1, e2) ->
        JDist_mult (joint_distribution_of_expr tbl e1,
                    joint_distribution_of_expr tbl e2)
    | Eapp (e1, e2) ->
        JDist_app (joint_distribution_of_expr tbl e1,
                   joint_distribution_of_expr tbl e2)
    | Epair (e1, e2) ->
        JDist_pair (joint_distribution_of_expr tbl e1,
                    joint_distribution_of_expr tbl e2)
    | Earray a ->
        JDist_array (Array.map (joint_distribution_of_expr tbl) a)
    | Ematrix a ->
        JDist_array
          (Array.map
             (fun ai ->
                JDist_array
                  (Array.map (joint_distribution_of_expr tbl) ai))
             a)
    | Elist l ->
        JDist_list (List.map (joint_distribution_of_expr tbl) l)
    | Eite (e, e1, e2) ->
        JDist_ite (joint_distribution_of_expr tbl e,
                   joint_distribution_of_expr tbl e1,
                   joint_distribution_of_expr tbl e2)
    | Emat_add (e1, e2) ->
        JDist_mat_add (joint_distribution_of_expr tbl e1,
                       joint_distribution_of_expr tbl e2)
    | Emat_scalar_mul (e1, e2) ->
        JDist_mat_scalar_mul (joint_distribution_of_expr tbl e1,
                              joint_distribution_of_expr tbl e2)
    | Emat_dot (e1, e2) ->
        JDist_mat_dot (joint_distribution_of_expr tbl e1,
                       joint_distribution_of_expr tbl e2)
    | Evec_get (e, n) ->
        JDist_vec_get (joint_distribution_of_expr tbl e, n)
    end

  let distribution_of_expr : type a. a expr -> a Distribution.t =
    fun expr ->
    if is_safe_marginal expr then
      marginal_distribution_of_expr expr
    else
      let tbl = Hashtbl.create 7 in
      Dist_joint (joint_distribution_of_expr tbl expr)

  type infer_dist_kind =
    | Infer_sample
    | Infer_marginal
    | Infer_graph
    | Infer_bounded

  type infer_resampling_kind =
    | Infer_resample
    | Infer_ess of float

  (** Auxiliary infer function that can select the kind of resampling
      and the kind of returned distribution. This function works for
      step function that returns a distribution. The infer thus
      returns a mixture distribution.
  *)
  let infer_aux_mixture resample_kind dist_kind
      n (Cnode { alloc; reset; copy = _; step; }) =
    let alloc () = ref (alloc ()) in
    let reset state = reset !state in
    let step =
      begin match dist_kind with
      | Infer_marginal ->
          (fun state (prob, x) ->
             marginal_distribution_of_expr (step !state (prob, x)))
      | Infer_graph ->
          (fun state (prob, x) -> distribution_of_expr (step !state (prob, x)))
      | Infer_sample -> assert false
      | Infer_bounded -> assert false
      end
    in
    let copy src dst = dst := Probzelus_utils.copy !src in
    let Cnode {alloc = infer_alloc; reset = infer_reset;
               copy = infer_copy; step = infer_step;} =
      begin match resample_kind with
      | Infer_resample ->
          Infer_pf.infer n (Cnode { alloc; reset; copy = copy; step; })
      | Infer_ess threshold ->
          Infer_pf.infer_ess_resample n threshold
            (Cnode { alloc; reset; copy; step; })
      end
    in
    let infer_step state i =
      Distribution.to_mixture (infer_step state i)
    in
    Cnode {alloc = infer_alloc; reset = infer_reset;
           copy = infer_copy;  step = infer_step; }

  (** Auxiliary infer function that can select the kind of resampling
      and the kind of returned distribution. This function works for
      step function that returns a value. The infer thus
      returns a support distribution.
  *)
  let infer_aux_support resample_kind dist_kind
      n (Cnode { alloc; reset; copy = _; step; }) =
    let alloc () = ref (alloc ()) in
    let reset state = reset !state in
    let step =
      begin match dist_kind with
      | Infer_sample ->
          (fun state (prob, x) -> sample_of_expr (step !state (prob, x)))
      | Infer_bounded ->
          (fun state (prob, x) -> eval (step !state (prob, x)))
      | Infer_marginal -> assert false
      | Infer_graph -> assert false
      end
    in
    let copy src dst = dst := Probzelus_utils.copy !src in
    begin match resample_kind with
    | Infer_resample ->
        Infer_pf.infer n (Cnode { alloc; reset; copy = copy; step; })
    | Infer_ess threshold ->
        Infer_pf.infer_ess_resample n threshold
          (Cnode { alloc; reset; copy; step; })
    end

  let infer_aux resample_kind dist_kind =
    begin match dist_kind with
    | Infer_sample | Infer_bounded ->
        infer_aux_support resample_kind dist_kind
    | Infer_marginal | Infer_graph ->
        infer_aux_mixture resample_kind dist_kind
    end

  let infer_sample n f =
    infer_aux Infer_resample Infer_sample n f

  let infer_marginal n f =
    infer_aux Infer_resample Infer_marginal n f

  let infer n f =
    infer_aux Infer_resample Infer_graph n f

  let infer_bounded n f =
    infer_aux Infer_resample Infer_bounded n f

  let infer_sample_ess_resample n threshold f =
    infer_aux (Infer_ess threshold) Infer_sample n f

  let infer_marginal_ess_resample n threshold f =
    infer_aux (Infer_ess threshold) Infer_marginal n f

  let infer_ess_resample n threshold f =
    infer_aux (Infer_ess threshold) Infer_graph n f

  let infer_bounded_ess_resample n threshold f =
    infer_aux (Infer_ess threshold) Infer_bounded n f


  let gen (Cnode { alloc; reset; copy = _; step; }) =
    let alloc () = ref (alloc ()) in
    let reset state = reset !state in
    let step state (prob, x) = eval (step !state (prob, x)) in
    let copy src dst = dst := Probzelus_utils.copy !src in
    let Cnode {alloc = gen_alloc; reset = gen_reset;
               copy = gen_copy; step = gen_step;} =
      Infer_pf.gen (Cnode { alloc; reset; copy = copy; step; })
    in
    Cnode {alloc = gen_alloc; reset = gen_reset;
           copy = gen_copy;  step = gen_step; }

end
