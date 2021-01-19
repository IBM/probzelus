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

(** Inference with delayed sampling *)
open Ztypes
open Types

(* type 'a random_var = RV : ('b, 'a) Ds_streaming_low_level.ds_node -> 'a random_var *)
type 'a random_var = { rv_id : int; }
type ('a, 'b) ds_node = ('a, 'b) ds_graph_node

(* module Gnodes = struct *)
(*   module E = Ephemeron.K1 *)

(*   module M = Hashtbl.Make(struct *)
(*       type t = int *)
(*       let equal (x: int) (y: int) = (x = y) *)
(*       let hash (x: int) = Hashtbl.hash x *)
(*       (\* let compare (x:int) (y:int) = compare x y *\) *)
(*     end) *)

(*   (\* type t = (Obj.t random_var, (Obj.t, Obj.t) ds_node) E.t  M.t ref *\) *)
(*   type ephemeron = (Obj.t random_var, unit) E.t *)
(*   type t = *)
(*     { live_nodes : (ephemeron * (Obj.t, Obj.t) ds_node) M.t; *)
(*       mutable ephemeron_pool: ephemeron list; } *)

(*   let create _ = *)
(*     { live_nodes = M.create 11; *)
(*       ephemeron_pool = []; } *)

(*   let new_ephemeron g = *)
(*     begin match g.ephemeron_pool with *)
(*       | [] ->  E.create () *)
(*       | e::p -> g.ephemeron_pool <- p; e *)
(*     end *)

(*   let add: type a p. *)
(*     t -> a random_var -> (p, a) ds_node -> unit = *)
(*     fun g x y -> *)
(*       let e = new_ephemeron g in *)
(*       E.set_key e (Obj.magic x: Obj.t random_var); *)
(*       (\* E.set_data e (Obj.magic y: (Obj.t, Obj.t) ds_node); *\) *)
(*       let n =  (Obj.magic y: (Obj.t, Obj.t) ds_node) in *)
(*       M.add g.live_nodes x.rv_id (e, n) *)

(*   let find_opt: type a p. *)
(*     t -> a random_var -> (p, a) ds_node option = *)
(*     fun g x -> *)
(*       let k = (Obj.magic x: Obj.t random_var).rv_id in *)
(*       begin match M.find_opt g.live_nodes k with *)
(*         | None -> None *)
(*         | Some (e, n) -> Some (Obj.magic n: (p, a) ds_node) *)
(*       end *)

(*   let clear: t -> unit = *)
(*     fun g -> *)
(*       g.ephemeron_pool <- *)
(*         M.fold *)
(*           (fun _ (e, _) acc -> E.unset_key e; e::acc) *)
(*           g.live_nodes *)
(*           g.ephemeron_pool; *)
(*       M.clear g.live_nodes *)

(*   let clean: t -> unit = *)
(*     fun g -> *)
(*       M.filter_map_inplace *)
(*         (fun _ (e, n) -> *)
(*            let b = E.check_key e in *)
(*            if not b then begin *)
(*              g.ephemeron_pool <- e::g.ephemeron_pool; *)
(*              None *)
(*            end *)
(*            else Some (e, n)) *)
(*         g.live_nodes *)

(*   let copy: t -> t -> unit = *)
(*     fun src dst -> *)
(*       let tbl = Hashtbl.create 11 in *)
(*       (\* clean src; *\) *)
(*       clear dst; *)
(*       M.iter (fun k (e, n) -> *)
(*           let e' = new_ephemeron dst in *)
(*           begin match E.get_key e with *)
(*             | Some x ->  E.set_key e' x; *)
(*             | _ -> () *)
(*           end; *)
(*           let n' = Ds_streaming_low_level.copy_node tbl n in *)
(*           M.add dst.live_nodes k (e', n')) *)
(*         src.live_nodes *)
(* end *)


module Gnodes = struct
  module E = Ephemeron.K1

  module M = Map.Make(struct
      type t = int
      let compare (x:int) (y:int) = compare x y
    end)

  (* type t = (Obj.t random_var, (Obj.t, Obj.t) ds_node) E.t  M.t ref *)
  type ephemeron = (Obj.t random_var, unit) E.t
  type t =
    { mutable live_nodes : (ephemeron * (Obj.t, Obj.t) ds_node) M.t;
      mutable ephemeron_pool: ephemeron list; }

  let create _ =
    { live_nodes = M.empty;
      ephemeron_pool = []; }

  let new_ephemeron g =
    begin match g.ephemeron_pool with
    | [] ->  E.create ()
    | e::p -> g.ephemeron_pool <- p; e
    end

  let add: type a p.
    t -> a random_var -> (p, a) ds_node -> unit =
    fun g x y ->
    let e = new_ephemeron g in
    E.set_key e (Obj.magic x: Obj.t random_var);
    (* E.set_data e (Obj.magic y: (Obj.t, Obj.t) ds_node); *)
    let n =  (Obj.magic y: (Obj.t, Obj.t) ds_node) in
    g.live_nodes <- M.add x.rv_id (e, n) g.live_nodes

  let find_opt: type a p.
    t -> a random_var -> (p, a) ds_node option =
    fun g x ->
    let k = (Obj.magic x: Obj.t random_var).rv_id in
    begin match M.find_opt k g.live_nodes with
    | None -> None
    | Some (_e, n) -> Some (Obj.magic n: (p, a) ds_node)
    end

  let clear: t -> unit =
    fun g ->
    g.ephemeron_pool <-
      M.fold
        (fun _ (e, _) acc -> E.unset_key e; e::acc)
        g.live_nodes
        g.ephemeron_pool;
    g.live_nodes <- M.empty

  let clean: t -> unit =
    fun g ->
    g.live_nodes <- M.filter
        (fun _ (e, _) ->
           let b = E.check_key e in
           if not b then g.ephemeron_pool <- e::g.ephemeron_pool;
           b)
        g.live_nodes

  let copy: t -> t -> unit =
    fun src dst ->
    let tbl = Hashtbl.create 41 in
    (* clean src; *)
    clear dst;
    dst.live_nodes <- M.map (fun (e, n) ->
        let e' = new_ephemeron dst in
        begin match E.get_key e with
        | Some x ->  E.set_key e' x;
        | _ -> ()
        end;
        let n' = Distribution.DS_graph.copy_node tbl n in
        (e', n'))
        src.live_nodes
end

(* module Gnodes = struct *)
(*    module E = Ephemeron.K1 *)

(*    module H = Ephemeron.K1.Make(struct *)
(*        (\* module Gnodes = Hashtbl.Make(struct *\) *)
(*        type t = Obj.t random_var (\* random_var *\) *)
(*        let equal x y = x.rv_id = y.rv_id *)
(*        let hash x = Hashtbl.hash x.rv_id *)
(*      end) *)

(*    type t = (Obj.t, Obj.t) ds_node H.t *)

(*    let create n = H.create n *)

(*    let add: type a p. *)
(*      t -> a random_var -> (p, a) ds_node -> unit = *)
(*      fun g x y -> *)
(*      let n = (Obj.magic y: (Obj.t, Obj.t) ds_node) in *)
(*      H.add g (Obj.magic x: Obj.t random_var) n *)

(*    let find_opt: type a p. *)
(*     t -> a random_var -> (p, a) ds_node option = *)
(*     fun g x -> *)
(*       let k = (Obj.magic x: Obj.t random_var) in *)
(*       begin match H.find_opt g k with *)
(*         | None -> None *)
(*         | Some n -> Some (Obj.magic n: (p, a) ds_node) *)
(*       end *)

(*    let clear: t -> unit = H.clear *)

(*    let clean: t -> unit = H.clean *)

(*    let copy: t -> t -> unit = *)
(*     fun src dst -> *)
(*       let tbl = Hashtbl.create 11 in *)
(*       (\* clean src; *\) *)
(*       clear dst; *)
(*       H.iter *)
(*         (fun k n -> H.add dst k (Ds_streaming_low_level.copy_node tbl n)) *)
(*         src *)
(*  end *)


type pstate =
  { pf_state: Ds_streaming_low_level.pstate;
    ds_graph: Gnodes.t; }

let rv_node : type a p.
  pstate -> a random_var -> (p, a) ds_graph_node =
  fun prob x ->
  let g = prob.ds_graph in
  begin match Gnodes.find_opt g x with
  | None ->
      Format.eprintf "Failed %d@." x.rv_id;
      assert false
  | Some o -> o
  end

let add_random_var: type a p.
  pstate -> a random_var -> (p, a) ds_graph_node -> unit =
  fun prob rv n ->
  let g = prob.ds_graph in
  Gnodes.add g rv n

let rv_kind prob rv =
  let n = rv_node prob rv in
  Ds_streaming_low_level.get_distr_kind n

let rv_distr prob rv =
  let n = rv_node prob rv in
  Ds_streaming_low_level.get_distr n

let factor' (prob, s) = Ds_streaming_low_level.factor' (prob.pf_state, s)
let factor =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    factor' input
  in
  Cnode { alloc; reset; copy; step; }

type _ expr_tree =
  | Econst : 'a -> 'a expr_tree
  | Ervar : 'a random_var -> 'a expr_tree
  | Eadd : float expr * float expr -> float expr_tree
  | Emult : float expr * float expr -> float expr_tree
  | Eapp : ('a -> 'b) expr * 'a expr -> 'b expr_tree
  | Epair : 'a expr * 'b expr -> ('a * 'b) expr_tree
  | Earray : 'a expr array -> 'a array expr_tree
and 'a expr = { mutable value : 'a expr_tree; }

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

let app  : type t1 t2. (t1 -> t2) expr * t1 expr -> t2 expr =
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

let rec eval' : type t.
  pstate -> t expr -> t =
  begin fun prob e ->
    begin match e.value with
    | Econst v -> v
    | Ervar x ->
        let n = rv_node prob x in
        let v = Ds_streaming_low_level.value n in
        e.value <- Econst v;
        v
    | Eadd (e1, e2) ->
        let v = eval' prob e1 +. eval' prob e2 in
        e.value <- Econst v;
        v
    | Emult (e1, e2) ->
        let v = eval' prob e1 *. eval' prob e2 in
        e.value <- Econst v;
        v
    | Eapp (e1, e2) ->
        let v = (eval' prob e1) (eval' prob e2) in
        e.value <- Econst v;
        v
    | Epair (e1, e2) ->
        let v = (eval' prob e1, eval' prob e2) in
        e.value <- Econst v;
        v
    | Earray a ->
        Array.map (eval' prob) a
    end
  end

let eval =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state (prob, input) =
    eval' prob input
  in
  Cnode { alloc; reset; copy; step; }


(* let rec fval : type t. t expr -> t =
   begin fun e ->
   begin match e.value with
   | Econst v -> v
   | Ervar (RV x) -> Ds_streaming_low_level.fvalue x
   | Eadd (e1, e2) -> fval e1 +. fval e2
   | Emult (e1, e2) -> fval e1 *. fval e2
   | Eapp (e1, e2) -> (fval e1) (fval e2)
   | Epair (e1, e2) -> (fval e1, fval e2)
   end
   end *)

let rec string_of_expr e =
  begin match e.value with
  | Econst v -> string_of_float v
  | Ervar x -> "RV_" ^ string_of_int x.rv_id
  | Eadd (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Emult (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Eapp (_, _) -> "App"
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
    (fun prob ->
       begin match !state with
       | None ->
           let dsd = of_distribution (d prob) in
           state := Some dsd;
           dsd
       | Some dsd -> dsd
       end)
  in
  let is' prob =
    begin match is prob with
    | None -> (dsd prob).isample prob
    | Some x -> x
    end
  in
  let iobs' (prob, obs) =
    begin match iobs (prob, obs) with
    | None -> (dsd prob).iobserve (prob, obs)
    | Some () -> ()
    end
  in
  { isample = is'; iobserve = iobs'; }

(* An affine_expr is either a constant or an affine transformation of a
 * random variable *)
type affine_expr =
  (* Interpretation (m, x, b) such that the output is m * x + b *)
  | AErvar of float * float random_var * float
  | AEconst of float

let rec affine_of_expr : float expr -> affine_expr option =
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
    end
  end

let assume_constant : type a.
  pstate -> a mdistr -> a random_var =
  fun prob d ->
  let n = Ds_streaming_low_level.assume_constant d in
  let rv = { rv_id = n.ds_graph_node_id } in
  add_random_var prob rv n;
  rv

let assume_conditional : type b c.
  pstate -> b random_var -> (b, c) cdistr -> c random_var =
  fun prob p d ->
  let par = rv_node prob p in
  let n = Ds_streaming_low_level.assume_conditional par d in
  let rv = { rv_id = n.ds_graph_node_id } in
  add_random_var prob rv n;
  rv

let observe_conditional : type b c.
  pstate -> b random_var -> (b, c) cdistr -> c -> unit =
  fun prob p cdistr obs ->
  let par = rv_node prob p in
  let _ = assume_conditional prob p cdistr in
  Ds_streaming_low_level.observe_conditional prob.pf_state par cdistr obs


(** Gaussian distribution (gaussianPD in Haskell) *)
let gaussian (mu, std) =
  let d prob = Distribution.gaussian(eval' prob mu, std) in
  let is prob =
    begin match affine_of_expr mu with
    | Some (AEconst v) ->
        let rv = assume_constant prob (Dist_gaussian(v, std)) in
        Some { value = (Ervar rv) }
    | Some (AErvar (m, x, b)) ->
        begin match rv_kind prob x with
        | KGaussian ->
            let rv =
              assume_conditional prob x (AffineMeanGaussian(m, b, std))
            in
            Some { value = (Ervar rv) }
        | _ -> None
        end
    | None -> None
    end
  in
  let iobs (prob, obs) =
    begin match affine_of_expr mu with
    | Some (AEconst _) ->
        None
    | Some (AErvar (m, x, b)) ->
        begin match rv_kind prob x with
        | KGaussian ->
            Some (observe_conditional prob x (AffineMeanGaussian(m, b, std)) obs)
        | _ -> None
        end
    | None -> None
    end
  in
  ds_distr_with_fallback d is iobs

(** Beta distribution (betaPD in Haskell) *)
let beta (a, b) =
  let d _prob = Distribution.beta(a, b) in
  let is prob =
    Some { value = Ervar (assume_constant prob (Dist_beta (a, b))) }
  in
  let iobs (_prob, _obs) = None in
  ds_distr_with_fallback d is iobs

(** Bernoulli distribution (bernoulliPD in Haskell) *)
let bernoulli p =
  let d prob = Distribution.bernoulli (eval' prob p) in
  let with_beta_prior prob f =
    begin match p.value with
    | Ervar par ->
        begin match rv_kind prob par with
        | KBeta -> Some (f par)
        | _ -> None
        end
    | _ -> None
    end
  in
  let is prob =
    with_beta_prior prob
      (fun par ->
         { value = Ervar (assume_conditional prob par CBernoulli) })
  in
  let iobs (prob, obs) =
    with_beta_prior prob
      (fun par -> observe_conditional prob par CBernoulli obs)
  in
  ds_distr_with_fallback d is iobs

(** Inference *)

let rec distribution_of_expr : type a. pstate -> a expr -> a Distribution.t =
  fun prob expr ->
  begin match expr.value with
  | Econst c -> Dist_support [c, 1.]
  | Ervar x -> rv_distr prob x
  | Eadd (e1, e2) ->
      Dist_add (distribution_of_expr prob e1, distribution_of_expr prob e2)
  | Emult (e1, e2) ->
      Dist_mult (distribution_of_expr prob e1, distribution_of_expr prob e2)
  | Eapp (e1, e2) ->
      Dist_app (distribution_of_expr prob e1, distribution_of_expr prob e2)
  | Epair (e1, e2) ->
      Dist_pair (distribution_of_expr prob e1, distribution_of_expr prob e2)
  | Earray a ->
      Dist_array (Array.map (distribution_of_expr prob) a)
  end

type 'a node_state =
  { node_state: 'a;
    node_graph: Gnodes.t; }

let infer n (Cnode { alloc; reset; copy; step; }) =
  let alloc () =
    { node_state = alloc ();
      node_graph = Gnodes.create 11; }
  in
  let reset state =
    reset state.node_state;
    Gnodes.clear state.node_graph
  in
  let step state (pf_prob, x) =
    let prob =
      { pf_state = pf_prob;
        ds_graph = state.node_graph; }
    in
    let d = distribution_of_expr prob (step state.node_state (prob, x)) in
    Gnodes.clean state.node_graph;
    d
  in
  let copy src dst =
    copy src.node_state dst.node_state;
    Gnodes.copy src.node_graph dst.node_graph
  in
  let Cnode {alloc = infer_alloc; reset = infer_reset;
             copy = infer_copy; step = infer_step;} =
    Infer_pf.infer n (Cnode { alloc; reset; copy = copy; step; })
  in
  let infer_step state i =
    Distribution.to_mixture (infer_step state i)
  in
  Cnode {alloc = infer_alloc; reset = infer_reset;
         copy = infer_copy;  step = infer_step; }


let infer_ess_resample n threshold (Cnode { alloc; reset; copy; step; }) =
  let alloc () =
    { node_state = alloc ();
      node_graph = Gnodes.create 11; }
  in
  let reset state =
    reset state.node_state;
    Gnodes.clear state.node_graph
  in
  let step state (pf_prob, x) =
    let prob =
      { pf_state = pf_prob;
        ds_graph = state.node_graph; }
    in
    let d = distribution_of_expr prob (step state.node_state (prob, x)) in
    Gnodes.clean state.node_graph;
    d
  in
  let copy src dst =
    copy src.node_state dst.node_state;
    Gnodes.copy src.node_graph dst.node_graph
  in
  let Cnode {alloc = infer_alloc; reset = infer_reset;
             copy = infer_copy; step = infer_step;} =
    Infer_pf.infer_ess_resample n threshold
      (Cnode { alloc; reset; copy; step; })
  in
  let infer_step state i =
    Distribution.to_mixture (infer_step state i)
  in
  Cnode {alloc = infer_alloc; reset = infer_reset;
         copy = infer_copy; step = infer_step;}

let infer_bounded n (Cnode { alloc; reset; copy; step; }) =
  let alloc () =
    { node_state = alloc ();
      node_graph = Gnodes.create 11; }
  in
  let reset state =
    reset state.node_state;
    Gnodes.clear state.node_graph
  in
  let step state (pf_prob, x) =
    let prob =
      { pf_state = pf_prob;
        ds_graph = state.node_graph; }
    in
    let v = eval' prob (step state.node_state (prob, x)) in
    Gnodes.clean state.node_graph;
    v
  in
  let copy src dst =
    copy src.node_state dst.node_state;
    Gnodes.copy src.node_graph dst.node_graph
  in
  Infer_pf.infer n (Cnode { alloc; reset; copy; step; })
