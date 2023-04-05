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
open Ds_graph_utils
open Types

(** Naive delayed sampling *)

(** Random variable of type ['b] and with parent of type ['a] *)
type ('p, 'a) ds_naive_node =
  { ds_naive_node_id : int;
    mutable ds_naive_node_children : 'a ds_naive_child list;
    mutable ds_naive_node_state : ('p, 'a) ds_naive_state; }

and ('p, 'a) ds_naive_state =
  | DSnaive_Initialized:
      ('z, 'p) ds_naive_node * ('p, 'a) cdistr
      -> ('p, 'a) ds_naive_state
  | DSnaive_Marginalized:
      'a mdistr * (('z, 'p) ds_naive_node * ('p, 'a) cdistr) option
      -> ('p, 'a) ds_naive_state
  | DSnaive_Realized of 'a

and 'b ds_naive_child =
    DSnaive_Child : ('b, 'c) ds_naive_node -> 'b ds_naive_child


(** {2 Graph manipulations} *)

let fresh_id =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    !cpt

(* initialize without parent node *)
let assume_constant : type a p.
  a mdistr -> (p, a) ds_naive_node =
  fun d ->
  { ds_naive_node_id = fresh_id ();
    ds_naive_node_children = [];
    ds_naive_node_state = DSnaive_Marginalized (d, None); }

(* initialize with parent node *)
let assume_conditional : type a b c.
  (a, b) ds_naive_node -> (b, c) cdistr -> (b, c) ds_naive_node =
  fun p cdistr ->
  let child =
    { ds_naive_node_id = fresh_id ();
      ds_naive_node_children = [];
      ds_naive_node_state = DSnaive_Initialized (p, cdistr); }
  in
  p.ds_naive_node_children <- DSnaive_Child child :: p.ds_naive_node_children;
  child

let marginalize : type a b.
  (a, b) ds_naive_node -> unit =
  fun n ->
  begin match n.ds_naive_node_state with
  | DSnaive_Initialized (p, cdistr) ->
      begin match p.ds_naive_node_state with
      | DSnaive_Realized x ->
          let mdistr = cdistr_to_mdistr cdistr x in
          n.ds_naive_node_state <- DSnaive_Marginalized (mdistr, None)
      | DSnaive_Marginalized (p_mdistr, _) ->
          let mdistr = make_marginal p_mdistr cdistr in
          n.ds_naive_node_state <- DSnaive_Marginalized (mdistr, Some(p, cdistr))
      | DSnaive_Initialized _ -> assert false
      end
  | DSnaive_Marginalized _ | DSnaive_Realized _ -> assert false
  end

let rec delete : type a b.
  (a, b) ds_naive_node -> a ds_naive_child list -> a ds_naive_child list =
  fun n l ->
  begin match l with
  | DSnaive_Child x :: l ->
      if n.ds_naive_node_id = x.ds_naive_node_id then l
      else DSnaive_Child x :: (delete n l)
  | [] -> assert false
  end

let realize : type a b.
  b -> (a, b) ds_naive_node -> unit =
  fun obs n ->
  begin match n.ds_naive_node_state with
  | DSnaive_Marginalized (_mdistr, None) -> ()
  | DSnaive_Marginalized (_mdistr, Some (p, cdistr)) ->
      begin match p.ds_naive_node_state with
      | DSnaive_Marginalized (p_mdistr, edge) ->
          let mdistr = make_conditional p_mdistr cdistr obs in
          p.ds_naive_node_state <- DSnaive_Marginalized (mdistr, edge);
          p.ds_naive_node_children <- delete n p.ds_naive_node_children
      | DSnaive_Initialized _ | DSnaive_Realized _ -> assert false
      end
  | DSnaive_Initialized _ | DSnaive_Realized _ -> assert false
  end;
  n.ds_naive_node_state <- DSnaive_Realized obs;
  List.iter (fun (DSnaive_Child c) -> marginalize c) n.ds_naive_node_children;
  n.ds_naive_node_children <- []

let sample : type a b.
  (a, b) ds_naive_node -> unit =
  fun n ->
  begin match n.ds_naive_node_state with
  | DSnaive_Marginalized (m, _) ->
      let x = Distribution.draw m in
      realize x n
  | DSnaive_Initialized _ | DSnaive_Realized _ -> assert false
  end

(* Invariant 2: A node always has at most one marginal DSnaive_Child *)
let marginal_child : type a b.
  (a, b) ds_naive_node -> b ds_naive_child option =
  let is_marginalized state =
    begin match state with
    | DSnaive_Initialized _ | DSnaive_Realized _ -> false
    | DSnaive_Marginalized _ -> true
    end
  in
  fun n ->
    List.find_opt
      (fun (DSnaive_Child x) -> is_marginalized x.ds_naive_node_state)
      n.ds_naive_node_children

let rec prune : type a b.
  (a, b) ds_naive_node -> unit =
  function n ->
    begin match marginal_child n with
    | Some (DSnaive_Child child) -> prune child
    | None -> ()
    end;
    sample n

let rec graft : type a b.
  (a, b) ds_naive_node -> unit =
  function n ->
    begin match n.ds_naive_node_state with
    | DSnaive_Realized _ -> assert false
    | DSnaive_Marginalized _ ->
        begin match marginal_child n with
        | Some (DSnaive_Child child) -> prune child
        | None -> ()
        end
    | DSnaive_Initialized (p, _cdistr) ->
        graft p;
        marginalize n
    end

let rec value: type a b.
  (a, b) ds_naive_node -> b =
  fun n ->
  begin match n.ds_naive_node_state with
  | DSnaive_Realized x -> x
  | DSnaive_Marginalized _ | DSnaive_Initialized _ ->
      graft n;
      sample n;
      value n
  end

let rec get_mdistr : type a b.
  (a, b) ds_naive_node -> b mdistr =
  function n ->
    begin match n.ds_naive_node_state with
    | DSnaive_Marginalized (m, _) -> m
    | DSnaive_Initialized (p, cdistr) ->
        begin match p.ds_naive_node_state with
        | DSnaive_Realized x ->
            cdistr_to_mdistr cdistr x
        | DSnaive_Marginalized (p_mdistr, _) ->
            make_marginal p_mdistr cdistr
        | DSnaive_Initialized _ ->
            let p_mdistr = get_mdistr p in
            make_marginal p_mdistr cdistr
        end
    | DSnaive_Realized _ -> assert false
    end

let get_distr : type a b.
  (a, b) ds_naive_node -> b Distribution.t =
  fun n ->
  begin match n.ds_naive_node_state with
  | DSnaive_Realized x -> Dist_support [ (x, 1.) ]
  | DSnaive_Initialized _ | DSnaive_Marginalized _ -> get_mdistr n
  end

let get_distr_kind : type a b.
  (a, b) ds_naive_node -> kdistr =
  fun n  ->
  begin match n.ds_naive_node_state with
  | DSnaive_Initialized (_, AffineMeanGaussian _) -> KGaussian
  | DSnaive_Marginalized (Dist_gaussian _, _) -> KGaussian
  | DSnaive_Initialized (_, AffineMeanGaussianMV (_, _, _)) -> KMVGaussian
  | DSnaive_Marginalized (Dist_mv_gaussian _, _) -> KMVGaussian
  | DSnaive_Initialized (_, CBernoulli) -> KBernoulli
  | DSnaive_Initialized (_, CBernBern _) -> KBernoulli
  | DSnaive_Marginalized (Dist_bernoulli _, _) -> KBernoulli
  | DSnaive_Marginalized (Dist_beta _, _) -> KBeta
  | DSnaive_Marginalized (Dist_binomial _, _) -> KOthers
  | DSnaive_Marginalized (Dist_beta_binomial _, _) -> KOthers
  | DSnaive_Marginalized (( Dist_sampler _
                          | Dist_support _), _) -> KOthers
  | DSnaive_Marginalized (Dist_sampler_float _, _) -> KOthers
  | DSnaive_Marginalized (Dist_mixture _, _) -> KOthers
  | DSnaive_Marginalized (Dist_pair _, _) -> KOthers
  | DSnaive_Marginalized (Dist_list _, _) -> KOthers
  | DSnaive_Marginalized (Dist_array _, _) -> KOthers
  | DSnaive_Marginalized (Dist_uniform_int _, _) -> KOthers
  | DSnaive_Marginalized (Dist_uniform_float _, _) -> KOthers
  | DSnaive_Marginalized (Dist_exponential _, _) -> KOthers
  | DSnaive_Marginalized (Dist_poisson _, _) -> KOthers
  | DSnaive_Marginalized (Dist_lognormal _, _) -> KOthers
  | DSnaive_Marginalized (Dist_add _, _) -> KOthers
  | DSnaive_Marginalized (Dist_mult _, _) -> KOthers
  | DSnaive_Marginalized (Dist_app _, _) -> KOthers
  | DSnaive_Marginalized (Dist_joint _, _) -> KOthers (* XXX TODO? XXX *)
  | DSnaive_Realized _ -> assert false
  end


let shape : type a. ((a, Mat.mat) ds_naive_node) -> int =
  fun r ->
  begin match r.ds_naive_node_state with
  | DSnaive_Initialized (_, AffineMeanGaussianMV (_, b, _)) ->
      let rows, _ = Mat.shape b in rows
  | DSnaive_Marginalized (Dist_mv_gaussian(mu, _, _), _) ->
      let rows, _ = Mat.shape mu in rows
  | DSnaive_Realized v ->
      let rows, _ = Mat.shape v in rows
  | DSnaive_Initialized (_, _) -> assert false
  | DSnaive_Marginalized (_, _) -> assert false
  end

let is_realized : type p a. (p, a) ds_naive_node -> bool =
  fun r ->
  begin match r.ds_naive_node_state with
  | DSnaive_Initialized _ -> false
  | DSnaive_Marginalized _ -> false
  | DSnaive_Realized _ -> true
  end

let rec copy_node : type p a.
  (int, Obj.t) Hashtbl.t -> (p, a) ds_naive_node ->  (p, a) ds_graph_node =
  fun tbl n ->
  begin match Hashtbl.find_opt tbl n.ds_naive_node_id with
  | None ->
      let state =
        begin match n.ds_naive_node_state with
        | DSnaive_Realized x -> DSgraph_Realized x
        | DSnaive_Marginalized(mdistr, None) ->
            DSgraph_Marginalized(mdistr, None)
        | DSnaive_Marginalized(mdistr, Some _) ->
            begin match marginal_child n with
            | None ->
                DSgraph_Marginalized(mdistr, None)
            | Some(DSnaive_Child(c)) ->
                begin match c.ds_naive_node_state with
                | DSnaive_Marginalized(_, Some(p, cdistr)) ->
                    assert (p.ds_naive_node_id = n.ds_naive_node_id);
                    let c =
                      copy_node tbl (Obj.magic c: (a, _) ds_naive_node)
                    in
                    DSgraph_Marginalized(mdistr, Some(c, cdistr))
                | DSnaive_Marginalized(_, None)
                | DSnaive_Realized _
                | DSnaive_Initialized _ -> assert false
                end
            end
        | DSnaive_Initialized(p, cdistr) ->
            let p = copy_node tbl p in
            DSgraph_Initialized(p, cdistr)
        end
      in
      let n =
        { ds_graph_node_id = n.ds_naive_node_id;
          ds_graph_node_state = state }
      in
      Hashtbl.add tbl n.ds_graph_node_id (Obj.repr n);
      n
  | Some o -> (Obj.obj o: (p, a) ds_graph_node)
  end
