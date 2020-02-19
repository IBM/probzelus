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
open Ds_distribution
open Inference_types

(** Inference with streaming delayed sampling *)

type pstate = Infer_pf.pstate

(** {2 Graph manipulations} *)

let fresh_id =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    !cpt

(* initialize without parent node *)
let assume_constant : type a p.
  a mdistr -> (p, a) ds_graph_node =
  fun d ->
  { ds_graph_node_id = fresh_id ();
    ds_graph_node_state = DSgraph_Marginalized (d, None); }

(* initialize with parent node *)
let assume_conditional : type a b c.
  (a, b) ds_graph_node -> (b, c) cdistr -> (b, c) ds_graph_node =
  fun p cdistr ->
  let child =
    { ds_graph_node_id = fresh_id ();
      ds_graph_node_state = DSgraph_Initialized (p, cdistr); }
  in
  child

let marginalize : type a b.
  (a, b) ds_graph_node -> unit =
  fun n ->
  begin match n.ds_graph_node_state with
  | DSgraph_Initialized (p, cdistr) ->
      begin match p.ds_graph_node_state with
      | DSgraph_Realized x ->
          let mdistr = cdistr_to_mdistr cdistr x in
          n.ds_graph_node_state <- DSgraph_Marginalized(mdistr, None)
      | DSgraph_Marginalized (p_mdistr, None) ->
          p.ds_graph_node_state <- DSgraph_Marginalized (p_mdistr, Some(n, cdistr));
          let mdistr = make_marginal p_mdistr cdistr in
          n.ds_graph_node_state <- DSgraph_Marginalized(mdistr, None)
      | DSgraph_Initialized _ | DSgraph_Marginalized (_, Some _) -> assert false
      end
  | DSgraph_Realized _ | DSgraph_Marginalized _ ->
      Format.eprintf "Error: marginalize@.";
      assert false
  end

let realize : type a b.
  b -> (a, b) ds_graph_node -> unit =
  fun obs n ->
  assert begin match n.ds_graph_node_state with
    | DSgraph_Marginalized (_mdistr, None) -> true
    | DSgraph_Initialized _ | DSgraph_Realized _ | DSgraph_Marginalized (_, Some _) -> false
  end;
  n.ds_graph_node_state <- DSgraph_Realized obs


let force_condition : type a b.
  (a, b) ds_graph_node -> unit =
  fun n ->
  begin match n.ds_graph_node_state with
  | DSgraph_Marginalized (mdistr, Some(child, cdistr)) ->
      begin match child.ds_graph_node_state with
      | DSgraph_Realized x ->
          let mdistr = make_conditional mdistr cdistr x in
          n.ds_graph_node_state <- DSgraph_Marginalized(mdistr, None)
      | DSgraph_Initialized _ | DSgraph_Marginalized _ -> ()
      end
  | DSgraph_Initialized _ | DSgraph_Realized _ | DSgraph_Marginalized (_, None) -> ()
  end

let sample : type a b.
  (a, b) ds_graph_node -> unit =
  fun n ->
  force_condition n;
  begin match n.ds_graph_node_state with
  | DSgraph_Marginalized (m, None) ->
      let x = Distribution.draw m in
      realize x n
  | DSgraph_Realized _ -> ()
  | DSgraph_Initialized _  | DSgraph_Marginalized (_, Some _) -> assert false
  end

let factor' = Infer_pf.factor'
let factor = Infer_pf.factor

let observe : type a b.
  pstate -> b -> (a, b) ds_graph_node -> unit =
  fun prob x n ->
  force_condition n;
  begin match n.ds_graph_node_state with
  | DSgraph_Marginalized (mdistr, None) ->
      factor' (prob, Distribution.score(mdistr, x));
      realize x n
  | DSgraph_Initialized _ | DSgraph_Realized _ | DSgraph_Marginalized (_, Some _) -> assert false
  end

let rec prune : type a b.
  (a, b) ds_graph_node -> unit =
  function n ->
    begin match n.ds_graph_node_state with
    | DSgraph_Marginalized(_, Some(c, _)) -> prune c
    | DSgraph_Initialized _ | DSgraph_Realized _ | DSgraph_Marginalized (_, None) -> ()
    end;
    sample n

let rec graft : type a b.
  (a, b) ds_graph_node -> unit =
  function n ->
    begin match n.ds_graph_node_state with
    | DSgraph_Marginalized (_, None) | DSgraph_Realized _  -> ()
    | DSgraph_Marginalized (_, Some(c, _)) -> prune c
    | DSgraph_Initialized (p, _cdistr) ->
        graft p;
        force_condition p;
        marginalize n
    end

let rec value: type a b.
  (a, b) ds_graph_node -> b =
  fun n ->
  begin match n.ds_graph_node_state with
  | DSgraph_Realized x -> x
  | DSgraph_Marginalized _ | DSgraph_Initialized _ ->
      graft n;
      sample n;
      value n
  end

let rec get_mdistr : type a b.
  (a, b) ds_graph_node -> b mdistr =
  function n ->
    force_condition n;
    begin match n.ds_graph_node_state with
    | DSgraph_Marginalized (m, _) -> m
    | DSgraph_Initialized (p, cdistr) ->
        let p_mdistr = get_mdistr p in
        make_marginal p_mdistr cdistr
    | DSgraph_Realized _ -> assert false
    end

let get_distr : type a b.
  (a, b) ds_graph_node -> b Distribution.t =
  fun n ->
  begin match n.ds_graph_node_state with
  | DSgraph_Realized x -> Dist_support [ (x, 1.) ]
  | DSgraph_Initialized _ | DSgraph_Marginalized _ -> get_mdistr n
  end

let observe_conditional : type a b c.
  pstate -> (a, b) ds_graph_node -> (b, c) cdistr -> c -> unit =
  fun prob p cdistr obs ->
  let n = assume_conditional p cdistr in
  graft n;
  observe prob obs n

let get_distr_kind : type a b.
  (a, b) ds_graph_node -> kdistr =
  fun n  ->
  begin match n.ds_graph_node_state with
  | DSgraph_Initialized (_, AffineMeanGaussian _) -> KGaussian
  | DSgraph_Marginalized (Dist_gaussian _, _) -> KGaussian
  | DSgraph_Initialized (_, AffineMeanGaussianMV (_, _, _)) -> KMVGaussian
  | DSgraph_Marginalized (Dist_mv_gaussian (_, _), _) -> KMVGaussian
  | DSgraph_Initialized (_, CBernoulli) -> KBernoulli
  | DSgraph_Initialized (_, CBernBern _) -> KBernoulli
  | DSgraph_Marginalized (Dist_bernoulli _, _) -> KBernoulli
  | DSgraph_Marginalized (Dist_beta _, _) -> KBeta
  | DSgraph_Marginalized (( Dist_sampler _
                  | Dist_support _), _) -> KOthers
  | DSgraph_Marginalized (Dist_sampler_float _, _) -> KOthers
  | DSgraph_Marginalized (Dist_mixture _, _) -> KOthers
  | DSgraph_Marginalized (Dist_pair _, _) -> KOthers
  | DSgraph_Marginalized (Dist_list _, _) -> KOthers
  | DSgraph_Marginalized (Dist_array _, _) -> KOthers
  | DSgraph_Marginalized (Dist_uniform_int _, _) -> KOthers
  | DSgraph_Marginalized (Dist_uniform_float _, _) -> KOthers
  | DSgraph_Marginalized (Dist_exponential _, _) -> KOthers
  | DSgraph_Marginalized (Dist_poisson _, _) -> KOthers
  | DSgraph_Marginalized (Dist_add _, _) -> KOthers
  | DSgraph_Marginalized (Dist_mult _, _) -> KOthers
  | DSgraph_Marginalized (Dist_app _, _) -> KOthers
  | DSgraph_Realized _ -> assert false
  end

let shape : type a. ((a, Mat.mat) ds_graph_node) -> int =
  fun r ->
  begin match r.ds_graph_node_state with
  | DSgraph_Initialized (_, AffineMeanGaussianMV (_, b, _)) ->
      let rows, _ = Mat.shape b in rows
  | DSgraph_Marginalized (Dist_mv_gaussian(mu, _), _) ->
      let rows, _ = Mat.shape mu in rows
  | DSgraph_Realized v ->
      let rows, _ = Mat.shape v in rows
  | DSgraph_Initialized (_, _) -> assert false
  | DSgraph_Marginalized (_, _) -> assert false
  end


let is_realized : type p a. (p, a) ds_graph_node -> bool =
  fun r ->
  begin match r.ds_graph_node_state with
  | DSgraph_Initialized _ -> false
  | DSgraph_Marginalized _ -> false
  | DSgraph_Realized _ -> true
  end
