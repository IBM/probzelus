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

open Types

(** Inference with streaming delayed sampling *)

type pstate = Infer_pf.pstate

include Distribution.DS_graph

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

let observe_conditional : type a b c.
  pstate -> (a, b) ds_graph_node -> (b, c) cdistr -> c -> unit =
  fun prob p cdistr obs ->
  let n = assume_conditional p cdistr in
  graft n;
  observe prob obs n
