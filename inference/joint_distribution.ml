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
open Infer_ds_ll_naive

let rec copy_node : type a b.
  (int, Obj.t) Hashtbl.t -> (a, b) ds_graph_node -> (a, b) ds_graph_node =
  fun tbl n  ->
  begin match Hashtbl.find_opt tbl n.ds_graph_node_id with
  | None ->
      let state =
        begin match n.ds_graph_node_state with
        | DSgraph_Realized x -> DSgraph_Realized x
        | DSgraph_Marginalized (mdistr, None) -> DSgraph_Marginalized (mdistr, None)
        | DSgraph_Marginalized (mdistr, Some (c, cdistr)) ->
            assert begin match c.ds_graph_node_state with
              | DSgraph_Initialized _ -> false
              | DSgraph_Marginalized _ | DSgraph_Realized _ -> true
            end;
            let c_copy = copy_node tbl c in
            DSgraph_Marginalized (mdistr, Some (c_copy, cdistr))
        | DSgraph_Initialized (p, cdistr) ->
            assert begin match p.ds_graph_node_state with
              | DSgraph_Marginalized (_, Some (c, _)) ->
                  c.ds_graph_node_id <> n.ds_graph_node_id
              | DSgraph_Initialized _ | DSgraph_Marginalized (_, None) | DSgraph_Realized _ ->
                  true
            end;
            let p_copy = copy_node tbl p in
            DSgraph_Initialized (p_copy, cdistr)
        end
      in
      let n =
        { ds_graph_node_id = n.ds_graph_node_id;
          ds_graph_node_state = state }
      in
      Hashtbl.add tbl n.ds_graph_node_id (Obj.repr n);
      n
  | Some o -> (Obj.obj o: (a, b) ds_graph_node)
  end

let rec copy_cast_node : type p a.
  (int, Obj.t) Hashtbl.t -> (p, a) ds_naive_node ->  (p, a) ds_graph_node =
  fun tbl n ->
  begin match Hashtbl.find_opt tbl n.ds_naive_node_id with
  | None ->
      let state =
        begin match n.ds_naive_node_state with
        | DSnaive_Realized x -> DSgraph_Realized x
        | DSnaive_Marginalized(mdistr, None) -> DSgraph_Marginalized(mdistr, None)
        | DSnaive_Marginalized(mdistr, Some _) ->
            begin match marginal_child n with
            | None ->
                DSgraph_Marginalized(mdistr, None)
            | Some(DSnaive_Child(c)) ->
                begin match c.ds_naive_node_state with
                | DSnaive_Marginalized(_, Some(p, cdistr)) ->
                    assert (p.ds_naive_node_id = n.ds_naive_node_id);
                    let c =
                      copy_cast_node tbl (Obj.magic c: (a, _) ds_naive_node)
                    in
                    DSgraph_Marginalized(mdistr, Some(c, cdistr))
                | DSnaive_Marginalized(_, None)
                | DSnaive_Realized _
                | DSnaive_Initialized _ -> assert false
                end
            end
        | DSnaive_Initialized(p, cdistr) ->
            let p = copy_cast_node tbl p in
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
