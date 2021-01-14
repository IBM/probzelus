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

type instrumentation = Obj.t Weak.t list ref

type pstate = { 
  ds_state : Ds_streaming_low_level.pstate; 
  nodes : instrumentation
}

let mk_pstate pf_state ins_state = {
  ds_state = pf_state;
  nodes = ins_state
}

let print_ins pstate =
  let nodes_alive = 
    List.fold_left (fun s w ->
      if Weak.check w 0 then
        s + 1
      else
        s
    ) 0 !(pstate.nodes)
  in
  print_string ("Nodes alive: " ^ (string_of_int nodes_alive) ^ "\n")

let empty_ins _ = ref []
let copy_ins src dst = dst := !src

let get_distr_kind = Ds_streaming_low_level.get_distr_kind
let get_distr = Ds_streaming_low_level.get_distr

let factor' (pstate, f0) = Infer_pf.factor' (pstate.ds_state, f0)

let value = Ds_streaming_low_level.value

let assume_constant : type a p.
  pstate -> a Types.mdistr -> (p, a) Types.ds_graph_node =
  fun ps d ->
    let ret = Ds_streaming_low_level.assume_constant d in
    let w = Weak.create 1 in
    Weak.set w 0 (Some (Obj.repr ret));
    Gc.finalise (fun _ -> ()) ret; (* Add a finaliser for the node to ensure
                                      the node was heap-allocated. 
                                      Otherwise, the value will be copied into 
                                      the weak array and the weak pointer will 
                                      not properly indicate memory usage. *)
    ps.nodes := w :: !(ps.nodes);
    ret

let assume_conditional : type a b c.
  pstate -> (a, b) Types.ds_graph_node -> (b, c) Types.cdistr -> (b, c) Types.ds_graph_node =
  fun ps p cdistr ->
    let ret = Ds_streaming_low_level.assume_conditional p cdistr in
    let w = Weak.create 1 in
    Weak.set w 0 (Some (Obj.repr ret));
    Gc.finalise (fun _ -> ()) ret; (* Add a finaliser for the node to ensure
                                      the node was heap-allocated. 
                                      Otherwise, the value will be copied into 
                                      the weak array and the weak pointer will 
                                      not properly indicate memory usage. *)
    ps.nodes := w :: !(ps.nodes);
    ret

let observe_conditional : type a b c.
  pstate -> (a, b) Types.ds_graph_node -> (b, c) Types.cdistr -> c -> unit =
  fun prob p cdistr obs ->
    Ds_streaming_low_level.observe_conditional prob.ds_state p cdistr obs

