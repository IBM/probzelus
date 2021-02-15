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

type instrumentation = instrumentation_impl ref
and instrumentation_impl ={
  weaks : (Obj.t Weak.t * int) list;
  finaliser_count : int ref
}

type pstate = { 
  ds_state : Ds_streaming_low_level.pstate; 
  nodes : instrumentation
}

let mk_pstate pf_state ins_state = {
  ds_state = pf_state;
  nodes = ins_state
}

let print_ins_helper ins =
(*
  let nodes_alive_weak = 
    List.fold_left (fun s w ->
      if Weak.check w 0 then
        s + 1
      else
        s
    ) 0 (!ins).weaks
  in
*)
  let nodes_alive_weak = 
    List.fold_left (fun s (w, i) ->
      match Weak.get w 0 with
      | None -> s
      | Some _ -> s + 1 
    ) 0 (!ins).weaks
  in
  let nodes_alive_finaliser = !((!ins).finaliser_count) in
  print_string ("Nodes alive weak: " ^ (string_of_int nodes_alive_weak) ^ "; Nodes alive finaliser: " ^ (string_of_int nodes_alive_finaliser) ^ "\n")

let print_ins pstate = print_ins_helper pstate.nodes
  

let empty_ins _ = ref { weaks = []; finaliser_count = ref 0 }
let clear ins = ins := { weaks = []; finaliser_count = ref 0 }
let copy_ins src dst = dst := !src

let get_distr_kind = Ds_streaming_low_level.get_distr_kind
let get_distr = Ds_streaming_low_level.get_distr

let factor' (pstate, f0) = Infer_pf.factor' (pstate.ds_state, f0)

let value = Ds_streaming_low_level.value

let add_all_nodes : type a. instrumentation -> (a, Obj.t) Hashtbl.t -> unit =
  fun ins tbl ->
    (*(print_string "---ADDING ALL NODES---\n");
    (print_string ("Initial finaliser: " ^ (string_of_int (!((!ins).finaliser_count))) ^ "\n"));*)
    Hashtbl.iter (fun _ o ->
      let w = Weak.create 1 in
      Weak.set w 0 (Some o);
      ins := {
        weaks = (w, -1) :: (!ins).weaks;
        finaliser_count = (!ins).finaliser_count
      };
      let finaliser_count = (!ins).finaliser_count in
      finaliser_count := !finaliser_count + 1;
      Gc.finalise (fun _ ->
        (*(print_string "---add_all_nodes FINALISER---\n");*)
        finaliser_count := !finaliser_count - 1
      ) o
    ) tbl
    (*(print_string ("End finaliser: " ^ (string_of_int (!((!ins).finaliser_count))) ^ "\n"));
    (print_string "---END ADD ALL NODES---\n")*)

let assume_constant : type a p.
  pstate -> a Types.mdistr -> (p, a) Types.ds_graph_node =
  fun ps d ->
    let ret = Ds_streaming_low_level.assume_constant d in
    (*(print_string ("---ASSUME CONSTANT---: " ^ (string_of_int ret.ds_graph_node_id) ^ "\n"));*)
    let w = Weak.create 1 in
    Weak.set w 0 (Some (Obj.repr ret));
    (*Gc.finalise (fun _ -> ()) ret; (* Add a finaliser for the node to ensure
                                      the node was heap-allocated. 
                                      Otherwise, the value will be copied into 
                                      the weak array and the weak pointer will 
                                      not properly indicate memory usage. *)*)
    (*Gc.finalise (fun _ -> (print_string "Finalizer called!\n")) ret;*)

    let finaliser_count = (!(ps.nodes)).finaliser_count in
    finaliser_count := !finaliser_count + 1;
    (*let node_num = ret.ds_graph_node_id in*)

    Gc.finalise (fun _ ->
      (*(print_string ("---assume_constant FINALISER for node " ^ (string_of_int node_num) ^ "---\n"));*)
      finaliser_count := !finaliser_count - 1
    ) ret;
    
    ps.nodes := {
      weaks = (w, ret.ds_graph_node_id) :: (!(ps.nodes)).weaks;
      finaliser_count = !(ps.nodes).finaliser_count
    };

    ret

let assume_conditional : type a b c.
  pstate -> (a, b) Types.ds_graph_node -> (b, c) Types.cdistr -> (b, c) Types.ds_graph_node =
  fun ps p cdistr ->
    let ret = Ds_streaming_low_level.assume_conditional p cdistr in
    (*(print_string ("---ASSUME CONDITIONAL---: " ^ (string_of_int ret.ds_graph_node_id) ^ "\n"));*)
    let w = Weak.create 1 in
    Weak.set w 0 (Some (Obj.repr ret));
    (*Gc.finalise (fun _ -> ()) ret; (* Add a finaliser for the node to ensure
                                      the node was heap-allocated. 
                                      Otherwise, the value will be copied into 
                                      the weak array and the weak pointer will 
                                      not properly indicate memory usage. *)*)
    (*Gc.finalise (fun _ -> (print_string "Finalizer called!\n")) ret;*)

    let finaliser_count = (!(ps.nodes)).finaliser_count in
    finaliser_count := !finaliser_count + 1;
    (*let node_num = ret.ds_graph_node_id in*)

    Gc.finalise (fun _ ->
      (*(print_string ("---assume_conditional FINALISER for node " ^ (string_of_int node_num) ^ "---\n"));*)
      finaliser_count := !finaliser_count - 1
    ) ret;
    
    ps.nodes := {
      weaks = (w, ret.ds_graph_node_id) :: (!(ps.nodes)).weaks;
      finaliser_count = (!(ps.nodes)).finaliser_count
    };
    ret

(* TODO(eatkinson): use Ds_streaming_low_level.observe_with_graft once it is merged *)
let observe_with_graft : type a b.
  pstate -> b -> (a, b) Types.ds_graph_node -> unit =
  fun prob x n ->
  Ds_streaming_low_level.graft n;
  Ds_streaming_low_level.observe prob.ds_state x n

let observe_conditional : type a b c.
  pstate -> (a, b) Types.ds_graph_node -> (b, c) Types.cdistr -> c -> unit =
  fun prob p cdistr obs ->
    (*(print_string ("---OBSERVE CONDITIONAL---: " ^ (string_of_int p.ds_graph_node_id) ^ "\n"));*)
    Ds_streaming_low_level.observe_conditional prob.ds_state p cdistr obs

