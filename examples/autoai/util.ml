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

let () = Random.self_init ()

let run_cmd cmd =
  let ch = Unix.open_process_in cmd in
  let n = float_of_string (input_line ch) in
  Format.printf "XXXXXXXXXXX %s accuracy = %f@." cmd n;
  close_in ch;
  n


let oiter f o =
  begin match o with
  | Some x -> f x
  | None -> ()
  end

open Ztypes

type state = { mutable ch : in_channel option }

(* let run_cmd_delay cmd = *)
(*   let alloc () = { ch = None } in *)
(*   let reset state = *)
(*     oiter close_in state.ch; *)
(*     state.ch <- None *)
(*   in *)
(*   let step state cmd = *)
(*     let res = *)
(*       begin match state.ch with *)
(*       | None -> 0. *)
(*       | Some ch -> *)
(*           let x = float_of_string (input_line ch) in *)
(*           close_in ch; *)
(*           x *)
(*       end *)
(*     in *)
(*     state.ch <- Some (Unix.open_process_in cmd); *)
(*     res *)
(*   in *)
(*   let copy src dst = *)

(*   in *)
(*   Cnode { alloc; reset; copy; step } *)

let run_cmd_half =
  let alloc () = { ch = None } in
  let reset state =
    oiter close_in state.ch;
    state.ch <- None
  in
  let step state cmd =
    begin match state.ch with
    | None ->
        Format.printf "XXXXXXXXXXX %s@." cmd;
        state.ch <- Some (Unix.open_process_in cmd);
        (4012., false)
    | Some ch ->
        let x = float_of_string (input_line ch) in
        Format.printf "XXXXXXXXXXX accuracy = %f (%s)@." x cmd;
        close_in ch;
        state.ch <- None;
        (x, true)
    end
  in
  let copy src dst =
    begin match src.ch, dst.ch with
    | None, None -> ()
    | _, _ -> failwith "Do not copy a node with a running command!"
    end
  in
  Cnode { alloc; reset; copy; step }

let open_process_in = Unix.open_process_in
