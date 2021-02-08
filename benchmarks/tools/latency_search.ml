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

open Benchlib

module Config = struct

  let file = ref None
  let perf_target = ref 0.1
  let pgf_format = ref false

  let args =
    Arg.align [
      ("-target", Float (fun f -> perf_target := f),
       "n Target value");
    ]

  let () =
    Arg.parse args (fun f -> file := Some f) "search target latency"

end

let search target stats =
  let n_res = ref 0 in
  List.iter
    (fun (n, (_, obs, _)) -> if obs <= target +. epsilon_float then n_res := n)
    (Array.to_list stats);
  !n_res

let main =
  let file =
    begin match !Config.file with
    | Some f -> f
    | None -> assert false
    end
  in
  let stats = Stats.read_stats file in
  let n = search !Config.perf_target stats in
  Format.printf "%s (target %f): number of particles = %d@."
    file !Config.perf_target n
