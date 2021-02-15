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

let name = "Kalman-1D"
let algo = "SDS"
type input = float * float
type output = float Probzelus.Distribution.t * float
let read_input () = Scanf.scanf ("%f, %f\n") (fun t o -> (t, o))
let main = Kalman_copy.main
let string_of_output (out, _) = string_of_float (Probzelus.Distribution.mean_float out)

let num_particles = 5


let rec run_helper step state =
  try
    let s = read_input () in
    let out = step state s in
    print_string ((string_of_output out) ^ "\n");
    run_helper step state
  with End_of_file -> []

let run _ =
  let Ztypes.Cnode {alloc; reset; step; copy = _} = main num_particles in
  let init_state = alloc () in
  reset init_state;
  run_helper step init_state;;

run ()
