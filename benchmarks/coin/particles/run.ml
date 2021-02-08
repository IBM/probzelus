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

module M = struct
  let name = "Beta-Bernoulli"
  let algo = "PF"
  type input = float * bool
  type output = float Probzelus.Distribution.t
  let read_input () = Scanf.scanf ("%f, %B\n") (fun t o -> (t, o))
  let main = Coin_particles.main
  let string_of_output out =
    string_of_float (Probzelus.Distribution.mean_float out)
end

module H = Harness.Make(M)

let () =
  H.run ()
