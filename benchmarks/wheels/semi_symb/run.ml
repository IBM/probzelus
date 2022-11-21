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
  let name = "Wheels"
  let algo = "Semi-Symbolic"
  type input = (float * float) * (float * float)
  type output = (float * float) Probzelus.Distribution.t
  let read_input () = Scanf.scanf ("%f, %f, %f, %f\n") (fun v o l r  -> ((v, o), (l, r)))
  let main = Wheels_semi_symb.main
  let string_of_output out = 
    let l_d, r_d = Probzelus.Distribution.split out in
    Format.sprintf "%f, %f\n" (Probzelus.Distribution.mean_float l_d) (Probzelus.Distribution.mean_float r_d)
end

module H = Harness.Make(M)

let () =
  H.run ()
