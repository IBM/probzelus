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
  let name = "Gaussian-Gaussian"
  let algo = "PF"
  type input = (float * float) * float
  type output = (float * float) Probzelus.Distribution.t
  let read_input () = Scanf.scanf ("%f, %f, %f\n") (fun mu sigma y -> ((mu, sigma), y))
  let main = Gaussian_particles.main
  let string_of_output out =
    let mu_d, sigma_d = Probzelus.Distribution.split out in
    Format.sprintf "%f, %f\n" (Probzelus.Distribution.mean_float mu_d) (Probzelus.Distribution.mean_float sigma_d)
end

module H = Harness.Make(M)

let () =
  H.run ()
