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
open Slamlib

module M = struct
  type input = bool array * unit
  type output = unit

  let read_input () =
    let a = Array.make (Array_misc.max_pos + 1) false in
    for i = 0 to Array_misc.max_pos do
      a.(i) <- Scanf.scanf "%B, " (fun x -> x)
    done;
    a, ()

  let main = Slam_ds_bounded.main
end

module H = Harness.Make(M)

let () =
  H.run ()
