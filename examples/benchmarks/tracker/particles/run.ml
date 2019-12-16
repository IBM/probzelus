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
    type input = unit
    type output = unit
    let iters = ref 0
    let read_input () =
        begin
            if !iters >= 1600 then
                raise End_of_file
            else
                iters := !iters + 1
        end;
        ()
    let main = Tracker_particles.main
end

module H = Harness.Make(M)

let () =
    H.run ()
