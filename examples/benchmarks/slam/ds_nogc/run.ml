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
  type output = (int * (bool array * int) Probzelus.Distribution.t)

  let read_input () =
    let a = Array.make (Array_misc.max_pos + 1) false in
    for i = 0 to Array_misc.max_pos do
      a.(i) <- Scanf.scanf "%B, " (fun x -> x)
    done;
    a, ()

  let main = Slam_ds_nogc.main

  let string_of_output (x, xm_distr) =
    let m_d, x_d = Probzelus.Distribution.split xm_distr in
    let m_list = Array.to_list (Probzelus.Distribution.split_array m_d) in
    string_of_int x ^ ", [" ^ (String.concat ", " (List.map (fun i ->
      string_of_float (Probzelus.Distribution.mean_bool i)
    ) m_list)) ^ "], " ^ string_of_float (Probzelus.Distribution.mean_int x_d)
end

module H = Harness.Make(M)

let () =
  H.run ()
