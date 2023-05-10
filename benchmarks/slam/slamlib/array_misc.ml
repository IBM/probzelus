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

open Ztypes
open Probzelus


let sensor_noise = 0.1
let wheel_noise = 0.1

let max_pos = 10

type 'a t = 'a array

let get a i =
  a.(i)

(* let set a i v = *)
(*   a.(i) <- v *)

let set a i v =
  let a = Array.copy a in
  a.(i) <- v;
  a

let of_list = Array.of_list

let make = Array.make

let ini n (Cnode f)  =
  let alloc () = f.alloc () in
  let reset state = f.reset state in
  let copy src dst = f.copy src dst in
  let step state (proba, arg) =
    Array.init n (fun i -> f.step state (proba, i))
  in
  Cnode { alloc; reset; copy; step; }


open Distribution

let print to_string a =
  Format.printf "[ @[";
  Array.iter
    (fun x -> Format.printf "%s;@ " (to_string x))
    a;
  Format.printf "@]]@."

let print_map_dist a =
  print
    (fun d ->
       let d_true, d_false = Distribution.split d in
       "("^(string_of_float (mean_float d_true))^", "
       ^(string_of_float (mean_float d_false))^")")
    (Distribution.split_array a)


let random n theta =
  Array.init n
    (fun _ -> Distribution.draw (Distribution.bernoulli theta))

let float_of_bool b =
  if b then 1. else 0.

let color_diff expected actual =
  let b = actual > 0.5 in
  if expected = b then 0. else 1.

let error (map, x) map_d d_x =
  let len = Array.length map in
  let e = ref ((float x -. Distribution.mean_int d_x) ** 2.) in
  for i = 0 to len - 1 do
    e := !e +. (float_of_bool map.(i) -. mean_bool map_d.(i)) ** 2.
  done;
  !e

let print_bool b =
  print_string (if b then "true" else "false")

let print_map map =
  Array.iter (fun b -> print_bool b;  print_string ", ") map


let print_map_d d_map =
  let m = Array.map (fun d -> mean_float d > 0.5) d_map in
  print_map m


let time n = 
  let Cnode { alloc = n_alloc; reset = n_reset; step = n_step; copy = n_copy } = n in

  Cnode {
    alloc = (n_alloc);
    reset = n_reset;
    step = (fun x a ->
      let prev_time = Mtime_clock.now () in
      let ret = n_step x a in
      let new_time = Mtime_clock.now () in
      (* Broken with MTime 2.0.0 *)
      (* let elapsed_ms = Mtime.Span.to_ms (Mtime.span prev_time new_time) in *)
      let elapsed_ms = (Mtime.Span.to_float_ns (Mtime.span prev_time new_time)) *. 1e-6 in
      (ret, elapsed_ms)
    );
    copy = n_copy
  }
