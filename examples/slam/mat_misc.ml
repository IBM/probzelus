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

open Probzelus
open Zelus_owl
open Ztypes

let mu_prior = Mat.of_arrays [| [| 127. |];
                                [| 127. |];
                                [| 127. |] |]

let sigma_prior = Mat.of_arrays [| [| 150.; 0.; 0. |];
                                   [| 0.; 150.; 0. |];
                                   [| 0.; 0.; 150. |] |]

let sigma_obs = Mat.of_arrays [| [| 50.; 0.; 0. |];
                                 [| 0.; 50.; 0. |];
                                 [| 0.; 0.; 50. |] |]

let with_graphics, max_x, max_y =
  let with_graphics = ref true in
  let x = ref 16 in
  let y = ref 5 in
  Arg.parse
    [ "-nox", Arg.Clear with_graphics, "Disable the graphics interface";
      "-x", Arg.Set_int x, "Set the xsize of the map";
      "-y", Arg.Set_int y, "Set the ysize of the map"; ]
    (fun _ -> ()) "options:";
  !with_graphics, !x - 1, !y - 1

let real_map = 
  Array.init (max_x + 1)
    (fun _ -> Array.init (max_y + 1)
      (fun _ -> Distribution.draw (Distribution.mv_gaussian (mu_prior, sigma_prior))))

type 'a t = 'a array array

let get a i j =
  a.(i).(j)

let set a i j v =
  let a = Array.copy a in
  let ai = Array.copy a.(i) in
  ai.(j) <- v;
  a.(i) <- ai;
  a

let mean_bool = Distribution.mean (fun b ->
  if b then
      1.
  else
      0.
)

let of_list l = Array.of_list (List.map (Array.of_list) l)

let make = Array.make_matrix

let ini nx ny (Cnode f)  =
  let alloc () = f.alloc () in
  let reset state = f.reset state in
  let copy src dst = f.copy src dst in
  let step state (proba, arg) =
    Array.init nx
      (fun i ->
         Array.init ny
           (fun j -> f.step state (proba, (i, j))))
  in
  Cnode { alloc; reset; copy; step; }


open Distribution

let print to_string a =
  Format.printf "[ @[";
  Array.iter
    (fun ai ->
       Format.printf "[ @[";
       Array.iter
         (fun x -> Format.printf "%s;@ " (to_string x))
         ai;
       Format.printf "@]]@.")
    a;
  Format.printf "@]]@."

let print_map_dist a =
  print
    (fun d ->
       let d_true, d_false = Distribution.split d in
       "("^(string_of_float (mean_float d_true))^", "
       ^(string_of_float (mean_float d_false))^")")
    (Distribution.split_matrix a)

let init_graph max_x max_y =
  let size = " "^(string_of_int ((max_x + 1) * 50))^"x"^(string_of_int ((max_y + 1) * 50)) in
  Graphics.open_graph size;
  Graphics.set_window_title "SLAM";
  Graphics.auto_synchronize false;
  Format.printf "Press in the graphic window:@.";
  Format.printf "- 'q' to quit@.";
  Format.printf "- any other key for automatic control@."

let clear () =
  Graphics.synchronize ();
  Graphics.clear_graph ()

let wait_event () =
  let c = Graphics.read_key () in
  begin match c with
  | 'l' | 's' ->  -1
  | 'r' | 'f' ->  1
  | 'u' | 'e' -> 2
  | 'd' -> -2
  | 'm' -> 42
  | 'q' -> exit 0
  | _ -> 0
  end
(* ignore (Graphics.read_key ()) *)

let width = 50
let height = 50

let draw_bot x y obs =
  Graphics.set_color (Graphics.green);
  Graphics.fill_circle (x * width + width / 2) (y * height + height / 2) 15;
  if obs then Graphics.set_color (Graphics.white)
  else Graphics.set_color (Graphics.black);
  Graphics.fill_circle (x * width + width / 2) (y * height + height / 2) 13

let draw_bot_color x y obs =
  Graphics.set_color (Graphics.green);
  Graphics.fill_circle (x * width + width / 2) (y * height + height / 2) 15;
  let r = Mat.get obs 0 0 in
  let g = Mat.get obs 1 0 in
  let b = Mat.get obs 1 0 in
  let color_of_float i = max 0 (min 255 (int_of_float i)) in
  Graphics.set_color (Graphics.rgb (color_of_float r) (color_of_float g) (color_of_float b));
  Graphics.fill_circle (x * width + width / 2) (y * height + height / 2) 13

let draw_position_dist d =
  let d_x, d_y = Distribution.split d in
  for x = 0 to max_x do
    for y = 0 to max_y do
      let p =
        exp (Distribution.score (d_x, x)) *. exp (Distribution.score (d_y, y))
      in
      if p > 0. then begin
        Graphics.set_color (Graphics.red);
        Graphics.fill_circle
          (x * width + width / 2) (y * height + height / 2)
          (1 + int_of_float (10. *. p))
      end
    done
  done

let draw_map_dist map_dist =
  let mw =
    Array.map
      (fun ai ->
         Array.map
           (fun d ->
              let d_true, d_false = Distribution.split d in
              let n_t, n_f = mean_float d_true, mean_float d_false in
              n_t /. (n_t +. n_f))
           ai)
      (Distribution.split_matrix map_dist)
  in
  Array.iteri
    (fun i ai ->
       Array.iteri
         (fun j w ->
            let gray = int_of_float (w *. 255.) in
            Graphics.set_color (Graphics.rgb gray gray gray);
            Graphics.fill_rect (i * width)  (j * height)  width height)
         ai)
    mw

let draw_map_dist_ds map_dist =
  let mw = Array.map
      (fun ai ->
         Array.map
           (fun d -> Distribution.mean_bool d)
           ai)
      (Distribution.split_matrix map_dist)
  in
  Array.iteri
    (fun i ai ->
       Array.iteri
         (fun j w ->
            let gray = int_of_float (w *. 255.) in
            Graphics.set_color (Graphics.rgb gray gray gray);
            Graphics.fill_rect (i * width)  (j * height)  width height)
         ai)
    mw

let draw_map_dist_ds_color map_dist =
  let mw = Array.map
      (fun ai ->
         Array.map
           (fun d -> Distribution.mean_matrix d)
           ai)
      (Distribution.split_matrix map_dist)
  in
  Array.iteri
    (fun i ai ->
       Array.iteri
         (fun j w ->
            let r = Mat.get w 0 0 in
            let g = Mat.get w 1 0 in
            let b = Mat.get w 2 0 in
            let color_of_float i = max 0 (min 255 (int_of_float i)) in
            Graphics.set_color (Graphics.rgb (color_of_float r) (color_of_float g) (color_of_float b));
            Graphics.fill_rect (i * width)  (j * height)  width height)
         ai)
    mw

let draw_map m =
  Array.iteri
    (fun i ai ->
       Array.iteri
         (fun j b ->
            if b then Graphics.set_color (Graphics.white)
            else Graphics.set_color (Graphics.black);
            Graphics.fill_rect (i * width) (j * height) width height)
         ai)
    m;
  clear ()

let random nx ny theta =
  Array.init nx
    (fun _ -> Array.init ny
        (fun _ -> Distribution.draw (Distribution.bernoulli theta)))

let () =
  if with_graphics then init_graph max_x max_y

let input =
  if with_graphics then
    wait_event
  else
    (fun () -> 0)

let output =
  if with_graphics then
    (fun real_map real_x real_y obs map_dist pos_dist ->
       draw_map_dist map_dist;
       draw_map real_map;
       draw_bot real_x real_y obs;
       draw_position_dist pos_dist;
       clear ())
  else
    (fun real_map real_x real_y obs map_dist pos_dist ->
       print_map_dist map_dist)

let output_ds =
  if with_graphics then
    (fun real_map real_x real_y obs map_dist pos_dist ->
       draw_map_dist_ds map_dist;
       draw_bot real_x real_y obs;
       draw_position_dist pos_dist;
       clear ())
  else
    (fun _ -> assert false)

let output_ds_color =
  if with_graphics then
    (fun real_map real_x real_y obs map_dist pos_dist ->
       draw_map_dist_ds_color map_dist;
       draw_bot_color real_x real_y obs;
       draw_position_dist pos_dist;
       clear ())
  else
    (fun _ -> assert false)

let float_of_bool b =
  if b then 1. else 0.

let color_diff expected actual =
  let b = actual > 0.5 in
  if expected = b then 0. else 1.

let error (map, x, y) map_d d_x d_y =
  let len_x = Array.length map in
  let len_y = Array.length map.(0) in
  let e =
    ref ((float x -. Distribution.mean_int d_x) ** 2. +. (float y -. Distribution.mean_int d_y) ** 2.)
  in
  for i = 0 to len_x - 1 do
    for j = 0 to len_y -1 do
      e := !e +. (float_of_bool map.(i).(j) -. mean_bool map_d.(i).(j)) ** 2.
    done
  done;
  !e

let error_color (map, x, y) map_d d_x d_y =
  let len_x = Array.length map in
  let len_y = Array.length map.(0) in
  let e =
    ref ((float x -. Distribution.mean_int d_x) ** 2. +. (float y -. Distribution.mean_int d_y) ** 2.)
  in
  for i = 0 to len_x - 1 do
    for j = 0 to len_y -1 do
      let m = mean_matrix map_d.(i).(j)  in
      e := !e +.  (Mat.get (Mat.dot (Mat.transpose (map.(i).(j))) m) 0 0)
    done
  done;
  !e
