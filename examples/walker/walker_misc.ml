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
open Infer_pf
open Distribution
open Types

let make_node f =
  let alloc () = () in
  let reset state = () in
  let copy src dst = () in
  let step state input = f input in
  Ztypes.Cnode { alloc; reset; copy; step; }

let pi = 3.14

type motion_type = Stationary | Walking | Running

type walker = {
  position : float * float;
  velocity : float * float;
  motion_type : motion_type;
}

let pos_noise = 0.01

let coast' (prob, (dt, w)) =
  let std_dev = sqrt dt *. pos_noise in
  let { position = (x, y); velocity = (vx, vy) } = w in
  let (x', y') = (x +. dt *. vx, y +. dt *. vy)  in
  let x'' = sample' (prob, gaussian (x', std_dev)) in
  let y'' = sample' (prob, gaussian (y', std_dev)) in
  { w with position = (x'', y'') }

let coast : (Infer_pf.pstate * (float * walker), walker) Ztypes.cnode =
  make_node coast'


(* half lives of changing the motion type, in seconds *)
(* sometimes, we change to ourselves, to change direction *)
let motion_type_transition mt =
  begin match mt with
  | Stationary -> [(30., Walking); (60. *. 5., Running)]
  | Walking -> [(1., Walking); (10., Stationary); (60., Running)]
  | Running -> [(0.5, Running); (2., Walking)]
  end

let init_velocity' (prob, mt) =
  let speed_at_random_direction speed =
    let angle = sample' (prob, uniform_float (0., 2. *. pi)) in
    (speed *. cos angle, speed *. sin angle)
  in
  begin match mt with
  | Stationary -> (0., 0.)
  | Walking ->
      let speed = sample' (prob, uniform_float (0., 2.)) in
      speed_at_random_direction speed
  | Running ->
      let speed = sample' (prob, uniform_float (2., 7.)) in
      speed_at_random_direction speed
  end

let init_velocity: (Infer_pf.pstate * motion_type, float * float) Ztypes.cnode =
  make_node init_velocity'

(* default units are seconds *)
(* motion :: Double -> Walker -> RVar Walker *)
let rec motion' (prob, (dt, w)) =
  let trans_lam =
    List.map
      (fun (t, mt) -> log 2. /. t, mt)
      (motion_type_transition w.motion_type)
  in
  let st =
    List.fold_left (fun acc (t, mt) -> acc +. t) 0. trans_lam
  in
  let t_transition = sample' (prob, exponential st) in
  if t_transition > dt then coast' (prob, (dt, w)), t_transition
  else
    let w' = coast' (prob, (t_transition, w)) in
    let mt = sample' (prob, weighted_list trans_lam) in
    let vel = init_velocity' (prob, mt) in
    motion' (prob,
             (dt -. t_transition, { w' with velocity = vel; motion_type = mt }))

let motion : (pstate * (float * walker), walker * float) Ztypes.cnode =
  make_node motion'

let real_motion (dt, w) =
  let w, _ = motion' ((), (dt, w)) in 
  w

let position_std_dev = 10.
let time_std_dev = 1.0

(* walkerMeasure :: Walker -> (Double, Double) -> PProg a () *)
let walker_measure' ((prob: pstate), (w, dtw, dt, (mx, my))) =
  let (x, y) = w.position in
  factor' (prob, score (gaussian (x, position_std_dev), mx));
  factor' (prob, score (gaussian (y, position_std_dev), my));
  factor' (prob, score (gaussian (dtw, time_std_dev), dt))

let walker_measure = make_node walker_measure'

(* walkerGenMeasurement :: Walker -> RVar (Double, Double) *)
let walker_gen_measurement w =
  let (x, y) = w.position in
  let mx = draw (gaussian (x, position_std_dev)) in
  let my = draw (gaussian (y, position_std_dev)) in
  (mx, my)

(* walkerStep :: Double -> (Double, Double) -> Walker -> PProg a Walker *)
let walker_step' ((prob: pstate), (dt, measured_position, w)) =
  let w', dtw = motion' (prob, (dt, w)) in
  walker_measure' (prob, (w', dtw, dt, measured_position));
  w'

let walker_step =
  make_node walker_step'

(* walkerInit :: RVar Walker *)
let walker_init' (prob, ()) =
  let mt =
    sample' (prob,
             weighted_list [(0.7, Stationary); (0.25, Walking); (0.05, Running)])
  in
  let vel = init_velocity' (prob, mt) in
  { position = (0., 0.); velocity = vel; motion_type = mt }

let walker_init : (pstate * unit, walker) Ztypes.cnode =
  make_node walker_init'

let real_walker_init () =
  walker_init' ((), ())

let print_mt_dist mt_dist =
  begin match mt_dist with
  | Dist_support sup ->
      Format.printf "([";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
        (fun fmt (mt, p) ->
           begin match mt with
           | Stationary -> Format.fprintf fmt "(%f, Stationary)" p
           | Walking -> Format.fprintf fmt "(%f, Walking)" p
           | Running -> Format.fprintf fmt "(%f, Running)" p
           end)
        Format.std_formatter
        sup;
      Format.printf "])"
  | _ -> assert false
  end

let print_pos_dist pos_dist =
  let x_dist, y_dist = Distribution.split pos_dist in
  Format.printf " (%f, %f)" (mean_float x_dist) (mean_float y_dist)

let print dist =
  let mt_dist, pos_dist = Distribution.split dist in
  print_mt_dist mt_dist;
  print_pos_dist pos_dist;
  Format.printf "@."

