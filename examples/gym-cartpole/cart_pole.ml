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
open Gymnasium

(* env *)
type env = Pytypes.pyobject

(* Observation given by gym *)
type observation = {
  cart_position : float;
  cart_velocity : float;
  pole_angle : float;
  pole_velocity : float;
}

(* Action we can perform *)
type action = Left | Right
type render = Human | Human_multi

let cast_render (render : render) : Utils.render =
  match render with Human -> Utils.Human | Human_multi -> Utils.Human_multi

(* Translation of observation (from gym to OCaml)*)
let to_observation obs =
  let cp, cv, pa, pv = Py.Tuple.to_tuple4 obs in
  {
    cart_position = Py.Float.to_float cp;
    cart_velocity = Py.Float.to_float cv;
    pole_angle = Py.Float.to_float pa;
    pole_velocity = Py.Float.to_float pv;
  }

(* Translation of action (from OCaml to gym) *)
let from_action = function Left -> Py.Int.of_int 0 | Right -> Py.Int.of_int 1

(* Function to interact with gym *)
let cart_make render =
  let render = cast_render render in
  Utils.make "CartPole-v1" ~render

let cart_reset env =
  let obs, _info = Py.Tuple.to_pair (Utils.reset env) in
  to_observation obs

let cart_step env cart_action =
  let obs, reward, terminated, truncated, _info =
    Utils.step env (from_action cart_action)
  in
  ( to_observation obs,
    Py.Float.to_float reward,
    terminated,
    truncated (*TODO: maybe not discard _info*) )

let cart_close = Utils.close
let sample_action = Utils.sample_action
