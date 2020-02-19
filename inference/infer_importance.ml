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
open Infer_pf
open Inference_types

type pstate = Infer_pf.pstate

let sample = Infer_pf.sample
let factor = Infer_pf.factor
let observe = Infer_pf.observe


let infer_decay n decay (Cnode { alloc; reset; copy; step }) =
  let alloc () =
    { infer_states = Array.init n (fun _ -> alloc ());
      infer_scores = Array.make n 0.0; }
  in
  let reset state =
    Array.iter reset state.infer_states;
    Array.fill state.infer_scores 0 n 0.0
  in
  let step { infer_states = states; infer_scores = scores } input =
    let values =
      Array.mapi
        (fun i state ->
           let value = step state ({ idx = i; scores = scores; }, input) in
           value)
        states
    in
    let weights, norm =
      let sum = ref 0. in
      let acc = ref [] in
      Array.iteri
        (fun i score ->
           let w = max (exp score) epsilon_float in
           acc := (values.(i), w) :: !acc;
           sum := !sum +. w)
        scores;
      (!acc, !sum)
    in
    if decay <> 1. then
      Array.iteri (fun i score -> scores.(i) <- decay *. score) scores;
    Dist_support
      (List.rev_map (fun (b, w) -> (b, w /. norm)) weights)
  in
  let copy src dst =
    for i = 0 to n - 1 do
      copy src.infer_states.(i) dst.infer_states.(i);
      dst.infer_scores.(i) <- src.infer_scores.(i)
    done
  in
  Cnode { alloc = alloc; reset = reset; copy = copy; step = step }


let infer n node =
  infer_decay n 1. node

