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

(* A zelus discrete node is implemented by a value of the following type *)
(* see module Ztypes *)
(* type ('a, 'b) node =
    Cnode: { alloc : unit -> 's;   (* 's is the internal state *)
            reset: 's -> unit;    (* initializes the state *)
            step: 's -> 'a -> 'b; (* the step function *)
            copy: 's -> 's -> unit; (* copy state *)
          } -> ('a, 'b) node
*)

open Ztypes
open Owl

type pstate = {
  idx : int; (** particle index *)
  scores : float array; (** score of each particle *)
}

let factor' (pstate, f0) =
  pstate.scores.(pstate.idx) <- pstate.scores.(pstate.idx) +. f0

let factor =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    factor' input
  in
  Cnode { alloc; reset; copy; step; }

let observe' (pstate, (d, v)) =
  factor' (pstate, Distribution.score(d, v))

let observe =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    observe' input
  in
  Cnode { alloc; reset; copy; step; }

let sample' (_pstate, dist) =
  Distribution.draw dist

let sample =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    sample' input
  in
  Cnode { alloc; reset; copy; step; }

let id x = x

let const = id
let add (x, y) = x +. y
let ( +~ ) = ( +. )
let mult (x, y) = x *. y
let ( *~ ) = ( *. )
let app (f, x) = f x
let ( @@~ ) f x = f x
let pair = id
let array = id
let lst = id
let matrix = id

let mat_add (x, y) = Mat.add x y
let ( +@~ ) = Mat.( + )
let mat_scalar_mult (a, x) = Mat.mul_scalar x a
let ( $*~ ) = Mat.( $* )
let mat_dot (x, y) = Mat.dot x y
let ( *@~ ) = Mat.( *@ )
let vec_get (x, i) = Mat.get x i 0

let eval = id

type 'a infer_state = {
  infer_states : 'a array;
  infer_scores : float array;
}

(** [infer nb_particles f (b, i)]
    val infer :
         int -S-> (pstate * 'a -D-> 'b)
             -S-> bool * 'a -D-> 'b Distribution.t

    The infer function takes a number of particles, a node, a stream of
    booleans and inputs, and returns a node.  The node in argument takes
    as argument a state for the inference and an input and returns the
    output.  The node in output takes as argument a boolen indiacting to
    the instants to resample and the input and returns the infered
    output.
*)
let infer_subresample n (Cnode { alloc; reset; copy; step }) =
  let alloc () =
    { infer_states = Array.init n (fun _ -> alloc ());
      infer_scores = Array.make n 0.0; }
  in
  let reset state =
    Array.iter reset state.infer_states;
    Array.fill state.infer_scores 0 n 0.0
  in
  let step { infer_states = states; infer_scores = scores } (c, input) =
    let values =
      Array.mapi
        (fun i state ->
           let value = step state ({ idx = i; scores = scores; }, input) in
           value)
        states
    in
    let probabilities, ret = Normalize.normalize_nohist values scores in
    if c then begin
      Normalize.resample copy n probabilities states;
      Array.fill scores 0 n 0.0
    end;
    ret
  in
  let copy src dst =
    for i = 0 to n - 1 do
      copy src.infer_states.(i) dst.infer_states.(i);
      dst.infer_scores.(i) <- src.infer_scores.(i)
    done
  in
  Cnode { alloc = alloc; reset = reset; copy = copy; step = step }


(** [infer_ess_resample nb_particles threshold f i] inference with
    resampling when the effective sample size goes below [threshold].
*)
let infer_ess_resample n threshold (Cnode { alloc; reset; copy; step }) =
  let alloc () =
    { infer_states = Array.init n (fun _ -> alloc ());
      infer_scores = Array.make n 0.0; }
  in
  let reset state =
    Array.iter reset state.infer_states;
    Array.fill state.infer_scores 0 n 0.0
  in
  let do_resampling scores =
    let norm = Normalize.log_sum_exp scores in
    let scores' = Array.map (fun score -> (score -. norm)) scores in
    let num =
      (Array.fold_right (fun x acc -> exp x +. acc) scores' 0.) ** 2.
    in
    let den =
      Array.fold_right (fun x acc -> (exp x) ** 2. +. acc) scores' 0.
    in
    let ess = num /. den in
    ess < threshold *. (float_of_int n)
  in
  let step { infer_states = states; infer_scores = scores } (input) =
    let values =
      Array.mapi
        (fun i state ->
           let value = step state ({ idx = i; scores = scores; }, input) in
           value)
        states
    in
    let probabilities, ret = Normalize.normalize_nohist values scores in
    if do_resampling scores then begin
      Normalize.resample copy n probabilities states;
      Array.fill scores 0 n 0.0
    end;
    ret
  in
  let copy src dst =
    for i = 0 to n - 1 do
      copy src.infer_states.(i) dst.infer_states.(i);
      dst.infer_scores.(i) <- src.infer_scores.(i)
    done
  in
  Cnode { alloc = alloc; reset = reset; copy = copy; step = step }

let infer n node =
  let Cnode { alloc; reset; copy; step } = infer_subresample n node in
  Cnode { alloc;
          reset;
          copy;
          step = (fun state input -> step state (true, input)); }


let infer_noresample n node =
  let Cnode { alloc; reset; copy; step } = infer_subresample n node in
  Cnode { alloc;
          reset;
          copy;
          step = (fun state input -> step state (false, input)); }


(* [memoize_step f x] is functionally equivalent to [f x] but stores *)
(* all the pairs (state, input) and the associated result *)
let memoize_step step (s, table) x =
  try
    Hashtbl.find table (s, x)
  with
  | Not_found ->
      let sc = Probzelus_utils.copy s in
      let o = step s x in
      Hashtbl.add table (sc, x) o;
      o

let expectation scores =
  let s = Array.fold_left (+.) 0. scores in
  s /. float (Array.length scores)


let plan_step n k model_step model_copy =
  let table = Hashtbl.create 7 in
  let rec expected_utility (state, score) (ttl, input) =
    if ttl < 1 then score
    else
      let states = Array.init n (fun _ -> Probzelus_utils.copy state) in
      let scores = Array.make n 0.0 in
      let score' =
        Array.iteri
          (fun i state ->
             let _ = model_step state ({ idx = i; scores = scores; }, input) in
             let score = scores.(i) in
             let eu =
               memoize_step
                 expected_utility ((state, score), table) (ttl - 1, input)
             in
             scores.(i) <- eu)
          states;
        (* let norm = Normalize.log_sum_exp scores in *)
        (* let probabilities = Array.map (fun score -> exp (score -. norm)) scores in *)
        (* let scores' = Array.copy scores in *)
        (* Normalize.resample model_copy n probabilities states; *)
        (* Array.fill scores 0 n 0.0; *)
        (* expectation scores' *)
        expectation scores
      in
      score +. score'
  in
  let state_value_copy (src_st, src_val) (dst_st, dst_val) =
    model_copy src_st dst_st;
    dst_val := !src_val
  in
  let step { infer_states = states; infer_scores = scores; } input =
    let values =
      Array.mapi
        (fun i state ->
           let value = model_step state ({ idx = i; scores = scores; }, input) in
           let score = scores.(i) in
           scores.(i) <- expected_utility (state, score) (k, input);
           value)
        states
    in
    let states_values =
      Array.mapi (fun i state -> (state, ref values.(i))) states
    in
    let norm = Normalize.log_sum_exp scores in
    let probabilities = Array.map (fun score -> exp (score -. norm)) scores in
    Normalize.resample state_value_copy n probabilities states_values;
    Array.fill scores 0 n 0.0;
    Hashtbl.clear table;
    states_values
  in
  step

(* [plan n k f x] runs n instances of [f] on the input stream *)
(* [x] but at each step, do a prediction of depth k *)
let plan n k (Cnode model : (pstate * 't1, 't2) Ztypes.cnode) =
  let alloc () = ref (model.alloc ()) in
  let reset state = model.reset !state in
  let copy src dst = model.copy !src !dst in
  let step_body = plan_step n k model.step model.copy in
  let step plan_state input =
    let states = Array.init n (fun _ -> Probzelus_utils.copy !plan_state) in
    let scores = Array.make n 0.0 in
    let states_values =
      step_body { infer_states = states; infer_scores = scores; } input
    in
    let dist = Normalize.normalize states_values in
    let state', value = Distribution.draw dist in
    plan_state := state';
    !value
  in
  Cnode { alloc = alloc; reset = reset; copy = copy; step = step }


type 'state infd_state =
  { infd_states : 'state array;
    infd_scores : float array; }

let infer_depth n k (Cnode model) =
  let alloc () =
    { infd_states = Array.init n (fun _ -> model.alloc ());
      infd_scores = Array.make n 0.0; }
  in
  let reset state =
    Array.iter model.reset state.infd_states;
    Array.fill state.infd_scores 0 n 0.0
  in
  let copy src dst =
    for i = 0 to n - 1 do
      model.copy src.infd_states.(i) dst.infd_states.(i);
      dst.infd_scores.(i) <- src.infd_scores.(i)
    done
  in
  let step infd_state input =
    let states_values =
      plan_step n k
        model.step model.copy
        { infer_states = infd_state.infd_states;
          infer_scores = infd_state.infd_scores; } input
    in
    let values = Array.map (fun (_, v) -> !v) states_values in
    Normalize.normalize values
  in
  Cnode { alloc = alloc; reset = reset; copy = copy; step = step }


(** [gen f x] generates a value sampled from the model [f] with input [x] and
    its corresponding score. The score is reseted at each instant and does
    not tack into account the likelihood and samples.
*)
let gen (Cnode { alloc; reset; copy; step }) =
  let alloc () =
    { infer_states = [| alloc () |];
      infer_scores = [| 0.0 |]; }
  in
  let reset state =
    reset state.infer_states.(0);
    state.infer_scores.(0) <- 0.0
  in
  let step { infer_states = states; infer_scores = scores } input =
    let value = step states.(0) ({ idx = 0; scores = scores; }, input) in
    let score = scores.(0) in
    scores.(0) <- 0.;
    value, score
  in
  let copy src dst =
    copy src.infer_states.(0) dst.infer_states.(0);
    dst.infer_scores.(0) <- src.infer_scores.(0)
  in
  Cnode { alloc = alloc; reset = reset; copy = copy; step = step }
