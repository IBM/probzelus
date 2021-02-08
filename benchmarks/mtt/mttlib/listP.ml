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

let map (Cnode { alloc; reset; copy; step; }) =
  let step state (pstate, l) =
    List.map (fun x -> step state (pstate, x)) l
  in
  Cnode { alloc; reset; copy; step; }


let ini (Cnode { alloc; reset; copy; step; }) =
  let step state (pstate, n) =
    if n < 0 then
      []
    else
      List.init n (fun x -> step state (pstate, x))
  in
  Cnode { alloc; reset; copy; step; }


let filter (Cnode { alloc; reset; copy; step; }) =
  let step state (pstate, l) =
    List.filter (fun x -> step state (pstate, x)) l
  in
  Cnode { alloc; reset; copy; step; }


let iter2 (Cnode { alloc; reset; copy; step; }) =
  let step state (pstate, (l1, l2)) =
    List.iter2 (fun x y -> step state (pstate, (x, y))) l1 l2
  in
  Cnode { alloc; reset; copy; step; }
