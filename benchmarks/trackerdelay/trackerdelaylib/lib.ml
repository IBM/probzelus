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

open Owl
open Ztypes

type 'a buffer = 'a array

let buffer (n, x) =
  Array.make (n + 1) x

let push (x, b) =
  Array.init (Array.length b) (fun i -> if i = 0 then x else b.(i - 1))

let peek (n, b) =
  Array.get b n

let max_delay = 10

let of_lists l = Mat.of_arrays (Array.of_list (List.map Array.of_list l))
let of_list l n m = Mat.of_array (Array.of_list l) n m

let diagm l = Mat.diagm (of_lists [l])
let vec l = of_list l (List.length l) 1

let a = of_lists
  [[ 1.0; 0.1; 0.0 ];
   [ 0.0; 1.0; 0.1 ];
   [ 0.0; 0.0; 0.0 ]]

let a_approx = of_lists
  [[ 1.0; 0.1; 0.0 ];
   [ 0.0; 1.0; 0.1 ];
   [ 0.0; 0.0; 0.000001 ]]

let b = Mat.eye 3
let q = diagm [1.0; 0.1; 0.1]
let r = diagm [1000.; 1000.; 1.0]
let n = Mat.zeros 3 3

let ( +@ ) x y = Mat.add x y
let ( *@ ) x y = Mat.dot x y
