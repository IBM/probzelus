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
open Owl_plplot

type mat = Mat.mat

let print = Mat.print
let shape = Mat.shape
let row = Mat.row
let col = Mat.col
let get = Mat.get
let add = Mat.add
let add_scalar = Mat.add_scalar
let elt_greater_equal = Mat.elt_greater_equal
let elt_equal = Mat.elt_equal
let map f (params, m) = Mat.map (fun x -> f (params, x)) m
let sum axis = Mat.sum ~axis:axis
let load_txt = Mat.load_txt ~sep: " "

let pca n_components train =
  let x = Mat.(train - (mean ~axis:0 train)) in
  let u,s,v = Linalg.D.svd x in
  let k = n_components - 1 in
  let v = Mat.(v.${[]; [0; k]}) in
  fun test -> Mat.(test *@ v)

let split_data p m =
  let extract_labels m =
    Mat.(m.${[]; [0;-2]}),
    Mat.(m.${[]; [-1]} -$ 1.)
  in
  let n, _ = Mat.shape m in
  let tflag = Mat.bernoulli ~p n 1 in
  let train_idx = Mat.filter ((=) 1.) tflag in
  let test_tidx = Mat.filter ((=) 0.) tflag in
  extract_labels (Mat.rows m train_idx),
  extract_labels (Mat.rows m test_tidx)

let res = ref []
let add_result acc = res := acc::!res

let h = Plot.create "plot_accuracy.pdf";;
Plot.set_page_size h 500 400

let exit_and_plot () =
  let res = Array.of_list (List.rev !res) in
  let res = Arr.of_array res [|Array.length res|] in
  Plot.semilogx ~h ~spec:[ RGB (150,0,0) ] res;
  Plot.output h;
  exit 0
