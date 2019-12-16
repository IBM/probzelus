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

open Owl;;
open Ztypes;;
open Mttlib;;

let read_input () =
  let mk_vec a b =
      Mat.of_arrays [| [| a |];
                       [| b |] |]
  in
  let process_vec_list _ = 
    let rec process_list_helper fst =
      try
        Scanf.scanf "]" (fun _ -> ()) 0;
        []
      with Scanf.Scan_failure _ ->
        let this_elem = if fst then
          Scanf.scanf "(%f, %f)" (fun x y -> mk_vec x y) 
        else 
          Scanf.scanf ",(%f, %f)" (fun x y -> mk_vec x y) 
        in
        this_elem :: (process_list_helper false)
    in
    Scanf.scanf "[" (fun _ -> ()) 0;
    process_list_helper true
  in

  let process_tr_list _ = 
    let rec process_list_helper fst =
      try
        Scanf.scanf "]" (fun _ -> ()) 0;
        []
      with Scanf.Scan_failure _ ->
        let this_elem = if fst then
          Scanf.scanf "(%i, %f, %f)" (fun i x y -> (i, mk_vec x y)) 
        else 
          Scanf.scanf ",(%i, %f, %f)" (fun i x y -> (i, mk_vec x y)) 
        in
        this_elem :: (process_list_helper false)
    in
    Scanf.scanf "[" (fun _ -> ()) 0;
    process_list_helper true
  in


  let truth = process_tr_list () in
  Scanf.scanf " : " (fun _ -> ()) 0;
  let obs = process_vec_list () in

  Scanf.scanf "\n" (fun _ -> ()) 0;
  (truth, obs)

let run () = 
    let Cnode {alloc; reset; step; copy = _} = Mtt_ds_nogc.main 1000 in
    let state = alloc () in
    reset state;
    let rec do_run () =
        try 
            let (tr, obs) = read_input () in
            let (_, err) = step state (tr, obs) in
            print_string (string_of_float err ^ "\n");
            do_run ()
        with End_of_file -> ()
    in
    do_run ()
;;

run ()

