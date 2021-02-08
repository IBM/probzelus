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
open Mttlib
open Util

let print_metrics (d, c, fp, m, mme, g) =
  print_string ("distance: " ^ string_of_float d ^ "\n");
  print_string ("matches: " ^ string_of_int c ^ "\n");
  print_string ("false positives: " ^ string_of_int fp ^ "\n");
  print_string ("misses: " ^ string_of_int m ^ "\n");
  print_string ("mismatches: " ^ string_of_int mme ^ "\n");
  print_string ("objects: " ^ string_of_int g ^ "\n")

let v1 = Mat.of_arrays [| [| 0. |];
                          [| 1. |]; |];;

let v2 = Mat.of_arrays [| [| 1. |];
                          [| 1. |] |];;

print_string ("dist(" ^ string_of_vec2 v1 ^ ", " ^ string_of_vec2 v2 ^ ") = " ^ 
              string_of_float (dist v1 v2) ^"\n")

let v3 = Mat.of_arrays [| [| 0. |];
                          [| 1. |]; |];;

let v4 = Mat.of_arrays [| [| 1. |];
                          [| 0. |] |];;

print_string ("dist(" ^ string_of_vec2 v3 ^ ", " ^ string_of_vec2 v4 ^ ") = " ^ 
              string_of_float (dist v3 v4) ^"\n");;

let assoc_list_tostring lst =
  "[" ^ (String.concat "," (List.map (fun (x,y) ->
      "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
    ) lst)) ^ "]"

let cmat1 = Mat.of_arrays [| [| 1. ;  2. ; 3. |];
                             [| 2. ;  4. ; 6. |];
                             [| 3. ;  6. ; 9. |] |];;

print_string "cmat1:\n";;
Mat.print cmat1;;
print_string "matching of cmat1:\n";;
print_string (assoc_list_tostring (optimal_assignment cmat1) ^ "\n");;

let cmat2 = Mat.of_arrays [| [| 1. ;  2. ; 3. ; 4. |];
                             [| 2. ;  4. ; 6. ; 8. |];
                             [| 3. ;  6. ; 9. ; 12.|] |];;

print_string "cmat2:\n";;
Mat.print cmat2;;
print_string "matching of cmat2:\n";;
print_string (assoc_list_tostring (optimal_assignment cmat2) ^ "\n");;


let match0 = empty_matching;;

print_string "match0\n";;
print_string (match_tostring match0 5);;

let truth0 = [ (1, v1) ; (2, v2) ];;
let hyp0 = [ (1, v2) ; (2, v1) ];;

print_string "truth0:\n";;
print_string (string_of_tr truth0 ^ "\n");;

print_string "hyp0:\n";;
print_string (string_of_tr hyp0 ^ "\n");;

let metrics0, match1 = matching match0 truth0 hyp0;;

print_metrics metrics0;;
print_string ("match1:\n");;
print_string (match_tostring match1 0);;

let truth1 = [ (1, v1) ; (2, v2) ];;
let hyp1 = [ (1, v2) ; (2, v1) ; (3, v4) ];;

print_string "truth1:\n";;
print_string (string_of_tr truth1 ^ "\n");;

print_string "hyp1:\n";;
print_string (string_of_tr hyp1 ^ "\n");;

let metrics1, match2 = matching match1 truth1 hyp1;;

print_metrics metrics1;;
print_string ("match2:\n");;
print_string (match_tostring match2 0);;

let truth2 = [ (1, v1) ; (2, v2) ; (3, v4) ];;
let hyp2 = [ (1, v2) ; (2, v1) ];;

print_string "truth2:\n";;
print_string (string_of_tr truth2 ^ "\n");;

print_string "hyp2:\n";;
print_string (string_of_tr hyp2 ^ "\n");;

let metrics2, match3 = matching match2 truth2 hyp2;;

print_metrics metrics2;;
print_string ("match3:\n");;
print_string (match_tostring match3 0);;


let v5 = Mat.of_arrays [| [| 10. |];
                          [| 11. |]; |];;

let v6 = Mat.of_arrays [| [| 15. |];
                          [| 16. |] |];;

let v7 = Mat.of_arrays [| [| 9. |];
                          [| 10. |]; |];;

let v8 = Mat.of_arrays [| [| 16. |];
                          [| 17. |] |];;

let truth3 = [ (1, v5); (2, v6) ];;
let hyp3 = [ (1, v7); (2, v8) ];;

print_string "truth3:\n";;
print_string (string_of_tr truth3 ^ "\n");;

print_string "hyp3:\n";;
print_string (string_of_tr hyp3 ^ "\n");;

let metrics3, match4 = matching match3 truth3 hyp3;;

print_metrics metrics3;;
print_string "match4:\n";;
print_string (match_tostring match4 5);;

let truth4 = [ (1, v5); (2, v6) ];;
let hyp4 = [ (1, v7); (2, v5) ];;

print_string "truth4:\n";;
print_string (string_of_tr truth4 ^ "\n");;

print_string "hyp4:\n";;
print_string (string_of_tr hyp4 ^ "\n");;

let metrics4, match5 = matching match4 truth4 hyp4;;

print_metrics metrics4;;
print_string "match5:\n";;
print_string (match_tostring match5 5);;

let truth5 = [ (1, v5); (2, v5); (3, v5) ];;
let hyp5 = [ (1, v5); (2, v5); (3, v5) ];;

print_string "truth5:\n";;
print_string (string_of_tr truth5 ^ "\n");;

print_string "hyp5:\n";;
print_string (string_of_tr hyp5 ^ "\n");;

let init_match = TrMap.add 1 3 TrMap.empty
let metrics5, match6 = matching init_match truth5 hyp5 ;;

print_metrics metrics5;;

print_string "match6:\n";;
print_string (match_tostring match6 5);;

