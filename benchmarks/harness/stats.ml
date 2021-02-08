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


module IntSet = Set.Make(Int)

let option_eq o x =
  begin match o with
  | None -> true
  | Some y -> x = y
  end

let option_iter f o =
  begin match o with
  | Some x -> f x
  | None -> ()
  end

let array_flatten arr =
  let l1 = Array.length arr in
  let l2 = Array.length arr.(0) in
  let res = Array.make (l1 * l2) arr.(0).(0) in
  let k = ref 0 in
  for i = 0 to l1 - 1 do
    for j = 0 to l2 - 1 do
      res.(!k) <- arr.(i).(j);
      incr k;
    done
  done;
  res

let array_unzip arr =
  let l = Array.length arr in
  let ret1 = Array.make l 0. in
  let ret2 = Array.make l 0. in
  for i = 0 to l - 1 do
    let (v1, v2) = arr.(i) in
    ret1.(i) <- v1;
    ret2.(i) <- v2
  done;
  (ret1, ret2)

let array_transpose arr =
  let l1 = Array.length arr in
  let l2 = Array.length arr.(0) in
  Array.init l2
    (fun i ->
       Array.init l1
         (fun j ->
            arr.(j).(i)))

let array_assoc x a =
  let res = ref None in
  Array.iter
    (fun (y, v) -> if x = y then res := Some v)
    a;
  begin match !res with
  | None -> raise Not_found
  | Some v -> v
  end


let stats (lower_quantile, middle_quantile, upper_quantile) arr =
  let len_i = Array.length arr in
  let len = float_of_int len_i in
  Array.sort compare arr;
  let upper_idx = min (len_i - 1) (truncate (len *. upper_quantile +. 0.5)) in
  let lower_idx = min (len_i - 1) (truncate (len *. lower_quantile +. 0.5)) in
  let middle_idx = min (len_i - 1) (truncate (len *. middle_quantile +. 0.5)) in
  (Array.get arr lower_idx, Array.get arr middle_idx, Array.get arr upper_idx)


let output_stats pgf_format file idx_label value_label stats  =
  let ch = open_out file in
  let fmt = Format.formatter_of_out_channel ch in
  Format.fprintf fmt
    "%s, %s lower quantile, median, upper quantile@."
    idx_label value_label;
  if not pgf_format then begin
    Array.iter
      (fun (idx, (low, mid, high)) ->
         Format.fprintf fmt "%d, %f, %f, %f@." idx low mid high)
      stats;
  end
  else begin
    Array.iter
      (fun (idx, (low, mid, high)) ->
         Format.fprintf fmt "%d   %f   %f   %f@." idx mid (mid -. low) (high -. mid))
      stats;
  end;
  close_out ch


let read_stats file =
  let ch = open_in file in
  let _ = input_line ch in
  let ic = Scanf.Scanning.from_channel ch in
  let acc = ref [] in
  begin try
    while true do
      let entry =
        Scanf.bscanf ic ("%d, %f, %f, %f\n")
          (fun idx low mid high -> (idx, (low, mid, high)))
      in
      acc := entry :: !acc

    done
  with End_of_file -> ()
  end;
  Array.of_list (List.rev !acc)


type entry =
  { entry_time_stamp : float;
    entry_name : string;
    entry_algo : string;
    entry_particles : int;
    entry_loss : float;
    entry_time : float;
    entry_gc : float;
    entry_result : string; }

let fprint_per_particles_header ppf =
  Format.fprintf ppf
    "Time stamp, Model, Algo, #particles, time, loss, live words, output@."

let fprint_per_step_header ppf =
  Format.fprintf ppf "Step, #step, ";
  fprint_per_particles_header ppf

let printf_entry ppf { entry_time_stamp ;
                       entry_name;
                       entry_algo;
                       entry_particles;
                       entry_loss;
                       entry_time;
                       entry_gc;
                       entry_result; } =
    Format.fprintf ppf "%f, %s, %s, %d, %f, %f, %f, %s@\n"
      entry_time_stamp
      entry_name
      entry_algo
      entry_particles
      entry_time
      entry_loss
      entry_gc
      entry_result



exception Bad_format of string

let bad_format msg =
  raise (Bad_format msg)

let csv_separator = ','

let load_csv ?excel_tricks file =
  try Csv.load ~separator:csv_separator ?excel_tricks file
  with Csv.Failure (record, field, msg) ->
    let msg =
      Format.sprintf "Fichier %s, ligne %d, colonne %d : %s"
        file record field msg
    in
    bad_format msg

let entry_of_list l =
  begin match l with
  | time_stamp :: name :: algo :: particles :: time :: loss :: gc :: tl ->
      let result =
        begin match tl with
        | [] -> ""
        | result_hd :: result_tl ->
            List.fold_left (fun acc x -> acc ^ ", " ^ x) result_hd result_tl
        end
      in
      { entry_time_stamp = float_of_string time_stamp;
        entry_name = name;
        entry_algo = algo;
        entry_particles = int_of_string particles;
        entry_loss = float_of_string loss;
        entry_time = float_of_string time;
        entry_gc  = float_of_string gc;
        entry_result = result; }
  | _ -> assert false
  end

let per_step_entry_of_list l =
  begin match l with
  | "Step" :: i :: entry ->
      (int_of_string i, entry_of_list entry)
  | _ -> assert false
  end

let load_per_particles_csv file =
  List.map entry_of_list (load_csv file)

let load_per_step_csv file =
  List.map per_step_entry_of_list (load_csv file)


let filter ?name ?algo ?particles l =
  List.filter
    (fun x ->
       option_eq name x.entry_name &&
       option_eq algo x.entry_algo &&
       option_eq particles x.entry_particles)
    l

let get_loss_array l =
  Array.of_list (List.map (fun x -> x.entry_loss) l)

let get_time_array l =
  Array.of_list (List.map (fun x -> x.entry_time) l)

let get_gc_array l =
  Array.of_list (List.map (fun x -> x.entry_gc) l)


(* Per particles stats *)

let per_particles_stats get_info config l =
  let particles =
    List.fold_left
      (fun acc x -> IntSet.add x.entry_particles acc)
      IntSet.empty l
  in
  let res =
    IntSet.fold
      (fun n acc ->
         let a = get_info (filter ~particles:n l) in
         (n, stats config a) :: acc)
      particles []
  in
  Array.of_list (List.rev res)

let per_particles_accuracy_stats config l =
  per_particles_stats get_loss_array config l

let per_particles_latency_stats config l =
  per_particles_stats get_time_array config l

(* Per step stats *)

let per_step_stats get_info config l =
  let steps =
    List.fold_left
      (fun acc (i, _) -> IntSet.add i acc)
      IntSet.empty l
  in
  let res =
    IntSet.fold
      (fun n acc ->
         let entries =
           List.fold_left
             (fun acc (i, entry) -> if i = n then entry :: acc else acc)
             [] l
         in
         let a = get_info entries in
         (n, stats config a) :: acc)
      steps []
  in
  Array.of_list (List.rev res)

let per_step_latency_stats config l =
  per_step_stats get_time_array config l

let per_step_memory_stats config l =
  per_step_stats get_gc_array config l
