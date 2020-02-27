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

module Config = struct
  let cmd = ref "false"
  let warmup = ref 1
  let min_particles = ref 1
  let max_particles = ref 100
  let exp_seq_flag = ref false
  let increment = ref 1
  let input_file = ref ""
  let per_particles_csv = ref None
  let per_step_csv = ref None
  let per_step_mem_csv = ref None

  let select_particle = ref None
  let num_runs = ref 10
  let seed = ref None
  let seed_long = ref None

  let args =
    Arg.align [
      ("-per_particles", String (fun file -> per_particles_csv := Some file),
       "file.csv output data file");
      ("-per_step", String (fun file -> per_step_csv := Some file),
       "file.csv output data file");
      ("-per_step_mem", String (fun file -> per_step_mem_csv := Some file),
       "file.csv output data file");
      ("-cmd", Set_string cmd,
       "cmd name of the command");
      ("-input", Set_string input_file,
       "file input data file");
      ("-w", Set_int warmup,
       "n Number of warmup iterations");
      ("-num-runs", Set_int num_runs,
       "n Number of runs");
      ("-particles", Int (fun i -> select_particle := Some i),
       "n Number of particles (single run)");
      ("-min-particles", Int (fun i -> min_particles := i),
       "n Lower bound of the particles interval");
      ("-max-particles", Int (fun i -> max_particles := i),
       "n Upper bound of the particles interval");
      ("-exp", Set exp_seq_flag,
       " Exponential sequence");
      ("-incr", Int (fun i -> increment := i),
       "n Increment in the particles interval");
      ("-seed", Int (fun i -> seed := Some i),
       "n Set seed of random number generator");
      ("-seed-long", String (fun s -> seed_long := Some s),
       "n Set seed of random number generator (extra bits)");
    ]

  let () =
    Arg.parse args (fun _ -> ()) "particles test harness"

  let () =
    begin match (!seed, !seed_long) with
    | None, None -> Random.self_init()
    | Some i, None -> Random.init i
    | _, Some s ->
        let ints = List.map (fun si -> int_of_string si)
            (String.split_on_char ',' s)
        in
        Random.full_init (Array.of_list ints)
    end

  let () =
    begin match !select_particle with
    | Some i -> min_particles := i; max_particles := i
    | None -> ()
    end

  let parts = ref !min_particles

  let particles _ =
    !parts

end

let do_runs particles =
  let mode_and_file =
    begin match !Config.per_particles_csv, !Config.per_step_csv, !Config.per_step_mem_csv with
    | Some file, None, None ->
        " -file "^file
    | None, Some file, None ->
        " -file "^file^" -step"
    | None, None, Some file ->
        " -file "^file^" -step -mem-ideal true"
    | None, None, None -> ""
    | _ -> assert false
    end
  in
  let warmup = " -warmup " ^ (string_of_int !Config.warmup) in
  let num_runs = " -num-runs " ^ (string_of_int !Config.num_runs) in
  let particles = " -particles " ^ (string_of_int particles) in
  let cmd =
    !Config.cmd ^ mode_and_file ^ warmup ^ num_runs ^ particles
    ^ " -no-header -append"
    ^ " < " ^ !Config.input_file
  in
  begin match Sys.command cmd with
  | 0 -> ()
  | n -> Format.eprintf "Error %d: \"%s\"@." n cmd
  end

let do_runs_particles particles_list =
  List.iter
    (fun particles -> do_runs particles)
    particles_list

let rec seq min incr max =
  if min > max then []
  else
    min :: seq (min + incr) incr max

let rec pow x n =
  if n = 0 then 1 else x * pow x (n - 1)

let exp_seq =
  let rec seq p incr exp max =
    if p > max then []
    else
      p ::
      if p >= exp then seq (p + (exp / 2)) (exp / 2) (10 * exp) max
      else seq (p + incr) incr exp max
  in
  fun min_particles max_particles ->
    let exp = pow 10 (int_of_float (log10 (float_of_int min_particles))) in
    let incr = max 1 (exp / 2) in
    let min = incr * min_particles / incr in
    seq min incr (10 * exp) max_particles

let run () =
  let particles_list =
    begin match !Config.exp_seq_flag with
    | false ->
        seq !Config.min_particles !Config.increment !Config.max_particles
    | true ->
        exp_seq !Config.min_particles !Config.max_particles
    end
  in
  do_runs_particles particles_list

let () = run ()
