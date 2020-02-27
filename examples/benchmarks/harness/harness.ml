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

module Make(M: sig
    type input
    type output
    val name : string
    val algo : string
    val read_input : unit -> input
    val main : int -> (input, output * float) Ztypes.cnode
    val string_of_output : output -> string
  end) = struct

  module Config = struct
    let particles = ref 100
    let warmup = ref 0
    let num_runs = ref 1
    let file = ref None
    let file_append = ref false
    let with_result = ref false
    let seed = ref None
    let seed_long = ref None
    let per_step = ref false
    let mem_ideal = ref None
    let header = ref true

    let args =
      Arg.align [
        ("-particles", Set_int particles,
         "n Number of particles");
        ("-warmup", Set_int warmup,
         "n Number of warmup iterations");
        ("-num-runs", Set_int num_runs,
         "n Number of runs");
        ("-file", String (fun f -> file := Some f),
         "f output file");
        ("-append", Set file_append,
         " Append result to the file");
        ("-step", Set per_step,
         " Output statistics at each step");
        ("-result", Set with_result,
         " output the computed result");
        ("-mem-ideal", Bool (fun b -> mem_ideal := Some b),
         "bool Output memory statistics (with or without Gc.compact)");
        ("-no-header", Clear header,
         " Do not print the header in the output");
        ("-seed", Int (fun i -> seed := Some i),
         "n Set seed of random number generator");
        ("-seed-long", String (fun s -> seed_long := Some s),
         "n Set seed of random number generator (extra bits)");
      ]

    let () =
      Arg.parse args (fun _ -> ()) ("harness for "^M.algo^"/"^M.name)

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

  end

  open Ztypes

  let ppf, ch_opt =
    begin match !Config.file, !Config.file_append with
    | None, _ -> Format.std_formatter, None
    | Some f, false ->
        let ch = open_out f in
        Format.formatter_of_out_channel ch, Some ch
    | Some f, true ->
        let ch = open_out_gen [Open_creat; Open_text; Open_append] 0o640 f in
        Format.formatter_of_out_channel ch, Some ch
    end

  let () =
    begin match !Config.header with
    | false -> ()
    | true ->
        if !Config.per_step then
          Stats.fprint_per_step_header ppf
        else
          Stats.fprint_per_particles_header ppf
    end

  let rec read_file _ =
    try
      let s = M.read_input () in
      s :: read_file ()
    with End_of_file -> []

  let gc_stat () =
    begin match !Config.mem_ideal with
    | None -> 0.
    | Some false ->
        let st = Gc.stat () in
        let words = float_of_int (st.live_words) in
        words /. 1000.
    | Some true ->
        let () = Gc.compact () in
        let st = Gc.stat () in
        let words = float_of_int (st.live_words) in
        words /. 1000.
    end

  let string_of_output =
    begin match !Config.with_result with
    | true -> M.string_of_output
    | false -> (fun _ -> "")
    end

  let output_entry step loss time output =
    Stats.printf_entry ppf
      { Stats.entry_time_stamp = Unix.time();
        entry_name = M.name;
        entry_algo = M.algo;
        entry_particles = !Config.particles;
        entry_loss = loss;
        entry_time = time;
        entry_gc = gc_stat ();
        entry_result = string_of_output output; };
    ignore step;
    ()

  let time f x =
    let t_counter = Mtime_clock.counter () in
    let o = f x in
    let t =  Mtime.Span.to_ms (Mtime_clock.count t_counter) in
    (o, t)

  let get_step () =
    let Cnode {alloc; reset; step; copy = _} = M.main !Config.particles in
    let state = alloc () in
    reset state;
    Gc.compact ();
    (fun i -> step state i)

  let get_step_with_output () =
    let Cnode {alloc; reset; step; copy = _} = M.main !Config.particles in
    let state = alloc () in
    let cpt = ref 0 in
    reset state;
    Gc.compact ();
    let f i = step state i in
    fun i ->
      let (o, e), t = time f i in
      incr cpt;
      Format.fprintf ppf "Step, %d, " !cpt;
      output_entry step e t o;
      (o, e)

  let do_warmup n inp =
    let step = get_step () in
    for _ = 1 to n do
      List.iter
        (fun i ->
           let _, _ = step i in
           ())
        inp
    done

  let run_per_particle inp =
    let step = get_step () in
    assert (inp <> []);
    let (o, e), t =
      time
        (List.fold_left
           (fun _ i -> (step i)) (Obj.magic (): M.output * float))
        inp
    in
    if !Config.file <> None then Format.printf ".@?";
    output_entry step e t o;
    Format.fprintf ppf "@?";
    ()

  let run_per_step inp =
    let step = get_step_with_output () in
    List.iter (fun i -> ignore (step i)) inp;
    if !Config.file <> None then Format.printf ".@?";
    Format.fprintf ppf "@?";
    ()

  let run =
    begin match !Config.per_step with
    | true -> run_per_step
    | false -> run_per_particle
    end

  let do_runs num_runs inp =
    for _ = 0 to num_runs - 1 do
      run inp
    done;
    Format.printf "@.";
    ()

  let run () =
    let inp = read_file () in
    do_runs !Config.num_runs inp

end
