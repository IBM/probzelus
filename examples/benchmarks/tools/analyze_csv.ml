open Benchlib

let upper_quantile = 0.9
let lower_quantile = 0.1
let middle_quantile = 0.5
let stats_config = (lower_quantile, middle_quantile, upper_quantile)

module Config = struct
  let accuracy = ref None
  let perf = ref None
  let perf_step = ref None
  let mem_ideal = ref None
  let per_particles_csv = ref None
  let per_step_csv = ref None
  let per_step_mem_csv = ref None

  let args =
    Arg.align [
      ("-per_particles", String (fun file -> per_particles_csv := Some file),
       "file.csv data file");
      ("-per_step", String (fun file -> per_step_csv := Some file),
       "file.csv data file");
      ("-per_step_mem", String (fun file -> per_step_mem_csv := Some file),
       "file.csv data file");
      ("-acc", String (fun file -> accuracy := Some file),
       "file Accuracy testing" );
      ("-perf", String (fun file -> perf := Some file),
       "file Performance testing");
      ("-perf-step", String (fun file -> perf_step := Some file),
       "file Performance testing on a per step basis");
      ("-mem-ideal-step", String (fun file -> mem_ideal := Some file),
       " Memory testing on a per step basis with GC at each step");
      (* ("-pgf",  Set pgf_format, *)
      (*  "PGF Format"); *)
    ]

  let () =
    Arg.parse args (fun _ -> ()) "particles test harness"

  let () =
    if !accuracy <> None && !per_particles_csv = None ||
       !perf <> None && !per_particles_csv = None ||
       !perf_step <> None && !per_step_csv = None ||
       !mem_ideal <> None && !per_step_mem_csv = None then begin
      Arg.usage args
        "Bad arguments";
      exit 1
    end

end

let opt_map f o =
  begin match o with
  | None -> None
  | Some x -> Some (f x)
  end

let per_particles_entries =
  try
    opt_map Stats.load_per_particles_csv !Config.per_particles_csv
  with e ->
    Format.eprintf "%s@." (Printexc.to_string e);
    None

let per_step_entries =
  try
    opt_map Stats.load_per_step_csv !Config.per_step_csv
  with e ->
    Format.eprintf "%s@." (Printexc.to_string e);
    None

let per_step_mem_entries =
  try
    opt_map  Stats.load_per_step_csv !Config.per_step_mem_csv
  with e ->
    Format.eprintf "%s@." (Printexc.to_string e);
    None

let _per_particles_accuracy =
  begin match !Config.accuracy, per_particles_entries with
  | Some acc_file, Some entries ->
      let accuracy_stats =
        Stats.per_particles_accuracy_stats stats_config entries
      in
      Stats.output_stats false acc_file "particles" "loss" accuracy_stats
  | None, _ -> ()
  | Some _, None -> ()
  end

let _per_particles_latency =
  begin match !Config.perf, per_particles_entries with
  | Some perf_file, Some entries ->
      let latency_stats =
        Stats.per_particles_latency_stats stats_config entries
      in
      Stats.output_stats false perf_file "particles" "time in ms" latency_stats
  | None, _ -> ()
  | Some _, None -> ()
  end

let _per_step_latency =
  begin match !Config.perf_step, per_step_entries with
  | Some perf_file, Some entries ->
      let latency_stats =
        Stats.per_step_latency_stats stats_config entries
      in
      Stats.output_stats false perf_file "particles" "time in ms" latency_stats
  | None, _ -> ()
  | Some _, None -> ()
  end

let _per_step_mem =
  begin match !Config.mem_ideal, per_step_mem_entries with
  | Some mem_file, Some entries ->
      let memeory_stats =
        Stats.per_step_memory_stats stats_config entries
      in
      Stats.output_stats false mem_file "particles" "live words" memeory_stats
  | None, _ -> ()
  | Some _, None -> ()
  end
