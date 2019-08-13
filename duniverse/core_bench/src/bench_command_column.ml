open Core

(* module T = struct
 *   type t =
 *   [ `Name
 *   | `Cycles
 *   | `Nanos
 *   | `Confidence
 *   | `Allocated
 *   | `Percentage
 *   | `GC
 *   | `Speedup
 *   | `Samples
 *   ] with sexp, compare
 * end
 *
 * module Map = Map.Make (T)
 * include T *)

(* include String *)

(* let name_desc_assoc_list = [
 *    ("cycles"    , `Cycles     , "Number of CPU cycles (RDTSC) taken.");
 *    ("time"      , `Nanos      , "Number of nano secs taken.");
 *    ("confidence", `Confidence , "95% confidence interval and R^2 error for predictors.");
 *    ("alloc"     , `Allocated  , "Allocation of major, minor and promoted words.");
 *    ("gc"        , `GC         , "Show major and minor collections per 1000 runs.");
 *    ("percentage", `Percentage , "Relative execution time as a percentage.");
 *    ("speedup"   , `Speedup    , "Relative execution cost as a speedup.");
 *    ("samples"   , `Samples    , "Number of samples collected for profiling.");
 *   ] *)

type t =
  | Analysis of Analysis_config.t list
  | Display_column of Display_column.t

let name_desc_assoc_list = [
   ( "time"
   , Analysis [ Analysis_config.nanos_vs_runs ]
   , "Number of nano secs taken.");

   ( "cycles"
   , Analysis [ Analysis_config.cycles_vs_runs ]
   , "Number of CPU cycles (RDTSC) taken.");

   ("alloc"
   , Analysis Analysis_config.allocations_vs_runs
   , "Allocation of major, minor and promoted words.");

   ("gc"
   , Analysis Analysis_config.gc_vs_runs
   , "Show major and minor collections per 1000 runs.");

   ("percentage"
   , Display_column `Percentage
   , "Relative execution time as a percentage.");

   ("speedup"
   , Display_column `Speedup
   , "Relative execution cost as a speedup.");

   ("samples"
   , Display_column `Samples
   , "Number of samples collected for profiling.");
  ]

let column_description_table =
  let max =
    let length (str, _, _) = String.length str in
    List.reduce_exn ~f:Int.max
      (List.map name_desc_assoc_list ~f:length)
  in
  let extend x =
    let slack = max - String.length x in
    x ^ String.make slack ' '
  in
  String.concat ~sep:"\n\t"
    (List.map name_desc_assoc_list ~f:(fun (name, _, desc) ->
      sprintf "%s - %s"
        (extend name) desc))

let name_assoc_list =
  List.map name_desc_assoc_list ~f:(fun (name, tag, _) -> (name, tag))

let of_string col  =
  let col, plus_prefix =
    match String.chop_prefix col ~prefix:"+" with
    | Some col -> col, true
    | None -> col, false
  in
  let t =
    match (List.Assoc.find ~equal:String.equal name_assoc_list col), plus_prefix with
    | Some t, false -> t
    | Some (Analysis ts), true ->
      Analysis (List.map ts ~f:(Analysis_config.with_error_estimation
                                  ~bootstrap_trials:Analysis_config.default_bootstrap_trials))
    | Some (Display_column _), true ->
      failwithf "Cannot compute error estimate for %s." col ()
    | None, _ -> failwithf "Invalid column name: %s" col ()
  in
  t

let arg = Command.Param.Arg_type.create of_string



