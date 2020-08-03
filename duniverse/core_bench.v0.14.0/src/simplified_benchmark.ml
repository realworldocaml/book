open Core
open Poly

module R = Analysis_result.Regression
module C = Analysis_result.Coefficient

module Regression_info = struct
  type t =
    { value : float
    ; predictors : string list
    }
  [@@deriving typerep]
end

module Field_type = struct
  type t =
    | Time_per_run
    | Minor_words_per_run
    | Major_words_per_run
    | Promoted_words_per_run
  [@@deriving compare]

  let equal = [%compare.equal: t]

  let to_string = function
    | Time_per_run -> "time_per_run_nanos"
    | Minor_words_per_run -> "minor_words_per_run"
    | Major_words_per_run -> "major_words_per_run"
    | Promoted_words_per_run -> "promoted_words_per_run"

  let of_string = function
    | "time_per_run_nanos" -> Time_per_run
    | "minor_words_per_run" -> Minor_words_per_run
    | "major_words_per_run" -> Major_words_per_run
    | "promoted_words_per_run" -> Promoted_words_per_run
    | _ -> raise (Failure "Not a valid field type")

  let of_short_string = function
    | "time" -> Time_per_run
    | "mwd"  -> Minor_words_per_run
    | "mjwd" -> Major_words_per_run
    | "prom" -> Promoted_words_per_run
    | _      -> raise (Failure "Not a valid short string")

  let to_short_string = function
    | Time_per_run           -> "time"
    | Minor_words_per_run    -> "mwd"
    | Major_words_per_run    -> "mjwd"
    | Promoted_words_per_run -> "prom"

  let to_label_string = function
    | Time_per_run           -> "Time (ns) per run"
    | Minor_words_per_run    -> "Minor Words per run"
    | Major_words_per_run    -> "Major Words per run"
    | Promoted_words_per_run -> "Promoted Words per run"

  let all =
    [ Time_per_run
    ; Minor_words_per_run
    ; Major_words_per_run
    ; Promoted_words_per_run
    ]

end

(* each result independently stores redundant info because Kibana can only handle flat
   document structure, i.e. no nesting *)
module Result = struct
  type t =
    { benchmark_name                  : string
    ; benchmark_name_with_index       : string
    ; full_benchmark_name             : string
    ; dup_id                          : int option
    ; file_name                       : string
    ; module_name                     : string
    ; library_name                    : string
    ; version                         : string
    ; hg_revision                     : string option
    ; hg_active_bookmark              : string option
    ; x_library_inlining              : bool
    ; ocaml_version                   : string
    ; machine_where_benchmark_was_run : string
    ; epoch_time_of_run               : Int63.t
    ; time_of_hg_revision             : string option
    ; time_r_square                   : float
    ; time_per_run_nanos              : float
    ; ci95_upper_bound                : float
    ; ci95_lower_bound                : float
    ; minor_words_per_run             : float
    ; major_words_per_run             : float
    ; promoted_words_per_run          : float
    }
  [@@deriving typerep, sexp]
end

module Results = struct
  type t = Result.t list
  [@@deriving typerep, sexp]
end

let extract ?(libname="") (results : Analysis_result.t list) =
  let get_bench_name_with_index res =
    let name = Analysis_result.name res in
    match String.lsplit2 ~on:']' name with
    | Some (_, subname) when String.is_prefix subname ~prefix:" " ->
      String.drop_prefix subname 1
    | Some (_, _)
    | None -> name
  in
  let estimate regr = C.estimate (R.coefficients regr).(0) in
  let get_ci regr = C.ci95 (R.coefficients regr).(0) in
  let check_time_preds regr =
    let preds = R.predictors regr in
    Array.length preds = 1 && Array.exists preds ~f:((=) `Runs)
  in
  let check_overhead_preds regr =
    let preds = R.predictors regr in
    Array.length preds = 2 && Array.exists preds ~f:((=) `One)
    && Array.exists preds ~f:((=) `Runs)
  in
  let cur_time = (Time_ns.now () |> Time_ns.to_int63_ns_since_epoch) in
  let version = Version_util.version in
  let simplified_results = List.map results ~f:(fun res ->
    let jtest =
      Result.({
        full_benchmark_name = Analysis_result.name res
      ; benchmark_name = Analysis_result.test_name res
      ; benchmark_name_with_index = get_bench_name_with_index res
      ; dup_id = None
      ; file_name = Analysis_result.file_name res
      ; module_name = Analysis_result.module_name res
      ; library_name = libname
      ; version = version
      ; hg_revision = None
      ; hg_active_bookmark = None
      ; x_library_inlining = Version_util.x_library_inlining
      ; ocaml_version = Version_util.ocaml_version
      ; machine_where_benchmark_was_run = Unix.gethostname ()
      ; epoch_time_of_run = cur_time
      ; time_of_hg_revision = None
      ; time_r_square = 0.
      ; time_per_run_nanos = 0.
      ; ci95_upper_bound = 0.
      ; ci95_lower_bound = 0.
      ; minor_words_per_run = 0.
      ; major_words_per_run = 0.
      ; promoted_words_per_run = 0.
      }) in
    Array.fold (Analysis_result.regressions res) ~init:jtest ~f:(fun acc regr ->
      let acc =
        match R.r_square regr with
        | Some rsq -> { acc with time_r_square = rsq }
        | None -> acc
      in
      let value = estimate regr in
      match (R.responder regr) with
      | `Nanos ->
        if check_time_preds regr
        then
          begin match get_ci regr with
          | None -> { acc with time_per_run_nanos = value }
          | Some ci ->
            let (ci_minus, ci_plus) = Analysis_result.Ci95.ci95_rel_err ci ~estimate:value
            in { acc with time_per_run_nanos = value
                        ; ci95_upper_bound = ci_plus
                        ; ci95_lower_bound = ci_minus
               }
          end
        else acc
      | `Minor_allocated ->
        if check_overhead_preds regr
        then { acc with minor_words_per_run = value }
        else acc
      | `Major_allocated ->
        if check_overhead_preds regr
        then { acc with major_words_per_run = value }
        else acc
      | `Promoted ->
        if check_overhead_preds regr
        then { acc with promoted_words_per_run = value }
        else acc
      | _ -> acc
    )
  ) in
  simplified_results

let to_sexp ?libname results =
  extract ?libname results |> Results.sexp_of_t

let%expect_test "extract" =
  let analysis_results =
    List.map ~f:(fun s -> Sexp.of_string s |> Analysis_result.t_of_sexp)
      [ {|((name alloc_list:10) (test_name "") (file_name "") (module_name "")
  (sample_count 1059) (largest_run 790170)
  (regressions
   (((responder Nanos) (r_square ())
     (coefficients
      (((predictor Runs) (estimate 12.614163005077506) (ci95 ()))))
     (regression_name ()) (key 1025))
    ((responder Minor_allocated) (r_square ())
     (coefficients
      (((predictor Runs) (estimate 12.00006865591944) (ci95 ()))
       ((predictor One) (estimate 30.998178509821386) (ci95 ()))))
     (regression_name ()) (key 3585))
    ((responder Major_allocated) (r_square ())
     (coefficients
      (((predictor Runs) (estimate 0.00010110434458848057) (ci95 ()))
       ((predictor One) (estimate 3.8155829518820092) (ci95 ()))))
     (regression_name ()) (key 4609))
    ((responder Promoted) (r_square ())
     (coefficients
      (((predictor Runs) (estimate 0.00010110434458848057) (ci95 ()))
       ((predictor One) (estimate 3.8155829518820092) (ci95 ()))))
     (regression_name ()) (key 5633)))))|}
      ; {|((name "[test.ml] Time.now") (test_name Time.now) (file_name test.ml)
  (module_name "") (sample_count 892) (largest_run 150024)
  (regressions
   (((responder Nanos) (r_square ())
     (coefficients (((predictor Runs) (estimate 65.779200048688) (ci95 ()))))
     (regression_name ()) (key 1025))
    ((responder Minor_allocated) (r_square ())
     (coefficients
      (((predictor Runs) (estimate 4.000015101820142) (ci95 ()))
       ((predictor One) (estimate 31.001015087871728) (ci95 ()))))
     (regression_name ()) (key 3585))
    ((responder Major_allocated) (r_square ())
     (coefficients
      (((predictor Runs) (estimate 9.51464237451749E-05) (ci95 ()))
       ((predictor One) (estimate -0.0087109316142560851) (ci95 ()))))
     (regression_name ()) (key 4609))
    ((responder Promoted) (r_square ())
     (coefficients
      (((predictor Runs) (estimate 9.51464237451749E-05) (ci95 ()))
       ((predictor One) (estimate -0.0087109316142560851) (ci95 ()))))
     (regression_name ()) (key 5633)))))|}
      ]
  in
  (* We can't just [print_s (to_sexp analysis_results)] because many fields in a
     [Result.t] depend on when the code is run, on what box, etc.  We just focus on
     testing the formerly problematic part of [extract]. *)
  let results = extract analysis_results in
  List.iter results ~f:(fun result ->
    printf "%s; %s\n" result.benchmark_name result.benchmark_name_with_index
  );
  [%expect {|
    ; alloc_list:10
    Time.now; Time.now |}]
