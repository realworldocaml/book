open Core
open Poly

module R = Analysis_result.Regression
module C = Analysis_result.Coefficient
module Magnitude = Display_units.Magnitude

module Warnings = struct
  let long_running_benchmark_time_limit_nanos = 1E8
  let has_long_running_benchmarks = ref false

  let check_for_long_running_benchmarks ~(resp:Variable.t) ~(pred:Variable.t) est =
    match (resp, pred) with
    | (`Nanos, `Runs) ->
      if est >= long_running_benchmark_time_limit_nanos
      then has_long_running_benchmarks := true;
    | (_, _) -> ()

  let display () =
    if !has_long_running_benchmarks
    then printf
           "Benchmarks that take 1ns to %s can be estimated precisely. For more reliable \n\
            estimates, redesign your benchmark to have a shorter execution time.\n%!"
           (Time.Span.to_string
              (Time.Span.of_ns
                 long_running_benchmark_time_limit_nanos))
end

module Regr = struct
  module Coeff = struct
    type t = {
      predictor      : Variable.t;
      mutable units   : Display_units.t;
      mutable magnitude : Magnitude.t;
      mutable has_ci : bool;
      mutable smallest : float option;
      mutable largest : float option;
    }

    let create units predictor = {
      predictor;
      units;
      has_ci=false;
      smallest=None;
      largest=None;
      magnitude=Magnitude.max;
    }

    let update t coeff =
      let select cmp n n_opt =
        match n, n_opt with
        | n, None -> Some n
        | n, Some m -> Some (cmp n m)
      in
      t.has_ci <- t.has_ci || C.has_ci95 coeff;
      let est = C.estimate coeff in
      t.smallest <- select min est t.smallest;
      t.largest  <- select max est t.largest;
      t.magnitude <- Magnitude.smaller t.magnitude (Magnitude.magnitude t.units est)
  end

  type t = {
    responder            : Variable.t;
    coeffs               : Coeff.t array;
    key                  : int;
    regression_name      : string option;
    mutable has_r_square : bool;
  }

  let create regr =
    let responder = R.responder regr in
    let units = Variable.get_units responder in
    {
      responder;
      coeffs          = Array.map (R.predictors regr) ~f:(Coeff.create units);
      key             = R.key regr;
      regression_name = R.regression_name regr;
      has_r_square    = false;
    }

  let update t ~regr =
    t.has_r_square <- t.has_r_square || R.has_r_square regr;
    Array.iteri (R.coefficients regr) ~f:(fun i coeff ->
      Coeff.update t.coeffs.(i) coeff)

  let create_col t str ~f =
    Ascii_table.Column.create_attr
      ~align:Right
      ~show:`If_not_empty
      str
      (fun res ->
         match Analysis_result.find_key res t.key with
         | None -> ([], "?")
         | Some regr -> f regr)

  let make_columns ~show_absolute_ci ~show_all_values ~show_overheads t =
    let append_name ~est col =
      match t.regression_name,  est && (Array.length t.coeffs = 1) with
      | Some name, true -> name
      | Some name, false -> col ^ name
      | None,_ -> col
    in
    let unit = Variable.get_units t.responder in
    let name = Variable.to_short_string t.responder in
    let cols = [] in
    let cols =
      (* Display R^2 is required *)
      if t.has_r_square then
        let r_square =
          create_col t (append_name (name ^ " R^2") ~est:false)  ~f:(fun regr ->
            let non_triv =
              Array.fold ~init:false (R.coefficients regr)
                ~f:(fun acc coeff ->
                  (acc || C.has_non_trivial_estimate
                            coeff
                            ~show_all_values:false
                            ~responder:t.responder))
            in
            if non_triv
            then ([], To_string.float_opt_to_string (R.r_square regr))
            else if show_all_values
            then ([`Dim], To_string.float_opt_to_string (R.r_square regr))
            else ([], ""))
        in
        r_square :: cols
      else
        cols
    in
    List.rev (Array.foldi t.coeffs ~init:cols ~f:(fun i acc coeff ->
      if coeff.Coeff.predictor = `One && (not show_overheads)
      then acc
      else begin
        let mag = coeff.Coeff.magnitude in
        (* Display Estimates *)
        let est_col =
          create_col t
            (append_name (Variable.make_col_name t.responder coeff.Coeff.predictor) ~est:false)
            ~f:(fun regr ->
              let est = (R.coefficients regr).(i) in
              Warnings.check_for_long_running_benchmarks
                ~resp:t.responder
                ~pred:coeff.Coeff.predictor
                (C.estimate est);
              Display_units.to_string ~show_all_values unit mag (C.estimate est))
        in
        (* Display confidence intervals *)
        if coeff.Coeff.has_ci then
          let est_ci_col =
            create_col t (append_name "95ci" ~est:false)
              ~f:(fun regr ->
                let est = (R.coefficients regr).(i) in
                match C.ci95 est with
                | None -> ([], "")
                | Some ci ->
                  (* Suppress the ci if the estimate has been suppressed. *)
                  let non_triv = C.has_non_trivial_estimate est
                                   ~show_all_values:false
                                   ~responder:t.responder
                  in
                  if non_triv || show_all_values then
                    let attr, str =
                      if show_absolute_ci then
                        Display_units.to_ci_string ~show_all_values unit mag
                          (Analysis_result.Ci95.ci95_abs_err ci ~estimate:(C.estimate est))
                      else
                        Display_units.to_ci_string ~show_all_values
                          Display_units.Percentage mag
                          (Analysis_result.Ci95.ci95_rel_err ci ~estimate:(C.estimate est))
                    in
                    let attr = if show_all_values then (`Dim :: attr) else attr in
                    (attr, str)
                  else ([], ""))
          in est_ci_col :: est_col :: acc
        else
          est_col :: acc
      end))

end

let make_speed_and_percentage_columns display_config tbl =
  let show_percentage = display_config.Display_config.show_percentage in
  let show_speedup = display_config.Display_config.show_speedup in
  let show_all_values = display_config.Display_config.show_all_values in
  if show_percentage || show_speedup then begin
    (* To computer speedup and percentage, we need the Nanos-vs-Rubs regression as to be
       present in the results. *)
    let timing_key = Analysis_config.make_key Analysis_config.nanos_vs_runs in
    match Int.Table.find tbl timing_key with
    | None ->
      printf "Error: Estimating speedup/percentage requires Nanos-vs-Runs analysis.\n%!";
      []
    | Some regr ->
      let smallest =
        Option.value_exn regr.Regr.coeffs.(0).Regr.Coeff.smallest
          ~message:"Reading smallest Nanos-vs-Runs value"
      in
      let largest =
        Option.value_exn regr.Regr.coeffs.(0).Regr.Coeff.largest
          ~message:"Reading largest Nanos-vs-Runs value"
      in
      let get_coeff regr = C.estimate (R.coefficients regr).(0) in
      let cols = [] in
      let cols =
        if show_speedup then
          let col =
            Ascii_table.Column.create
              ~align:Right
              "Speedup"
              (fun res ->
                 match Analysis_result.find_key res timing_key with
                 | None -> "?"
                 | Some regr ->
                   To_string.float_to_string (get_coeff regr /. smallest))
          in
          col :: cols
        else cols
      in
      let cols =
        if show_percentage then
          let col =
            Ascii_table.Column.create_attr
              ~align:Right
              "Percentage"
              (fun res ->
                 match Analysis_result.find_key res timing_key with
                 | None -> ([], "?")
                 | Some regr ->
                   let dummy = Display_units.Magnitude.One in
                   Display_units.to_string
                     ~show_all_values
                     Display_units.Percentage dummy
                     (get_coeff regr /. largest))
          in
          col :: cols
        else cols
      in
      cols
  end else []

let make_columns_for_regressions display_config results =
  let tbl = Int.Table.create () in
  let add_to_table regr =
    Regr.update ~regr (Int.Table.find_or_add tbl (R.key regr)
                         ~default:(fun () -> Regr.create regr))
  in
  List.iter results ~f:(fun result ->
    Array.iter (Analysis_result.regressions result) ~f:(fun regr ->
      add_to_table regr));
  let regressions =
    List.sort (Int.Table.to_alist tbl) ~compare:(fun (a, _) (b, _) -> compare a b)
  in
  let show_absolute_ci = display_config.Display_config.show_absolute_ci in
  let show_all_values = display_config.Display_config.show_all_values in
  let show_overheads = display_config.Display_config.show_overheads in
  let cols =
    List.fold ~init:[] regressions ~f:(fun acc (_key, data) ->
      acc @ Regr.make_columns ~show_absolute_ci ~show_all_values ~show_overheads data)
  in
  cols @ (make_speed_and_percentage_columns display_config tbl)


let make_columns display_config results =
  let cols = make_columns_for_regressions display_config results in
  let cols =
    if display_config.Display_config.show_samples then
      let samples =
        Ascii_table.Column.create
          ~align:Right
          "Runs @ Samples"
          (fun res ->
             sprintf "%d @ %d"
               (Analysis_result.largest_run res)
               (Analysis_result.sample_count res))
      in
      samples :: cols
    else cols
  in
  let cols =
    let name =
      Ascii_table.Column.create
        ~align:Left
        "Name"
        (fun res -> Analysis_result.name res)
    in
    name :: cols
  in
  cols


let display ?libname ~display_config results =
  if display_config.Display_config.show_output_as_sexp
  then Simplified_benchmark.to_sexp ?libname results
       |> Sexp.to_string |> print_endline
  else begin
    let cols = make_columns display_config results in
    Ascii_table.output
      ~oc:stdout
      ~limit_width_to:(Display_config.limit_width_to display_config)
      ~bars:(if (Display_config.ascii_table display_config)
             then `Ascii
             else `Unicode)
      ~display:(Display_config.display display_config)
      cols
      results;
    Warnings.display ();
  end

