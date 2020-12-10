open Core
open Poly

let debug = false

let random_indices_in_place ~max arr =
  let len = Array.length arr in
  for i = 0 to len - 1 do
    arr.(i) <- Random.int max
  done

(* [quantile_of_array] sorts the array and returns the values at the quantile indices.  If
   we ever expose this function, we should check that low_quantile and high_quantile are
   in the interval [0,1]. *)
let quantile_of_array arr ?(failures=0) ~len ~low_quantile ~high_quantile =
  Array.sort arr ~len ~compare:Float.compare;
  let index q =
    Float.iround_towards_zero_exn (Float.of_int len *. q +. 0.5 *. Float.of_int failures)
  in
  (* [extended_get i] retrieves entry [i] from [arr], pretending that
     [arr.(i) = infinity] when [i > len - 1], and that [arr.(i) = neg_infinity]
     when [i < failures].
     It assumes [i >= 0] and that entries with [i < failures] are already [neg_infinity].
  *)
  let extended_get i = if i >= len then Float.infinity else arr.(i) in
  (* For the low_quantile calculation, if the index is too large (too many failures),
     return the last entry of sorted array to preserve monotonicity *)
  let left_endpoint  = extended_get (Int.min (index low_quantile) (len - 1)) in
  let right_endpoint = extended_get (Int.max (index high_quantile) failures) in
  Analysis_result.Ci95.create  ~left_endpoint ~right_endpoint

let debug_print pred_matrix resp_vector =
  for i = 0 to (Array.length pred_matrix - 1) do
    printf "(%4d) " i;
    for j = 0 to (Array.length pred_matrix.(0) - 1) do
      printf "%3g " pred_matrix.(i).(j);
    done;
    printf "| %3g\n%!" resp_vector.(i);
  done

(* val ols : Measurement.t -> responder -> predictors -> float array *)
let make_lr_inputs ?indices meas ~resp ~preds =
  let module Ms = Measurement_sample in
  let preds_acc = Array.map preds ~f:Ms.accessor in
  let resp_acc = Ms.accessor resp in
  let make_pred_values ms = Array.map preds_acc ~f:(fun pred_acc -> pred_acc ms) in
  let mss = Measurement.samples meas in
  let pred_matrix, resp_vector =
    match indices with
    | Some indices ->
      Array.map indices ~f:(fun i -> make_pred_values mss.(i)),
      Array.map indices ~f:(fun i -> resp_acc mss.(i))
    | None ->
      Array.init (Measurement.sample_count meas) ~f:(fun i ->
        make_pred_values mss.(i)),
      Array.init (Measurement.sample_count meas) ~f:(fun i ->
        resp_acc mss.(i))
  in
  if debug
  then debug_print pred_matrix resp_vector;
  pred_matrix, resp_vector

let ols meas ~resp ~preds =
  if debug then begin
    Array.iteri preds ~f:(fun i pred ->
      printf "(%d) %s " i (Variable.to_string pred));
    printf "\n%!";
  end;
  let matrix, vector = make_lr_inputs meas ~resp ~preds in
  match Linear_algebra.ols ~in_place:true matrix vector with
  | Ok _ as x -> x
  | Error _ ->
    Or_error.error_string "\
Regression failed. (In Bench, this is usually because the predictors were
linearly dependent, commonly as a result of having a predictor that is always
zero, or having two predictors that are multiples of each other.)"

(* val r_square : Measurement.t -> responder -> predictors -> coefficients:float array -> float *)
let r_square meas ~resp ~preds ~coeffs =
  let predictors_matrix, responder_vector = make_lr_inputs meas ~resp ~preds in
  let sum_responder = Array.fold responder_vector ~init:0. ~f:(+.) in
  let mean = sum_responder /. Float.of_int (Array.length responder_vector) in
  let tot_ss = ref 0. in
  let res_ss = ref 0. in
  let predicted i =
    let x = ref 0. in
    for j = 0 to Array.length coeffs - 1 do
      x := !x +. predictors_matrix.(i).(j) *. coeffs.(j)
    done;
    !x
  in
  for i = 0 to Array.length responder_vector - 1 do
    tot_ss := !tot_ss +. (responder_vector.(i) -. mean) ** 2.;
    res_ss := !res_ss +. (responder_vector.(i) -. predicted i) ** 2.;
  done;
  1. -. !res_ss /. !tot_ss


(* A note about the constant [threshold = 10] (for the number of nonzero values each
   predictor must have) below:

   We are interested in producing 95% confidence intervals.  As a result, we want enough
   nonzero entries so that at least 95% of bootstrap replications succeed.  The
   probability that a particular row is omitted in a particular bootstrap replication is
   about 1/e = 0.36788.  If there are n nonzero entries in a column, the probability that
   they're all omitted is 0.36788^n; we want that to be less than 0.05.  n = 3 is
   sufficiently large for that.

   Of course, there are multiple columns to worry about. Supposing conservatively that the
   user wants to use up to 20 predictors, and noting that the probability that we get
   failure in some column is bounded above by the sum of the probabilities for the
   individual columns, we want 0.36788^n < 0.05/20.  n = 6 is sufficiently large for that.
   (In practice, the predictors will tend to be correlated, so the upper bound obtained by
   summing is conservative.)

   Something else to worry about is that the fundamental assumption made when using
   bootstrapping--that the empirical distribution is a good approximation of the
   underlying distribution--starts to break down if we have very few nonzero values.  So,
   for a little "breathing room", n = 10 should be sufficient.

   This should yield far fewer than 5% of bootstrap trials failing, so the following is a
   relatively minor point.  In the presence of failures, our 95% confidence interval still
   encompasses 95% of _all_ trials, including failures.  We position the confidence
   interval so that it is centered within the interval of all trials that succeeded.
   E.g., if there were 1000 trials, and no failures, we would ordinarily take the values
   at indices 25 and 975 as endpoints for the 95% confidence interval; if there are 20
   failures, we will (with all the failures sorted to the front of the array) instead take
   the values at indices 35 and 985.
*)
let bootstrap_threshold = 10

let can_bootstrap meas ~resp ~preds =
  let matrix, _vector = make_lr_inputs meas ~resp ~preds in
  let non_zero = Array.create ~len:(Array.length preds) 0 in
  let non_zero_cols = ref 0 in
  Array.iter matrix ~f:(fun row ->
    for i = 0 to Array.length non_zero - 1 do
      if row.(i) <> 0.0 then begin
        non_zero.(i) <- non_zero.(i) + 1;
        if non_zero.(i) = bootstrap_threshold then incr non_zero_cols
      end
    done);
  if !non_zero_cols = Array.length non_zero
  then None
  else Some (Error.of_string
               (sprintf "Columns %s have less that %d non-zero values."
                  (Array.foldi non_zero ~init:"" ~f:(fun i str col_count ->
                     if col_count < bootstrap_threshold
                     then sprintf "%s %s(non-zero %d)" str
                            (Variable.to_string preds.(i)) col_count
                     else str))
                  bootstrap_threshold))

let bootstrap ~trials meas ~resp ~preds =
  let num_preds = Array.length preds in
  match can_bootstrap meas ~resp ~preds with
  | Some err -> Error err
  | None ->
    let bootstrap_fails = ref 0 in
    let indices = Array.create ~len:(Measurement.sample_count meas) 0 in
    let bootstrap_coeffs =
      Array.init num_preds ~f:(fun _ -> Array.create ~len:trials 0.0)
    in
    (* Each bootstrap replication samples with replacement from the rows we have. *)
    for i = 0 to trials - 1 do
      random_indices_in_place indices ~max:(Measurement.sample_count meas);
      let matrix, vector = make_lr_inputs ~indices meas ~resp ~preds in
      match Linear_algebra.ols ~in_place:true matrix vector with
      (* If the run succeeded, save the coefficients. *)
      | Ok bt_ols_result ->
        for p = 0 to num_preds - 1 do
          bootstrap_coeffs.(p).(i) <- bt_ols_result.(p);
        done
      (* If the run failed, assume neg_infinity *)
      | _ ->
        incr bootstrap_fails;
        for p = 0 to num_preds - 1 do
          bootstrap_coeffs.(p).(i) <- Float.neg_infinity;
        done
    done;
    Ok (Array.init num_preds ~f:(fun i ->
      if trials = 0
      then Analysis_result.Ci95.bad_ci95
      else quantile_of_array (bootstrap_coeffs.(i))
             ~failures:!bootstrap_fails
             ~len:trials
             ~low_quantile:0.025
             ~high_quantile:0.975))
