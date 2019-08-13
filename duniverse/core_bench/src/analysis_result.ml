open Core

module Ci95 = struct
  (* 95% confidence interval, stored as (left endpoint, right endpoint) *)
  type t = {
    left_endpoint  : float;
    right_endpoint : float
  } [@@deriving fields, sexp]


  let create ~left_endpoint ~right_endpoint = {
    left_endpoint; right_endpoint;
  }

  (* 95% confidence interval expressed in (absolute) error form.  E.g., if estimate =
     50. and confidence95 = (49., 52.), then [confidence95_abs_err] returns (-1., 2.).  *)
  let ci95_abs_err t ~estimate =
    (t.left_endpoint -. estimate, t.right_endpoint -. estimate)

  (* 95% confidence interval in relative error form (with 2.5 = 250%, etc.).  E.g., if
     estimate = 50. and confidence95 = (49., 52.), then [confidence95_rel_err] returns
     (-0.02, 0.04).  *)
  let ci95_rel_err t ~estimate =
    let (low, high) = ci95_abs_err t ~estimate in
    (low /. estimate, high /. estimate)

  let bad_ci95 = {
    left_endpoint = Float.neg_infinity;
    right_endpoint = Float.neg_infinity;
  }
end

module Coefficient = struct
  type t = {
    predictor    : Variable.t;
    estimate     : float;
    mutable ci95 : Ci95.t option;
  } [@@deriving fields, sexp]

  let has_ci95 t = Option.is_some t.ci95

  let create ~predictor ~estimate ?ci95 () = {
    predictor; estimate; ci95;
  }

  let has_non_trivial_estimate ~show_all_values t ~responder =
    let unit = Variable.get_units responder in
    Display_units.is_displayed ~show_all_values unit t.estimate

end

module Regression = struct
  type t = {
    responder       : Variable.t;
    r_square        : float option;
    coefficients    : Coefficient.t array;
    regression_name : string option;
    key             : int;
  } [@@deriving fields, sexp]

  let has_r_square t = Option.is_some t.r_square

  let predictors t =
    Array.map t.coefficients ~f:(fun c -> c.Coefficient.predictor)

  let make_key responder coefficients =
    let init = ((Variable.to_int responder) lsl Variable.max_int) in
    (Array.fold ~init coefficients ~f:(fun acc coeff ->
       (1 lsl (Variable.to_int (Coefficient.predictor coeff))) + acc))

  let create ~responder ?r_square ~coefficients ~regression_name () = {
    responder;
    r_square;
    coefficients;
    key = make_key responder coefficients;
    regression_name;
  }
end

type t = {
  name          : string;
  test_name     : string;
  file_name     : string;
  module_name   : string;
  sample_count  : int;
  largest_run   : int;
  regressions   : Regression.t array;
} [@@deriving fields, sexp]

let create ~name ~test_name ~file_name ~module_name ~sample_count ~largest_run ~regressions = {
  name; test_name; file_name; module_name; sample_count; largest_run; regressions;
}

let find_key t key =
  let index = ref (-1) in
  for i = 0 to Array.length t.regressions - 1 do
    if t.regressions.(i).Regression.key = key
    then index := i
  done;
  if !index = -1
  then None
  else Some (t.regressions.(!index))

