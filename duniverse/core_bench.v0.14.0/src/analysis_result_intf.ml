open! Core

(** Results of a benchmark analysis, including all the regressions. *)
module type Analysis_result = sig

  (** 95% confidence interval, stored as (left endpoint, right endpoint) *)
  module Ci95 : sig
    type t [@@deriving sexp]

    val left_endpoint  : t -> float
    val right_endpoint : t -> float

    val create : left_endpoint:float -> right_endpoint:float -> t

    (** 95% confidence interval expressed in (absolute) error form.  E.g., if estimate =
        50. and confidence95 = (49., 52.), then [confidence95_abs_err] returns (-1., 2.).
    *)
    val ci95_abs_err : t -> estimate:float -> float * float

    (** 95% confidence interval in relative error form (with 2.5 = 250%, etc.).  E.g., if
        estimate = 50. and confidence95 = (49., 52.), then [confidence95_rel_err] returns
        (-0.02, 0.04).  *)
    val ci95_rel_err : t -> estimate:float -> float * float

    val bad_ci95 : t
  end

  module Coefficient : sig
    type t [@@deriving sexp]

    val predictor : t -> Variable.t
    val estimate  : t -> float
    val set_ci95  : t -> Ci95.t option -> unit
    val ci95      : t -> Ci95.t option
    val has_ci95  : t -> bool

    val has_non_trivial_estimate
      :  show_all_values : bool
      -> t
      -> responder : Variable.t
      -> bool

    val create
      :  predictor : Variable.t
      -> estimate  : float
      -> ?ci95     : Ci95.t
      -> unit
      -> t
  end


  module Regression : sig
    type t

    val responder       : t -> Variable.t
    val r_square        : t -> float option
    val has_r_square    : t -> bool
    val coefficients    : t -> Coefficient.t array
    val key             : t -> int
    val predictors      : t -> Variable.t array
    val regression_name : t -> string option

    val create
      :  responder       : Variable.t
      -> ?r_square       : float
      -> coefficients    : Coefficient.t array
      -> regression_name : string option
      -> unit
      -> t
  end

  type t [@@deriving sexp]

  val name         : t -> string
  val test_name    : t -> string
  val file_name    : t -> string
  val module_name  : t -> string
  val sample_count : t -> int
  val largest_run  : t -> int
  val regressions  : t -> Regression.t array

  val create
    :  name         : string
    -> test_name    : string
    -> file_name    : string
    -> module_name  : string
    -> sample_count : int
    -> largest_run  : int
    -> regressions  : Regression.t array
    -> t

  val find_key : t -> int -> Regression.t option
end
