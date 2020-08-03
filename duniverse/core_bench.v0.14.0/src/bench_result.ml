(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core
open Poly

module Coefficient = Analysis_result.Coefficient
module Ci95        = Analysis_result.Ci95

module Predictor_result = struct
  type t = {
    pred        : Variable.t;
    coefficient : Coefficient.t;
  } [@@deriving fields, sexp]

  let estimate t = Coefficient.estimate t.coefficient
  let ci95 t = Coefficient.ci95 t.coefficient
end

(* This holds the result from a running a benchmark. It is returned by [get_benchmarks]
   and is used in benchmarks.sexp to hold the results for an individual benchmark *)
type t = {
  name      : string;
  key       : int;
  group_key : int option;
  arg       : int option;

  (* This contains a list of predictors together with the R^2 value from the regression *)
  cycles : (Predictor_result.t list) * float;
  nanos  : (Predictor_result.t list) * float;

  minor_allocated : float;
  major_allocated : float;
  promoted        : float;
  minor_gcs       : float;
  major_gcs       : float;
  compactions     : float;
  samples         : string;
  percentage      : float;
  speedup         : float;
  time_taken      : Time.Span.t;
} [@@deriving fields, sexp]


(* These are here for convenient access *)
let nanos_coeff t =
  List.find_exn (fst t.nanos) ~f:(fun pred -> (Predictor_result.pred pred)=`Runs)

let nanos_est t =
  Predictor_result.estimate (nanos_coeff t)

let cycles_coeff t =
  List.find_exn (fst t.cycles) ~f:(fun pred -> (Predictor_result.pred pred)=`Runs)

let cycles_est t =
  Predictor_result.estimate (cycles_coeff t)
