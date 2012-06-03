open Core.Std

module Config = struct
  type t = { num_predictors: int; }
  with sexp
end

module Model = struct

  type t = { covariance_matrix: float array array }
  type config = Config.t
  type predictor = float array
  type responder = float

  let create _ = assert false
  let train _ = assert false
end

module Predictive_model = struct
  type t = float array (** betas, one per predictor *)
  let create _model  = [||]
  let predict t predictor =
    (* dot-product of predictor and betas *)
    Array.fold ~init:0. ~f:(+.)
      (Array.map2 t predictor ~f:(fun x y -> x *. y))
end
