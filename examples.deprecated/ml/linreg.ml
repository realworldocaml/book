open Core.Std

module Predictor = struct
  type t = float array
end
module Responder = struct
  type t = float
end

module Config = struct
  type t = { num_predictors: int; } with sexp
end

module Model = struct
  type t = { covariance_matrix: Lacaml.D.mat }
  type config = Config.t
  type predictor = Predictor.t
  type responder = Responder.t

  let create _ = assert false
  let train  _ = assert false
end

module Predictive_model = struct
  type t = float array (** betas, one per predictor *)
  let create _model = assert false
  let predict t predictor =
    (* dot-product of predictor and betas *)
    Array.fold ~init:0. ~f:(+.)
      (Array.map2_exn t predictor ~f:( *. ))
end
