open Core.Std

module Config : sig
  type t = { num_predictors: int } with sexp
end

module Predictor : sig
  type t = float array
end

module Responder : sig
  type t = float
end

module Model :
  Mlearn.Model
  with type predictor := Predictor.t
  and  type responder := Responder.t
  and  type config    := Config.t

module Predictive_model :
  Mlearn.Predictive_model
  with type model     := Model.t
  and  type predictor := Predictor.t
  and  type responder := Responder.t


