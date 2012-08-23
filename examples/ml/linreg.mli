module Config : sig
  type t = { num_predictors: int } with sexp
end

module Model :
  Mlearn.MODEL
  with type predictor = float array
  and  type responder = float
  and  type config    = Config.t

module Predictive_model :
  Mlearn.PREDICTIVE_MODEL
  with type model     := Model.t
  and  type predictor := Model.predictor
  and  type responder := Model.responder


