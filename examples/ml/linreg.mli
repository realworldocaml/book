module Config : sig
  type t = { num_predictors: int; }
  with sexp
end

include (Mlearn.MODEL
         with type predictor = float array
         and type responder = float
         and type config = Config.t)

