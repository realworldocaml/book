open Core.Std

module Harness = Mlearn.Make_harness
  (Linreg.Model)
  (Linreg.Predictive_model)
  (Reports.Single_responder_sq_error)

let train_and_evaluate
    (config : Linreg.Config.t)
    (training :  (float array * float) array)
    (out_of_sample : (float array * float) array)
    =
  Harness.train_and_evaluate config ~training ~out_of_sample
