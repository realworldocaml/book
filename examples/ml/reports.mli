open Core.Std

type float_array = float array

module Single_responder_sq_error :
  Mlearn.Report
  with type responder := float
  with type predictor := float_array
