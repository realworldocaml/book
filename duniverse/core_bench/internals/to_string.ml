open Core

let float_to_string = Float.to_string_hum ~decimals:2

let float_opt_to_string = function
  | Some v -> float_to_string v
  | None -> ""
;;
