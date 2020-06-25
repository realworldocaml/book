type point_error =
  [ `Invalid_format
  | `Invalid_length
  | `Invalid_range
  | `Not_on_curve
  | `At_infinity ]

type scalar_error = [ `Invalid_length | `Invalid_range ]

let error_to_string = function
  | `Invalid_format -> "invalid format"
  | `Not_on_curve -> "point is not on curve"
  | `At_infinity -> "point is at infinity"
  | `Invalid_length -> "invalid length"
  | `Invalid_range -> "invalid range"

let pp_point_error fmt e =
  Format.fprintf fmt "Cannot parse point: %s" (error_to_string e)

let pp_scalar_error fmt e =
  Format.fprintf fmt "Cannot parse scalar: %s" (error_to_string e)
