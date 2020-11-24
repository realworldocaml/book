type point_error =
  [ `Invalid_format
  | `Invalid_range
  | `Invalid_length
  | `Not_on_curve
  | `At_infinity ]

type scalar_error = [ `Invalid_length | `Invalid_range ]

val pp_point_error : Format.formatter -> point_error -> unit

val pp_scalar_error : Format.formatter -> scalar_error -> unit
