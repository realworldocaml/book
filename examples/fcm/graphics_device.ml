type point = { x:float; y:float }

module type Device = sig
  type t
  type config with sexp
  type color

  module Color : sig
    type t = color

    val of_rgb : r:int -> g:int -> b:int -> t Or_error.t
    val red    : t
    val green  : t
    val blue   : t
    val orange : t
    val yellow : t
    val purple : t
  end

  val create      : config -> t Or_error.t
  val dim         : t -> point
  val draw_circle : t -> point -> color -> radius : float -> unit
  val draw_rect   : t -> point -> color -> point -> unit
  val draw_spline : t -> ....
end
