type vec = { x:float; y:float }

module type IMAGE = sig
  type t
  type config

  module Config : sig
    type t = config with sexp
  end

  module Color : sig
    type t

    val of_rgb : r:int -> g:int -> b:int -> t Or_error.t
  end

  val create      : Config.t -> t Or_error.t
  val dim         : t -> vec

  val draw_circle
    : t -> Color.t -> vec -> float -> unit

  val draw_rect
    : t -> Color.t -> [`Fill | `Outline of float] -> vec -> vec -> unit

  val overlay : t list -> t
  val hbox : t list -> t
  val vbox : t list -> t
end
