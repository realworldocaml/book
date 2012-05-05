open Core.Std

module type S = sig
  type t
  type config

  val create : config -> Time.t -> t Or_error.t
  val log : t -> Time.t -> string -> unit
  val rotate_log  : t -> Time.t -> unit
end
