open Core.Std

module type S = sig
  type t

  val empty : t
  val touch : t -> string -> t
  val to_list : t -> (string * int) list
end
