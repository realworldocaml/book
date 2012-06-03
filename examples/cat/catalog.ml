open Core.Std

module type Cat = sig
  module Type : sig
    type 'a t
    val int : int t
    val float : float t
    val string : string t
  end

  val sub : 'a Type.t -> string -> ('a -> unit) -> unit Or_error.t
end

module Action = struct
  type 'a t = | A of 'a
              | B of int
              | C of string
end

module Update = struct
  type 'a t = | One of 'a
              | Two of string
end

module type Strat : sig
  type t
  type foo
  type bar
  type snoo

  val create :
end
