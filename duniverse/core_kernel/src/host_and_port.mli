(** Type for the commonly-used notion of host and port in networking. *)

open Std_internal

type t =
  { host : string
  ; port : int
  }
[@@deriving hash]

val create : host:string -> port:int -> t
val host : t -> string
val port : t -> int


val tuple : t -> string * int

include Identifiable with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare, hash]
  end
end

val type_id : t Type_equal.Id.t
