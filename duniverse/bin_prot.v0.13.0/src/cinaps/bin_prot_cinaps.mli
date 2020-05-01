open! Base
open! Stdio

module Code : sig
  type t =
    | NEG_INT8
    | INT16
    | INT32
    | INT64

  val char : t -> unit
end

module Sig : sig
  val mk_base_tp : string -> string -> unit
  val mk_base : string -> unit
  val mk_base1_tp : string -> string -> unit
  val mk_base1 : string -> unit
  val mk_base2_tp : string -> string -> unit
  val mk_base2 : string -> unit
end

module Str : sig
  val mk_base : string -> unit
  val mk_base1 : string -> unit
  val mk_base2 : string -> unit
  val mk_base3 : string -> unit
end
