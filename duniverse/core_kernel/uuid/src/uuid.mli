(** Implements universally unique identifiers based on version 3 of the UUID
    specification.  Identifier generation is thread safe, and fast.
*)

open! Core

(** When [am_running_test], [sexp_of_t] masks the UUID, showing only
    "<uuid-omitted-in-test>". You can use [Unstable.sexp_of_t] if you definitely want to
    see it within your tests. *)
type t [@@deriving hash, sexp_of]

include Identifiable.S with type t := t
include Invariant.S with type t := t
include Quickcheckable.S with type t := t

val t_of_sexp : Sexp.t -> t
[@@deprecated "[since 2017-11] Use a [Stable] or [Unstable] [t_of_sexp]."]

val create_random : Random.State.t -> t
val arg_type : t Command.Arg_type.t

module Unstable : sig
  (** Unlike the toplevel [sexp_of_t], [Unstable.sexp_of_t] shows the uuid even when
      [am_running_test]. Unlike [Stable] deserializers, [Unstable.t_of_sexp] validates the
      input. *)
  type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]

  include Comparator.S with type t := t with type comparator_witness = comparator_witness
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving hash]

    include
      Stable_comparable.V1
      with type t := t
      with type comparator_witness = comparator_witness

    val for_testing : t
  end
end

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val is_valid_exn : t -> unit
  val nil : t
  val create : hostname:string -> pid:int -> t
  val bottom_4_bits_to_hex_char : int -> char
end
