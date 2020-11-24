(** This module implements derived integer operations (e.g., modulo, rounding to
    multiples) based on other basic operations. *)

open! Import

module type Make_arg = sig
  type t

  include Floatable.S with type t := t
  include Stringable.S with type t := t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ~- ) : t -> t

  include Comparisons.Infix with type t := t

  val abs : t -> t
  val neg : t -> t
  val zero : t
  val of_int_exn : int -> t
  val rem : t -> t -> t
end

(** Derived operations common to various integer modules.

    See {{!Base.Int.S_common}[Int.S_common]} for a description of the operations derived
    by this module. *)
module Make (X : Make_arg) : sig
  val ( % ) : X.t -> X.t -> X.t
  val ( /% ) : X.t -> X.t -> X.t
  val ( // ) : X.t -> X.t -> float

  include Int_intf.Round with type t := X.t
end

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val int_pow : int -> int -> int
  val int64_pow : int64 -> int64 -> int64
  val int63_pow_on_int64 : int64 -> int64 -> int64

  module Pow_overflow_bounds = Pow_overflow_bounds
end
