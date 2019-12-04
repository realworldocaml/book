(** This module is not exposed in Core.  Instead, these functions are accessed and
    commented in the various Core modules implementing [Int_intf.S]. *)

open! Import

(*_ This interface is not defined in int_intf.ml because we don't want users of Core to
  think about it. *)
module type T = sig
  type t
  include Floatable.S  with type t := t
  include Stringable.S with type t := t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ~- ) : t -> t
  include Comparisons.Infix with type t := t

  val abs    : t -> t
  val neg    : t -> t
  val zero   : t
  val of_int_exn : int -> t

  val rem : t -> t -> t
end

(** derived operations common to various integer modules *)
module Make (X : T) : sig
  val ( %  ) : X.t -> X.t -> X.t
  val ( /% ) : X.t -> X.t -> X.t
  val ( // ) : X.t -> X.t -> float
  include Int_intf.Round with type t := X.t
end

val int_pow   : int   -> int   -> int
val int64_pow : int64 -> int64 -> int64

val int63_pow_on_int64 : int64 -> int64 -> int64
