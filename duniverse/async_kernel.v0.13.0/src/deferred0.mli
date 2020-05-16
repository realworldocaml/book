(** Internal to Async -- see {!Deferred} for the public API. *)

open! Core_kernel
open! Import

type +'a t = 'a Types.Deferred.t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val of_ivar : 'a Ivar0.t -> 'a t
val create : ('a Ivar0.t -> unit) -> 'a t
val peek : 'a t -> 'a option
val value_exn : 'a t -> 'a
val is_determined : _ t -> bool
val return : 'a -> 'a t
val upon : 'a t -> ('a -> unit) -> unit
val bind : 'a t -> f:('a -> 'b t) -> 'b t

module Handler : sig
  type 'a t [@@deriving sexp_of]
end

val add_handler : 'a t -> ('a -> unit) -> Execution_context.t -> 'a Handler.t
val remove_handler : 'a t -> 'a Handler.t -> unit
