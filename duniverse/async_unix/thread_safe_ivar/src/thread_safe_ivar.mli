(** A simple thread-safe ivar implementation. *)

open! Core
open! Import

type 'a t [@@deriving sexp_of]

val create : unit -> _ t
val fill : 'a t -> 'a -> unit

(** [read t] blocks until [t] is filled. *)
val read : 'a t -> 'a
