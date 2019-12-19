(** Various combinators for functions. *)

open! Import

(** A "pipe" operator. *)
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(** Produces a function that just returns its first argument. *)
val const : 'a -> _ -> 'a

(** [ignore] is the same as [Caml.ignore].  It is useful to have here so that code
    that rebinds [ignore] can still refer to [Fn.ignore]. *)
external ignore : _ -> unit = "%ignore"

(** Negates a boolean function. *)
val non : ('a -> bool) -> 'a -> bool

(** [forever f] runs [f ()] until it throws an exception and returns the
    exception. This function is useful for read_line loops, etc. *)
val forever : (unit -> unit) -> exn

(** [apply_n_times ~n f x] is the [n]-fold application of [f] to [x]. *)
val apply_n_times : n:int -> ('a -> 'a) -> 'a -> 'a

(** The identity function.

    See also: {!Sys.opaque_identity}. *)
external id : 'a -> 'a = "%identity"

(** [compose f g x] is [f (g x)]. *)
val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** Reverses the order of arguments for a binary function. *)
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
