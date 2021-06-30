
(** A value of type ['a Lazy.t] is a deferred computation, called a suspension, that has a
    result of type ['a].

    The special expression syntax [lazy (expr)] makes a suspension of the computation of
    [expr], without computing [expr] itself yet.  "Forcing" the suspension will then
    compute [expr] and return its result.

    Note: [lazy_t] is the built-in type constructor used by the compiler for the [lazy]
    keyword.  You should not use it directly.  Always use [Lazy.t] instead.

    Note: [Lazy.force] is not thread-safe.  If you use this module in a multi-threaded
    program, you will need to add some locks.

    Note: if the program is compiled with the [-rectypes] option, ill-founded recursive
    definitions of the form [let rec x = lazy x] or [let rec x = lazy(lazy(...(lazy x)))]
    are accepted by the type-checker and lead, when forced, to ill-formed values that
    trigger infinite loops in the garbage collector and other parts of the run-time
    system.  Without the [-rectypes] option, such ill-founded recursive definitions are
    rejected by the type-checker. *)

open! Import

type 'a t = 'a lazy_t [@@deriving_inline compare, hash, sexp]

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val hash_fold_t
  :  (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
  -> Ppx_hash_lib.Std.Hash.state
  -> 'a t
  -> Ppx_hash_lib.Std.Hash.state

include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

[@@@end]

include Monad.S with type 'a t := 'a t

exception Undefined

(** [force x] forces the suspension [x] and returns its result. If [x] has already been
    forced, [Lazy.force x] returns the same value again without recomputing it. If it
    raised an exception, the same exception is raised again. Raise [Undefined] if the
    forcing of [x] tries to force [x] itself recursively. *)
external force : 'a t -> 'a = "%lazy_force"

(** Like [force] except that [force_val x] does not use an exception handler, so it may be
    more efficient.  However, if the computation of [x] raises an exception, it is
    unspecified whether [force_val x] raises the same exception or [Undefined]. *)
val force_val : 'a t -> 'a

(** [from_fun f] is the same as [lazy (f ())] but slightly more efficient if [f] is a
    variable.  [from_fun] should only be used if the function [f] is already defined.  In
    particular it is always less efficient to write [from_fun (fun () -> expr)] than [lazy
    expr]. *)
val from_fun : (unit -> 'a) -> 'a t

(** [from_val v] returns an already-forced suspension of [v] (where [v] can be any
    expression).  Essentially, [from_val expr] is the same as [let var = expr in lazy
    var]. *)
val from_val : 'a -> 'a t

(** [is_val x] returns [true] if [x] has already been forced and did not raise an
    exception. *)
val is_val : 'a t -> bool

(** This type offers a serialization function [sexp_of_t] that won't force its argument.
    Instead, it will serialize the ['a] if it is available, or just use a custom string
    indicating it is not forced. Note that this is not a round-trippable type, thus the
    type does not expose [of_sexp]. To be used in debug code, while tracking a Heisenbug,
    etc. *)
module T_unforcing : sig
  type nonrec 'a t = 'a t [@@deriving_inline sexp_of]

  val sexp_of_t : ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t

  [@@@end]
end
