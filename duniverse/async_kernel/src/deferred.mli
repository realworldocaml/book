(** A value that will become determined asynchronously.

    A deferred can be "undetermined" or "determined".  A deferred that is undetermined may
    at some point become determined with value v, and will henceforth always be determined
    with value v. *)

open! Core_kernel
open! Import

type +'a t = 'a Deferred1.t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

(** [sexp_of_t t f] returns a sexp of the deferred's value, if it is determined, or an
    informative string otherwise.

    This is just for display purposes.  There is no [t_of_sexp]. *)

(** [create f] calls [f i], where [i] is an empty ivar.  [create] returns a deferred that
    becomes determined when [f] fills [i]. *)
val create : ('a Ivar.t -> unit) -> 'a t

(** [upon t f] will run [f v] at some point after [t] becomes determined with value
    [v]. *)
val upon : 'a t -> ('a -> unit) -> unit

(** [peek t] returns [Some v] iff [t] is determined with value [v]. *)
val peek : 'a t -> 'a option

(** [value_exn t] returns [v] if [t] is determined with value [v], and raises
    otherwise. *)
val value_exn : 'a t -> 'a

(** [is_determined t] returns [true] iff [t] is determined. *)
val is_determined : 'a t -> bool

(** Deferreds form a monad.

    [let%bind v = t in f v] returns a deferred [t'] that waits until [t] is determined
    with value [v], at which point it waits for [f v] to become determined with value
    [v'], to which [t'] will become determined.

    [return v] returns a deferred that is immediately determined with value v.

    Note that:

    {[ upon t f ]}

    is more efficient than:

    {[ ignore (let%bind a = t in f a; return ()) ]}

    because [upon], unlike [let%bind], does not create a deferred to hold the result.

    For example, one can write a loop that has good constant factors with:

    {[
      let rec loop () =
        upon t (fun a -> ... loop () ... ) ]}

    although often [forever] or [repeat_until_finished] is more clear.

    The same loop written with [let%bind] would allocate deferreds that would be
    immediately garbage collected.  (In the past, this loop would have also used linear
    space in recursion depth!)

    In general, for deferreds that are allocated by [let%bind] to be garbage collected
    quickly, it is sufficient that the allocating bind be executed in tail-call position
    of the right-hand side of an outer bind. *)
include
  Monad with type 'a t := 'a t

module Infix : sig
  include Monad.Infix with type 'a t := 'a t


  val ( >>> ) : 'a t -> ('a -> unit) -> unit
end

(** [unit] is a deferred that is always determined with value [()] *)
val unit : unit t

val ignore : _ t -> unit t [@@deprecated "[since 2019-06] Use [ignore_m] instead"]

(** [never ()] returns a deferred that never becomes determined. *)
val never : unit -> _ t

(** [both t1 t2] becomes determined after both [t1] and [t2] become determined. *)
val both : 'a t -> 'b t -> ('a * 'b) t

(** [all ts] returns a deferred that becomes determined when every [t] in [t]s is
    determined.  The output is in the same order as the input. *)
val all : 'a t list -> 'a list t

(** Like [all], but ignores results of the component deferreds. *)
val all_unit : unit t list -> unit t

(** [any ts] returns a deferred that is determined when any of the underlying deferreds is
    determined. *)
val any : 'a t list -> 'a t

(** [any_unit] is like [any], but ignores results of the component deferreds. *)
val any_unit : unit t list -> unit t

(** [don't_wait_for t] ignores [t].  It is like [Fn.ignore], but is more constrained
    because it requires a [unit Deferred.t].

    Rather than [ignore (t : _ t)], do [don't_wait_for (Deferred.ignore t)].

    We chose to give [don't_wait_for] type [unit t] rather than [_ t] to catch errors
    where a value is accidentally ignored. *)
val don't_wait_for : unit t -> unit

(** A [Choice.t] is used to produce an argument to [enabled] or [choose].  See below. *)
module Choice : sig
  type +'a t = 'a Deferred1.choice

  val map : 'a t -> f:('a -> 'b) -> 'b t
end


type 'a choice = 'a Choice.t

val choice : 'a t -> ('a -> 'b) -> 'b Choice.t

(** [enabled [choice t1 f1; ... choice tn fn;]] returns a deferred [d] that becomes
    determined when any of the [ti] becomes determined. The value of [d] is a function
    [f] that when called, for each [ti] that is enabled, applies [fi] to [ti], and returns
    a list of the results. It is guaranteed that the list is in the same order as the
    choices supplied to [enabled], but of course it may be shorter than the input list if
    not all [ti] are determined. *)
val enabled : 'b Choice.t list -> (unit -> 'b list) t

(**
   {[
     choose [ choice t1 f1
            ; ...
              ; choice tn fn ] ]}

   returns a deferred [t] that becomes determined with value [fi ai] after some [ti]
   becomes determined with value [ai].  It is guaranteed that [choose] calls at most one
   of the [fi]s, the one that determines its result.  There is no guarantee
   that the [ti] that becomes determined earliest in time will be the one whose value
   determines the [choose].  Nor is it guaranteed that the value in [t] is the first value
   (in place order) from [choices] that is determined at the time [t] is examined.

   For example, in:

   {[
     choose [ choice t1 (fun () -> `X1)
            ; choice t2 (fun () -> `X2) ]
     >>> function
     | `X1 -> e1
     | `X2 -> e2 ]}

   it may be the case that both [t1] and [t2] become determined, yet [e2] actually runs.

   It is guaranteed that if multiple choices are determined with no intervening
   asynchrony, then the earliest choice in the list will become the value of the
   [choose]. *)
val choose : 'b Choice.t list -> 'b t

(** [for_ start ~to_:stop ~do_:f] is the deferred analog of:

    {[
      for i = start to stop do
        f i;
      done ]} *)
val for_ : int -> to_:int -> do_:(int -> unit t) -> unit t

(** [repeat_until_finished initial_state f] repeatedly runs [f] until [f] returns
    [`Finished].  The first call to [f] happens immediately when [repeat_until_finished]
    is called. *)
val repeat_until_finished
  :  'state
  -> ('state -> [ `Repeat of 'state | `Finished of 'result ] t)
  -> 'result t

(** [forever initial_state f] repeatedly runs [f], supplying the state returned to the
    next call to [f]. *)
val forever : 'state -> ('state -> 'state t) -> unit

(** Useful for lifting values from the [Deferred.t] monad to the [Result.t Deferred.t]
    monad. *)
val ok : 'a t -> ('a, _) Core_kernel.Result.t t

(** {2 Deferred collections}

    These contain operations for iterating in a deferred manner over different
    collection types. *)

module Array = Deferred_array
module List = Deferred_list
module Map = Deferred_map
module Memo = Deferred_memo
module Queue = Deferred_queue
module Sequence = Deferred_sequence

(** {2 Error-carrying deferreds}

    These contain interfaces for working with deferred type containing error-aware types,
    like ['a Option.t Deferred.t], or ['a Or_error.t Deferred.t].  These all include
    support for monadic programming. *)

module Option = Deferred_option
module Or_error = Deferred_or_error
module Result = Deferred_result
