(** The deferred analog of [Core.Or_error].  It is exposed in std.ml as
    [Deferred.Or_error].

    The mental model for a function returning an ['a Deferred.Or_error.t] is that the
    function never raises.  All error cases are caught and expressed as an [Error _]
    result.  This module preserves that property.

    Unfortunately, there is no way to enforce this property using the type system, so it
    is more like a convention, or idiom.  A function whose type ends with [... -> 'a
    Deferred.Or_error.t] and still raises should be considered broken, and be fixed.  With
    that property in mind, [Deferred.Or_error.List.iter], for example, does not wrap the
    execution of the given iter function [f] inside a monitor.  If one of these
    application raises, the whole function [Deferred.Or_error.List.iter] will raise as a
    way to try to alert the developer that the function is broken and needs attention
    and fixing, rather than silently catching the error and converting it to
    [Or_error.Error].

    This behavior is consistent with [Core.Or_error]'s treatment of user-supplied
    functions.

    If you have to deal with a function that does not respect this idiom, you can use
    [Deferred.Or_error.try_with_join] to wrap its execution and enforce this property. *)

open! Core_kernel
open! Import
module Deferred = Deferred1

type 'a t = 'a Or_error.t Deferred.t

(** The applicative operations match the behavior of the applicative operations in
    [Or_error].  This means that [all] and [all_unit] are equivalent to [combine_errors]
    and [combine_errors_unit] respectively. *)
include
  Applicative.S with type 'a t := 'a t

(** [return x = Deferred.return (Ok x)] **)
include Monad.S with type 'a t := 'a t

(** [fail error = Deferred.return (Error error)] **)
val fail : Error.t -> _ t

val ignore : _ t -> unit t [@@deprecated "[since 2019-06] Use [ignore_m] instead"]

(** These functions are direct analogs of the corresponding [Core.Or_error] functions. *)
val ok_exn : 'a t -> 'a Deferred.t

val of_exn : exn -> _ t
val of_exn_result : ('a, exn) Result.t Deferred.t -> 'a t
val error : string -> 'a -> ('a -> Sexp.t) -> _ t
val error_s : Sexp.t -> _ t
val error_string : string -> _ t
val errorf : ('a, unit, string, _ t) format4 -> 'a
val tag : 'a t -> tag:string -> 'a t
val tag_s : 'a t -> tag:Sexp.t -> 'a t
val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t
val unimplemented : string -> _ t


val combine_errors : 'a t list -> 'a list t
val combine_errors_unit : unit t list -> unit t
val filter_ok_at_least_one : 'a t list -> 'a list t

(** [find_map_ok l ~f] returns the first value in [l] for which [f] returns [Ok],
    otherwise it returns the same error as [combine_errors (Deferred.List.map l ~f)]. *)
val find_map_ok : 'a list -> f:('a -> 'b t) -> 'b t

(** [ok_unit = return ()] *)
val ok_unit : unit t

(** [try_with f] catches exceptions thrown by [f] and returns them in the Result.t as an
    Error.t.  [try_with_join] is like [try_with], except that [f] can throw exceptions or
    return an [Error] directly, without ending up with a nested error; it is equivalent to
    [try_with f >>| Result.join].

    The option [extract_exn] is passed along to [Monitor.try_with ?extract_exn] and
    specifies whether or not the monitor exn wrapper should be skipped ([extract_exn:true]
    or kept ([extract_exn:false]). *)
val try_with
  :  ?extract_exn:bool (** default is [false] *)
  -> ?run:[ `Now | `Schedule ] (** default is [`Schedule] *)
  -> ?here:Lexing.position
  -> ?name:string
  -> (unit -> 'a Deferred.t)
  -> 'a t

val try_with_join
  :  ?extract_exn:bool (** default is [false] *)
  -> ?run:[ `Now | `Schedule ] (** default is [`Schedule] *)
  -> ?here:Lexing.position
  -> ?name:string
  -> (unit -> 'a t)
  -> 'a t

(** All of the [List] functions that take a [how] argument treat it the following way:

    [`Sequential] indicates both sequential evaluation of the deferreds, and sequential
    combination of the results.

    [`Parallel] indicates parallel evaluation of the deferreds (in the sense that they are
    all in the scheduler at the same time), and parallel combination of the results. For
    example, [List.iter ~how:`Parallel l ~f] will call [f] on each element of [l],
    creating all of the deferreds, then wait for _all_ of them to finish, then combine any
    errors (as in [Or_error.combine_errors_unit]).

    [`Max_concurrent_jobs n] acts like [`Parallel] in the way it combines the results, but
    only evaluates [n] of the deferreds at a time. *)
module List : Monad_sequence.S with type 'a monad := 'a t with type 'a t := 'a list

(** [repeat_until_finished initial_state f] works the just like
    {!Deferred.repeat_until_finished} but with the [Deferred.Or_error] monad.
    If [f] returns an [Or_error.Error] the loop terminates and returns. *)
val repeat_until_finished
  :  'state
  -> ('state -> [ `Repeat of 'state | `Finished of 'result ] t)
  -> 'result t
