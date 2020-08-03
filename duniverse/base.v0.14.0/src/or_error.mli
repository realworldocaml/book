(** Type for tracking errors in an [Error.t]. This is a specialization of the [Result]
    type, where the [Error] constructor carries an [Error.t].

    A common idiom is to wrap a function that is not implemented on all platforms, e.g.,

    {[val do_something_linux_specific : (unit -> unit) Or_error.t]}
*)

open! Import

(** Serialization and comparison of an [Error] force the error's lazy message. *)
type 'a t = ('a, Error.t) Result.t [@@deriving_inline compare, equal, hash, sexp]

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val hash_fold_t
  :  (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
  -> Ppx_hash_lib.Std.Hash.state
  -> 'a t
  -> Ppx_hash_lib.Std.Hash.state

include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

[@@@end]

(** [Applicative] functions don't have quite the same semantics as
    [Applicative.Of_Monad(Or_error)] would give -- [apply (Error e1) (Error e2)] returns
    the combination of [e1] and [e2], whereas it would only return [e1] if it were defined
    using [bind]. *)
include
  Applicative.S with type 'a t := 'a t

include Invariant.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val is_ok : _ t -> bool
val is_error : _ t -> bool

(** [try_with f] catches exceptions thrown by [f] and returns them in the [Result.t] as an
    [Error.t].  [try_with_join] is like [try_with], except that [f] can throw exceptions
    or return an [Error] directly, without ending up with a nested error; it is equivalent
    to [Result.join (try_with f)]. *)
val try_with : ?backtrace:bool (** defaults to [false] *) -> (unit -> 'a) -> 'a t

val try_with_join : ?backtrace:bool (** defaults to [false] *) -> (unit -> 'a t) -> 'a t

(** [ok t] returns [None] if [t] is an [Error], and otherwise returns the contents of the
    [Ok] constructor. *)
val ok : 'ok t -> 'ok option

(** [ok_exn t] throws an exception if [t] is an [Error], and otherwise returns the
    contents of the [Ok] constructor. *)
val ok_exn : 'a t -> 'a

(** [of_exn ?backtrace exn] is [Error (Error.of_exn ?backtrace exn)]. *)
val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> _ t

(** [of_exn_result ?backtrace (Ok a) = Ok a]

    [of_exn_result ?backtrace (Error exn) = of_exn ?backtrace exn] *)
val of_exn_result : ?backtrace:[ `Get | `This of string ] -> ('a, exn) Result.t -> 'a t

(** [error] is a wrapper around [Error.create]:

    {[
      error ?strict message a sexp_of_a
      = Error (Error.create ?strict message a sexp_of_a)
    ]}

    As with [Error.create], [sexp_of_a a] is lazily computed when the info is converted
    to a sexp.  So, if [a] is mutated in the time between the call to [create] and the
    sexp conversion, those mutations will be reflected in the sexp.  Use [~strict:()] to
    force [sexp_of_a a] to be computed immediately. *)
val error : ?strict:unit -> string -> 'a -> ('a -> Sexp.t) -> _ t

val error_s : Sexp.t -> _ t

(** [error_string message] is [Error (Error.of_string message)]. *)
val error_string : string -> _ t

(** [errorf format arg1 arg2 ...] is [Error (sprintf format arg1 arg2 ...)].  Note that it
    calculates the string eagerly, so when performance matters you may want to use [error]
    instead. *)
val errorf : ('a, unit, string, _ t) format4 -> 'a

(** [tag t ~tag] is [Result.map_error t ~f:(Error.tag ~tag)].
    [tag_arg] is similar. *)
val tag : 'a t -> tag:string -> 'a t

val tag_s : 'a t -> tag:Sexp.t -> 'a t
val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t

(** For marking a given value as unimplemented.  Typically combined with conditional
    compilation, where on some platforms the function is defined normally, and on some
    platforms it is defined as unimplemented.  The supplied string should be the name of
    the function that is unimplemented. *)
val unimplemented : string -> _ t

val map : 'a t -> f:('a -> 'b) -> 'b t
val iter : 'a t -> f:('a -> unit) -> unit
val iter_error : _ t -> f:(Error.t -> unit) -> unit

(** [combine_errors ts] returns [Ok] if every element in [ts] is [Ok], else it returns
    [Error] with all the errors in [ts].  More precisely:

    - [combine_errors [Ok a1; ...; Ok an] = Ok [a1; ...; an]]
    - {[ combine_errors [...; Error e1; ...; Error en; ...]
         = Error (Error.of_list [e1; ...; en]) ]} *)
val combine_errors : 'a t list -> 'a list t

(** [combine_errors_unit ts] returns [Ok] if every element in [ts] is [Ok ()], else it
    returns [Error] with all the errors in [ts], like [combine_errors]. *)
val combine_errors_unit : unit t list -> unit t

(** [filter_ok_at_least_one ts] returns all values in [ts] that are [Ok] if there is at
    least one, otherwise it returns the same error as [combine_errors ts]. *)
val filter_ok_at_least_one : 'a t list -> 'a list t

(** [find_ok ts] returns the first value in [ts] that is [Ok], otherwise it returns the
    same error as [combine_errors ts]. *)
val find_ok : 'a t list -> 'a t

(** [find_map_ok l ~f] returns the first value in [l] for which [f] returns [Ok],
    otherwise it returns the same error as [combine_errors (List.map l ~f)]. *)
val find_map_ok : 'a list -> f:('a -> 'b t) -> 'b t
