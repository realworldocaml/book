(** Option type. *)

open! Import

type 'a t = 'a option =
  | None
  | Some of 'a
[@@deriving_inline compare, hash, sexp, sexp_grammar]

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val hash_fold_t
  :  (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
  -> Ppx_hash_lib.Std.Hash.state
  -> 'a t
  -> Ppx_hash_lib.Std.Hash.state

include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

val t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t

[@@@end]

include Container.S1 with type 'a t := 'a t
include Equal.S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t

(** Options form a monad, where [return x = Some x], [(None >>= f) = None], and [(Some x
    >>= f) = f x]. *)
include
  Monad.S with type 'a t := 'a t

(** [is_none t] returns true iff [t = None]. *)
val is_none : 'a t -> bool

(** [is_some t] returns true iff [t = Some x]. *)
val is_some : 'a t -> bool

(** [value_map ~default ~f] is the same as [function Some x -> f x | None -> default]. *)
val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

(** [map2 o f] maps ['a option] and ['b option] to a ['c option] using [~f]. *)
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

(** [call x f] runs an optional function [~f] on the argument. *)
val call : 'a -> f:('a -> unit) t -> unit

(** [value None ~default] = [default]

    [value (Some x) ~default] = [x] *)
val value : 'a t -> default:'a -> 'a

(** [value_exn (Some x)] = [x].  [value_exn None] raises an error whose contents contain
    the supplied [~here], [~error], and [message], or a default message if none are
    supplied. *)
val value_exn
  :  ?here:Source_code_position0.t
  -> ?error:Error.t
  -> ?message:string
  -> 'a t
  -> 'a

val some : 'a -> 'a t
val both : 'a t -> 'b t -> ('a * 'b) t
val first_some : 'a t -> 'a t -> 'a t
val some_if : bool -> 'a -> 'a t

(** [merge a b ~f] merges together the values from [a] and [b] using [f].  If both [a] and
    [b] are [None], returns [None].  If only one is [Some], returns that one, and if both
    are [Some], returns [Some] of the result of applying [f] to the contents of [a] and
    [b]. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t

val filter : 'a t -> f:('a -> bool) -> 'a t

(** [try_with f] returns [Some x] if [f] returns [x] and [None] if [f] raises an
    exception.  See [Result.try_with] if you'd like to know which exception. *)
val try_with : (unit -> 'a) -> 'a t

(** [try_with_join f] returns the optional value returned by [f] if it exits normally, and
    [None] if [f] raises an exception. *)
val try_with_join : (unit -> 'a t) -> 'a t

val validate : none:unit Validate.check -> some:'a Validate.check -> 'a t Validate.check
