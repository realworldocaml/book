(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Explicit error handling

    @since 2.6.0 *)

(** This module provides helpers for values of type [('a, 'b) result Lwt.t].
    The module is experimental and may change in the future. *)

type (+'a, +'b) t = ('a, 'b) result Lwt.t

val return : 'a -> ('a, _) t

val fail : 'b -> (_, 'b) t

val lift : ('a, 'b) result -> ('a, 'b) t

val ok : 'a Lwt.t -> ('a, _) t

val error : 'b Lwt.t -> (_, 'b) t
(** @since 5.6.0  *)

val catch : 'a Lwt.t -> ('a, exn) t
(** [catch x] behaves like [return y] if [x] evaluates to [y],
    and like [fail e] if [x] raises [e] *)

val get_exn : ('a, exn) t -> 'a Lwt.t
(** [get_exn] is the opposite of {!catch}: it unwraps the result type,
    returning the value in case of success, calls {!Lwt.fail} in
    case of error. *)

val map : ('a -> 'b) -> ('a,'e) t -> ('b,'e) t

val map_error : ('e1 -> 'e2) -> ('a,'e1) t -> ('a,'e2) t
(** @since 5.6.0 *)

val bind : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t

val bind_error : ('a,'e1) t -> ('e1 -> ('a,'e2) t) -> ('a,'e2) t
(** @since 5.6.0 *)

val bind_lwt : ('a,'e) t -> ('a -> 'b Lwt.t) -> ('b,'e) t

val bind_lwt_error : ('a,'e1) t -> ('e1 -> 'e2 Lwt.t) -> ('a,'e2) t
(** @since 5.6.0 *)

val bind_result : ('a,'e) t -> ('a -> ('b,'e) result) -> ('b,'e) t

val both : ('a,'e) t -> ('b,'e) t -> ('a * 'b,'e) t
(** [Lwt.both p_1 p_2] returns a promise that is pending until {e both} promises
    [p_1] and [p_2] become {e resolved}.
    If only [p_1] is [Error e], the promise is resolved with [Error e],
    If only [p_2] is [Error e], the promise is resolved with [Error e],
    If both [p_1] and [p_2] resolve with [Error _], the promise is resolved with
    the error that occurred first. *)

val iter : ('a -> unit Lwt.t) -> ('a, 'e) t -> unit Lwt.t
(** [iter f r] is [f v] if [r] is a promise resolved with [Ok v], and
    {!Lwt.return_unit} otherwise.

    @since Lwt 5.6.0
*)

val iter_error : ('e -> unit Lwt.t) -> ('a, 'e) t -> unit Lwt.t
(** [iter_error f r] is [f v] if [r] is a promise resolved with [Error v],
    and {!Lwt.return_unit} otherwise.

    @since Lwt 5.6.0
*)

module Infix : sig
  val (>|=) : ('a,'e) t -> ('a -> 'b) -> ('b,'e) t
  val (>>=) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
end

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> ('a, _) t
    (** See {!Lwt_result.return}. *)

    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    (** See {!Lwt_result.map}. *)

    val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
    (** See {!Lwt_result.bind}. *)

    val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
    (** See {!Lwt_result.both}. *)

    module Open_on_rhs : sig
    end
  end
end

(** {3 Let syntax} *)
module Syntax : sig

  (** {1 Monadic syntax} *)

  val (let*) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
  (** Syntax for {!bind}. *)

  val (and*) : ('a,'e) t -> ('b,'e) t -> ('a * 'b,'e) t
  (** Syntax for {!both}. *)

  (** {1 Applicative syntax} *)

  val (let+) : ('a,'e) t -> ('a -> 'b) -> ('b, 'e) t
  (** Syntax for {!map}. *)

  val (and+) : ('a,'e) t -> ('b,'e) t -> ('a * 'b,'e) t
  (** Syntax for {!both}. *)
end

include module type of Infix

(** {3 Deprecated} *)

val map_err : ('e1 -> 'e2) -> ('a,'e1) t -> ('a,'e2) t [@@deprecated "Alias to map_error"]
(** @deprecated Alias to [map_error] since 5.6.0. *)

val bind_lwt_err : ('a,'e1) t -> ('e1 -> 'e2 Lwt.t) -> ('a,'e2) t [@@deprecated "Alias to bind_lwt_error"]
(** @deprecated Alias to [bind_lwt_error] since 5.6.0. *)
