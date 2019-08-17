(** An uninhabited type. This is useful when interfaces require that a type be specified,
    but the implementer knows this type will not be used in their implementation of the
    interface.

    For instance, [Async.Rpc.Pipe_rpc.t] is parameterized by an error type, but a user
    may want to define a Pipe RPC that can't fail. *)

open! Import

(** Having [[@@deriving enumerate]] may seem strange due to the fact that generated
    [val all : t list] is the empty list, so it seems like it could be of no use.

    This may be true if you always expect your type to be [Nothing.t], but [[@@deriving
    enumerate]] can be useful if you have a type which you expect to change over time.
    For example, you may have a program which has to interact with multiple servers which
    are possibly at different versions.  It may be useful in this program to have a
    variant type which enumerates the ways in which the servers may differ.  When all the
    servers are at the same version, you can change this type to [Nothing.t] and code
    which uses an enumeration of the type will continue to work correctly.

    This is a similar issue to the identifiability of [Nothing.t].  As discussed below,
    another case where [[@deriving enumerate]] could be useful is when this type is part
    of some larger type.
*)
type t = (unit, int) Typerep_lib.Std.Type_equal.t [@@deriving hash, enumerate]

(** Because there are no values of type [Nothing.t], a piece of code that has a value of
    type [Nothing.t] must be unreachable.  In such an unreachable piece of code, one can
    use [unreachable_code] to give the code whatever type one needs.  For example:

    {[
      let f (r : (int, Nothing.t) Result.t) : int =
        match r with
        | Ok i -> i
        | Error n -> Nothing.unreachable_code n
      ;;
    ]}

    Note that the compiler knows that [Nothing.t] is uninhabited, hence this will type
    without warning:

    {[
      let f (Ok i : (int, Nothing.t) Result.t) = i
    ]}
*)
val unreachable_code : t -> _

(** It may seem weird that this is identifiable, but we're just trying to anticipate all
    the contexts in which people may need this. It would be a crying shame if you had some
    variant type involving [Nothing.t] that you wished to make identifiable, but were
    prevented for lack of [Identifiable.S] here.

    Obviously, [of_string] and [t_of_sexp] will raise an exception. *)
include Identifiable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare, hash]
  end
end
