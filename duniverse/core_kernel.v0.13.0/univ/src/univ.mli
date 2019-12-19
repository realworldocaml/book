(** An extensible "universal" variant type.

    Every type id ([Type_equal.Id.t]) corresponds to one branch of the variant type.
*)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

val type_id_name : t -> string
val type_id_uid : t -> Type_equal.Id.Uid.t
val create : 'a Type_equal.Id.t -> 'a -> t

(** [does_match t id] returns [true] iff [t] was created by [create id v]. *)
val does_match : t -> _ Type_equal.Id.t -> bool

(** [match_ t id] returns [Some v] if [t] was created by [create id v], and returns [None]
    otherwise.

    [match_exn t id] returns [v] if [t] was created by [create id v], and raises
    otherwise. *)
val match_ : t -> 'a Type_equal.Id.t -> 'a option

val match_exn : t -> 'a Type_equal.Id.t -> 'a

module View : sig
  type t = T : 'a Type_equal.Id.t * 'a -> t
end

(** [view t] provides access to the GADT representation of [t].  This is currently the
    same as the underlying representation, but is put in the [View] module to make later
    changes to the underlying representation easier. *)
val view : t -> View.t
