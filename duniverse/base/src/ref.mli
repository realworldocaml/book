(** Module for the type [ref], mutable indirection cells [r] containing a value of type
    ['a], accessed with [!r] and set by [r := a]. *)

open! Import

type 'a t = 'a Caml.ref = { mutable contents : 'a }
[@@deriving_inline compare, equal, sexp, sexp_grammar]

include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
include Sexplib0.Sexpable.S1 with type 'a t := 'a t

val t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

[@@@end]

(*_ defined as externals to avoid breaking the inliner *)

external create : 'a -> 'a t = "%makemutable"
external ( ! ) : 'a t -> 'a = "%field0"
external ( := ) : 'a t -> 'a -> unit = "%setfield0"

(** [swap t1 t2] swaps the values in [t1] and [t2]. *)
val swap : 'a t -> 'a t -> unit

(** [replace t f] is [t := f !t] *)
val replace : 'a t -> ('a -> 'a) -> unit

(** [set_temporarily t a ~f] sets [t] to [a], calls [f ()], and then restores [t] to its
    value prior to [set_temporarily] being called, whether [f] returns or raises. *)
val set_temporarily : 'a t -> 'a -> f:(unit -> 'b) -> 'b

module And_value : sig
  type t = T : 'a ref * 'a -> t [@@deriving sexp_of]

  (** [set (T (r, x))] is equivalent to [r := x]. *)
  val set : t -> unit

  (** [sets ts = List.iter ts ~f:set] *)
  val sets : t list -> unit

  (** [snapshot (T (r, _))] returns [T (r, !r)]. *)
  val snapshot : t -> t
end

(** [sets_temporarily [ ...; T (ti, ai); ... ] ~f] sets each [ti] to [ai], calls [f ()],
    and then restores all [ti] to their value prior to [sets_temporarily] being called,
    whether [f] returns or raises. *)
val sets_temporarily : And_value.t list -> f:(unit -> 'a) -> 'a
