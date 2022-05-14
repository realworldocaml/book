(** Used for specifying a bound (either upper or lower) as inclusive, exclusive, or
    unbounded. *)

open! Import

type 'a t =
  | Incl of 'a
  | Excl of 'a
  | Unbounded
[@@deriving_inline enumerate, sexp, sexp_grammar]

include Ppx_enumerate_lib.Enumerable.S1 with type 'a t := 'a t
include Sexplib0.Sexpable.S1 with type 'a t := 'a t

val t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

[@@@end]

val map : 'a t -> f:('a -> 'b) -> 'b t
val is_lower_bound : 'a t -> of_:'a -> compare:('a -> 'a -> int) -> bool
val is_upper_bound : 'a t -> of_:'a -> compare:('a -> 'a -> int) -> bool

(** [interval_contains_exn ~lower ~upper x ~compare] raises if [lower] and [upper] are
    crossed. *)
val interval_contains_exn
  :  lower:'a t
  -> upper:'a t
  -> 'a
  -> compare:('a -> 'a -> int)
  -> bool

(** [bounds_crossed ~lower ~upper ~compare] returns true if [lower > upper].

    It ignores whether the bounds are [Incl] or [Excl]. *)
val bounds_crossed : lower:'a t -> upper:'a t -> compare:('a -> 'a -> int) -> bool

type interval_comparison =
  | Below_lower_bound
  | In_range
  | Above_upper_bound
[@@deriving_inline sexp, sexp_grammar, compare, hash]

val sexp_of_interval_comparison : interval_comparison -> Sexplib0.Sexp.t
val interval_comparison_of_sexp : Sexplib0.Sexp.t -> interval_comparison
val interval_comparison_sexp_grammar : interval_comparison Sexplib0.Sexp_grammar.t
val compare_interval_comparison : interval_comparison -> interval_comparison -> int

val hash_fold_interval_comparison
  :  Ppx_hash_lib.Std.Hash.state
  -> interval_comparison
  -> Ppx_hash_lib.Std.Hash.state

val hash_interval_comparison : interval_comparison -> Ppx_hash_lib.Std.Hash.hash_value

[@@@end]

(** [compare_to_interval_exn ~lower ~upper x ~compare] raises if [lower] and [upper] are
    crossed. *)
val compare_to_interval_exn
  :  lower:'a t
  -> upper:'a t
  -> 'a
  -> compare:('a -> 'a -> int)
  -> interval_comparison
