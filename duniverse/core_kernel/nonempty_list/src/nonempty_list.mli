open Core

(** A ['a t] represents a non-empty list, as evidenced by the fact that there is no [[]]
    variant. The sexp representation is as a regular list (i.e., the same as the
    [Stable.V3] module below).
*)
type 'a t = ( :: ) of 'a * 'a list
[@@deriving compare, equal, sexp, sexp_grammar, hash, quickcheck, typerep]

include Comparator.Derived with type 'a t := 'a t
include Container.S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val create : 'a -> 'a list -> 'a t
val of_list : 'a list -> 'a t option
val of_list_error : 'a list -> 'a t Or_error.t
val of_list_exn : 'a list -> 'a t
val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
val reverse : 'a t -> 'a t
val append : 'a t -> 'a list -> 'a t
val unzip : ('a * 'b) t -> 'a t * 'b t
val zip : 'a t -> 'b t -> ('a * 'b) t List.Or_unequal_lengths.t
val zip_exn : 'a t -> 'b t -> ('a * 'b) t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val concat : 'a t t -> 'a t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val last : 'a t -> 'a
val drop_last : 'a t -> 'a list
val to_sequence : 'a t -> 'a Sequence.t
val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val stable_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val dedup_and_sort : compare:('a -> 'a -> int) -> 'a t -> 'a t
val fold_right : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
val folding_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'c t

(** [min_elt'] and [max_elt'] differ from [min_elt] and [max_elt] (included in
    [Container.S1]) in that they don't return options. *)
val min_elt' : 'a t -> compare:('a -> 'a -> int) -> 'a

val max_elt' : 'a t -> compare:('a -> 'a -> int) -> 'a

(** Like [Map.of_alist_multi], but comes with a guarantee that the range of the returned
    map is all nonempty lists.
*)
val map_of_alist_multi
  :  ('k * 'v) list
  -> comparator:('k, 'cmp) Map.comparator
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_sequence_multi], but comes with a guarantee that the range of the
    returned map is all nonempty lists.
*)
val map_of_sequence_multi
  :  ('k * 'v) Sequence.t
  -> comparator:('k, 'cmp) Map.comparator
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Result.combine_errors] but for non-empty lists *)
val combine_errors : ('ok, 'err) Result.t t -> ('ok t, 'err t) Result.t

(** Like [Result.combine_errors_unit] but for non-empty lists *)
val combine_errors_unit : (unit, 'err) Result.t t -> (unit, 'err t) Result.t

(** validates a list, naming each element by its position in the list (where the first
    position is 1, not 0). *)
val validate_indexed : 'a Validate.check -> 'a t Validate.check

(** validates a list, naming each element using a user-defined function for computing the
    name. *)
val validate : name:('a -> string) -> 'a Validate.check -> 'a t Validate.check

val flag : 'a Command.Param.Arg_type.t -> 'a t Command.Flag.t

type 'a nonempty_list := 'a t


(** This module provides 0-alloc versions of [to_list] and [of_list], via [some] and
    allowing you to [match%optional] on a list, respectively. *)
module Option : sig
  type 'a t = 'a list
  [@@deriving compare, equal, sexp, sexp_grammar, hash, quickcheck, typerep]

  (** Constructors analogous to [None] and [Some]. *)

  val none : _ t
  val some : 'a nonempty_list -> 'a t
  val is_none : _ t -> bool
  val is_some : _ t -> bool

  (** [value (some x) ~default = x] and [value none ~default = default]. *)
  val value : 'a t -> default:'a nonempty_list -> 'a nonempty_list

  (** [value_exn (some x) = x].  [value_exn none] raises.  Unlike [Option.value_exn],
      there is no [?message] argument, so that calls to [value_exn] that do not raise
      also do not have to allocate. *)
  val value_exn : 'a t -> 'a nonempty_list

  (** [unchecked_value (some x) = x].  [unchecked_value none] returns an unspecified
      value.  [unchecked_value t] is intended as an optimization of [value_exn t] when
      [is_some t] is known to be true. *)
  val unchecked_value : 'a t -> 'a nonempty_list

  val to_option : 'a t -> 'a nonempty_list option
  val of_option : 'a nonempty_list option -> 'a t

  module Optional_syntax :
    Optional_syntax.S1 with type 'a t := 'a t and type 'a value := 'a nonempty_list
end

module Reversed : sig
  type 'a t = ( :: ) of 'a * 'a Reversed_list.t [@@deriving sexp_of]

  val cons : 'a -> 'a t -> 'a t
  val to_rev_list : 'a t -> 'a Reversed_list.t
  val rev : 'a t -> 'a nonempty_list
  val rev_append : 'a t -> 'a list -> 'a nonempty_list
  val rev_map : 'a t -> f:('a -> 'b) -> 'b nonempty_list
  val rev_mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b nonempty_list
end

val rev_append : 'a Reversed_list.t -> 'a t -> 'a t

module Unstable : sig
  type nonrec 'a t = 'a t [@@deriving bin_io, compare, equal, hash, sexp]
end

module Stable : sig
  (** Represents a [t] as an ordinary list for sexp and bin_io conversions, e.g. [1::2]
      is represented as [(1 2)]. *)
  module V3 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, equal, sexp, hash]
  end

  (** Represents a [t] as an ordinary list for sexp conversions, but uses a record [{hd :
      'a; tl ; 'a list}] for bin_io conversions. *)
  module V2 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, equal, sexp, hash]
  end

  (** Represents a [t] as an ordinary list for sexps, but as a pair for bin_io conversions
      (i.e., a ['a t] is represented as the type ['a * 'a list]). *)
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, equal, sexp]
  end
end
