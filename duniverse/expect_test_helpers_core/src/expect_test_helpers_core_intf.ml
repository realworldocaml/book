open! Core

module Allocation_limit = struct
  type t =
    | Major_words of int
    | Minor_words of int
  [@@deriving sexp_of]
end

module type With_comparable = sig
  type t [@@deriving compare, sexp_of]
  type key := t

  include Comparator.S with type t := t

  (* [Set] and [Map] submodules are here because we specifically want to test whether they
     have been constructed correctly, as opposed to testing the functor that creates
     them. *)

  module Set : sig
    type t = (key, comparator_witness) Set.t [@@deriving sexp_of]
  end

  module Map : sig
    type 'a t = (key, 'a, comparator_witness) Map.t [@@deriving sexp_of]
  end
end

module Comparable_satisfies_with_comparable (M : sig
    type t [@@deriving sexp_of]

    include Comparable.S with type t := t
  end) : With_comparable =
  M

module type With_hashable = sig
  type t [@@deriving compare, hash, sexp_of]
  type key := t

  (* [Hash_set] and [Table] submodules are here because we specifically want to test
     whether they have been constructed correctly, as opposed to testing the functor that
     creates them. *)

  module Hash_set : sig
    type t = key Hash_set.t [@@deriving sexp_of]
  end

  module Table : sig
    type 'a t = (key, 'a) Hashtbl.t [@@deriving sexp_of]
  end
end

module Hashable_satisfies_with_hashable (M : sig
    type t [@@deriving sexp_of]

    include Hashable.S with type t := t
  end) : With_hashable =
  M

module type With_containers = sig
  type t

  include With_comparable with type t := t
  include With_hashable with type t := t
end

module type Expect_test_helpers_core = sig
  (** Helpers for producing output inside [let%expect_test]. Designed for code using
      [Core]. See also [Expect_test_helpers_base] and [Expect_test_helpers_async]. *)

  include module type of struct
    include Expect_test_helpers_base
  end

  module type With_containers = With_containers
  module type With_comparable = With_comparable
  module type With_hashable = With_hashable

  (** {3 Serialization tests} *)

  (** [print_and_check_stable_type] prints the bin-io digest for the given type, and the
      bin-io and sexp serializations of the given values.  Prints an error message for any
      serializations that fail to round-trip, and for any bin-io serializations that
      exceed [max_binable_length]. *)
  val print_and_check_stable_type
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?max_binable_length:int (** default is [Int.max_value] *)
    -> Source_code_position.t
    -> (module Stable_without_comparator with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_stable_int63able_type] works like [print_and_check_stable_type],
      and includes [Int63.t] serializations. *)
  val print_and_check_stable_int63able_type
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?max_binable_length:int (** default is [Int.max_value] *)
    -> Source_code_position.t
    -> (module Stable_int63able with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_container_sexps] prints the sexp representation of maps, sets, hash
      tables, and hash sets based on the given values.  For sets and hash sets, prints a
      CR if the sexp does not correspond to a list of elements.  For maps and hash tables,
      prints a CR if the sexp does not correspond to an association list keyed on
      elements. *)
  val print_and_check_container_sexps
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (module With_containers with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_comparable_sexps] is like [print_and_check_container_sexps] for
      maps and sets only. *)
  val print_and_check_comparable_sexps
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (module With_comparable with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_hashable_sexps] is like [print_and_check_container_sexps] for hash
      tables and hash sets only. *)
  val print_and_check_hashable_sexps
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (module With_hashable with type t = 'a)
    -> 'a list
    -> unit

  (** Removes strings that look like time spans; see [Time.Span]. *)
  val remove_time_spans : string -> string

  (** {3 Allocation tests} *)


  module Allocation_limit : module type of struct
    include Allocation_limit
  end

  (** [require_allocation_does_not_exceed] is a specialized form of [require] that only
      produces output when [f ()] allocates more than the given limits.  The output will
      include the actual number of major and minor words allocated.  We do NOT include
      these numbers in the successful case because those numbers are not stable with
      respect to compiler versions and build flags.

      If [f] returns a value that should be ignored, use this idiom:

      {[
        ignore (require_allocation_does_not_exceed ... f : t)
      ]}

      rather than this idiom:

      {[
        require_allocation_does_not_exceed ... (fun () -> ignore (f () : t))
      ]}

      With the latter idiom, the compiler may optimize the computation of [f ()] taking
      advantage of the fact that the result is ignored, and eliminate allocation that is
      intended to be measured.  With the former idiom, the compiler cannot do such
      optimization and must compute the result of [f ()].

      See documentation above about CRs and workflows for failing allocation tests. *)
  val require_allocation_does_not_exceed
    :  ?hide_positions:bool (** default is [false] *)
    -> Allocation_limit.t
    -> Source_code_position.t
    -> (unit -> 'a)
    -> 'a

  (** [require_no_allocation here f] is equivalent to [require_allocation_does_not_exceed
      (Minor_words 0) here f].

      See documentation above about CRs and workflows for failing allocation tests. *)
  val require_no_allocation
    :  ?hide_positions:bool (** default is [false] *)
    -> Source_code_position.t
    -> (unit -> 'a)
    -> 'a

  (**/**)

  (** This module is called [Expect_test_helpers_core_private] rather than [Private]
      because [include Expect_test_helpers_core] is a common idiom, and we don't want to
      interfere with other [Private] modules or create problems due to multiple
      definitions of [Private]. *)
  module Expect_test_helpers_core_private : sig
    val require_allocation_does_not_exceed
      :  ?cr:CR.t
      -> ?hide_positions:bool
      -> Allocation_limit.t
      -> Source_code_position.t
      -> (unit -> 'a)
      -> 'a
  end
end
