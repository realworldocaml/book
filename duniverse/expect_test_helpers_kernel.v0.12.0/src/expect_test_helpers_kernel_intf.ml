open! Core_kernel

module Allocation_limit = struct
  type t =
    | Major_words of int
    | Minor_words of int
  [@@deriving sexp_of]
end

module type With_containers = sig
  type t [@@deriving sexp]

  include Comparable with type t := t
  include Hashable with type t := t
end

module type With_comparable = sig
  type t [@@deriving sexp]

  include Comparable with type t := t
end

module type With_hashable = sig
  type t [@@deriving compare, sexp]

  include Hashable with type t := t
end

module type Expect_test_helpers_kernel = sig
  include module type of struct
  include Expect_test_helpers_base
end

  module type With_containers = With_containers
  module type With_comparable = With_comparable
  module type With_hashable = With_hashable

  module Allocation_limit : module type of struct
    include Allocation_limit
  end

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

  (** [prepare_heap_to_count_minor_allocation] calls [Gc] functions to setup the heap so
      that one can subsequently measure minor allocation via:

      {[
        let minor_words_before = Gc.minor_words () in
        (* ... do stuff ... *)
        let minor_words_after = Gc.minor_words () in
        let minor_words_allocated = minor_words_after - minor_words_before in
      ]} *)
  val prepare_heap_to_count_minor_allocation : unit -> unit

  (** [require_allocation_does_not_exceed] is a specialized form of [require] that only
      produces output when [f ()] allocates more than the given limits.  The output will
      include the actual number of major and minor words allocated.  We do NOT include
      these numbers in the successful case because those numbers are not stable with
      respect to compiler versions and build flags.

      If [f] returns a value that should be ignored, use this idiom:

      {[
        ignore (show_allocation f : t)
      ]}

      rather than this idiom:

      {[
        show_allocation (fun () -> ignore (f () : t))
      ]}

      With the latter idiom, the compiler may optimize the computation of [f ()] taking
      advantage of the fact that the result is ignored, and eliminate allocation that is
      intended to be measured.  With the former idiom, the compiler cannot do such
      optimization and must compute the result of [f ()]. *)
  val require_allocation_does_not_exceed
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Allocation_limit.t
    -> Source_code_position.t
    -> (unit -> 'a)
    -> 'a

  (** [require_no_allocation here f] is equivalent to [require_allocation_does_not_exceed
      (Minor_words 0) here f]. *)
  val require_no_allocation
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (unit -> 'a)
    -> 'a

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
end
