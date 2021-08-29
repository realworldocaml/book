open! Base
open Base_quickcheck

module type Value = sig
  type t [@@deriving compare, sexp_of]
end

module type With_examples = sig
  type t [@@deriving compare, sexp_of]

  val examples : t list
end

module type Base_quickcheck_test_helpers = sig
  module type With_examples = With_examples

  (** This module provides rough sanity tests of generators, observers, and shrinkers
      based on a handful of example values for the relevant type. *)

  (** Tests whether the generator's distribution produces all the example values. Prints a
      cr if the result is inconsistent with the [~mode] argument. *)
  val test_generator
    :  ?config:Test.Config.t
    -> ?mode:[ `exhaustive | `inexhaustive ] (** default: [`exhaustive] *)
    -> ?cr:Expect_test_helpers_core.CR.t
    -> 'a Generator.t
    -> (module With_examples with type t = 'a)
    -> unit

  (** Tests whether the observer can distinguish all examples from each other. Prints a cr
      if the result is inconsistent with the [~mode] argument. *)
  val test_observer
    :  ?config:Test.Config.t
    -> ?mode:[ `transparent | `opaque ] (** default: [`transparent] *)
    -> ?cr:Expect_test_helpers_core.CR.t
    -> 'a Observer.t
    -> (module With_examples with type t = 'a)
    -> unit

  (** Tests whether the shrinker can produce smaller versions of any of the example
      values. Prints a cr if the result is inconsistent with the [~mode] argument. *)
  val test_shrinker
    :  ?config:Test.Config.t
    -> ?mode:[ `compound | `atomic ] (** default: [`compound] *)
    -> ?cr:Expect_test_helpers_core.CR.t
    -> 'a Shrinker.t
    -> (module With_examples with type t = 'a)
    -> unit

  (** Shows the approximate distribution of output values for a generator based on
      [config.test_count] trials. *)
  val show_distribution
    :  ?config:Test.Config.t
    -> ?show:int
    -> 'a Generator.t
    -> (module Value with type t = 'a)
    -> unit

  (** These first-class modules provide examples for use in the above tests. *)

  val m_int : (module Int.S with type t = 'a) -> (module With_examples with type t = 'a)
  val m_nat : up_to:int -> (module With_examples with type t = int)

  val m_nat'
    :  up_to:int
    -> (module Int.S with type t = 'a)
    -> (module With_examples with type t = 'a)

  val m_unit : (module With_examples with type t = unit)
  val m_bool : (module With_examples with type t = bool)
  val m_char : (module With_examples with type t = char)
  val m_float : (module With_examples with type t = float)
  val m_string : (module With_examples with type t = string)
  val m_sexp : (module With_examples with type t = Sexp.t)

  val m_option
    :  (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'a option)

  val m_list
    :  (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'a list)

  val m_either
    :  (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'b)
    -> (module With_examples with type t = ('a, 'b) Either.t)

  val m_result
    :  (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'b)
    -> (module With_examples with type t = ('a, 'b) Result.t)

  val m_pair
    :  (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'b)
    -> (module With_examples with type t = 'a * 'b)

  val m_arrow
    :  (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'b)
    -> (module With_examples with type t = 'a -> 'b)

  val m_arrow_named
    :  (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'b)
    -> (module With_examples with type t = x:'a -> 'b)

  val m_arrow_optional
    :  (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'b)
    -> (module With_examples with type t = ?x:'a -> unit -> 'b)

  val m_set
    :  (module Comparator.S with type t = 'a and type comparator_witness = 'c)
    -> (module With_examples with type t = 'a)
    -> (module With_examples with type t = ('a, 'c) Set.t)

  val m_map
    :  (module Comparator.S with type t = 'a and type comparator_witness = 'c)
    -> (module With_examples with type t = 'a)
    -> (module With_examples with type t = 'b)
    -> (module With_examples with type t = ('a, 'b, 'c) Map.t)

  val m_biject
    :  (module With_examples with type t = 'a)
    -> f:('a -> 'b)
    -> f_inverse:('b -> 'a)
    -> (module With_examples with type t = 'b)
end
