open! Import
open Ppx_quickcheck_test_examples

module Helpers = struct
  module type S = sig
    type t

    val quickcheck_generator : t Generator.t
    val quickcheck_observer : t Observer.t
    val quickcheck_shrinker : t Shrinker.t
  end

  let test (type a) ?config ?cr ?generator ?observer ?shrinker q m =
    let (module Q : S with type t = a) = q in
    test_generator ?config ?cr ?mode:generator Q.quickcheck_generator m;
    test_observer ?config ?cr ?mode:observer Q.quickcheck_observer m;
    test_shrinker ?config ?cr ?mode:shrinker Q.quickcheck_shrinker m
  ;;

  module type All = sig
    type t [@@deriving compare, enumerate, sexp_of]
  end

  let m_all (type a) (module M : All with type t = a) =
    (module struct
      include M

      let examples = all
    end : With_examples
      with type t = a)
  ;;
end

open Helpers
module Simple_reference = Simple_reference
module Dotted_reference = Dotted_reference
module Nonrec_reference = Nonrec_reference

let%expect_test "type names" =
  test ~shrinker:`atomic (module Simple_reference) m_bool;
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker atomic) |}];
  test ~shrinker:`atomic (module Dotted_reference) m_bool;
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker atomic) |}];
  test ~shrinker:`atomic (module Nonrec_reference) m_bool;
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker atomic) |}]
;;

module Application_of_polymorphic_type = Application_of_polymorphic_type

let%expect_test "application of polymorphic type" =
  test (module Application_of_polymorphic_type) (m_option m_bool);
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker (((false) => ()) ((true) => ()))) |}]
;;

module Tuple = Tuple

let%expect_test "tuple" =
  test (module Tuple) (m_pair m_bool (m_option m_unit));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker (((false (())) => (false ())) ((true (())) => (true ())))) |}]
;;

module Poly_variant = Poly_variant
module Inherit_poly_variant = Inherit_poly_variant

let%expect_test "polymorphic variant" =
  let module Poly_variant' = struct
    type t =
      [ `A
      | `B
      | `C of bool
      | `D of bool
      | `E of bool * unit option
      | `F of bool * unit option
      ]
    [@@deriving compare, enumerate, sexp_of]
  end
  in
  test (module Poly_variant) (m_all (module Poly_variant'));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker
     (((E (false (()))) => (E (false ())))
      ((E (true (()))) => (E (true ())))
      ((F (false (()))) => (F (false ())))
      ((F (true (()))) => (F (true ()))))) |}];
  let module Inherit_poly_variant' = struct
    type t =
      [ `X
      | Poly_variant'.t
      | `Z of unit option
      ]
    [@@deriving compare, enumerate, sexp_of]
  end
  in
  test (module Inherit_poly_variant) (m_all (module Inherit_poly_variant'));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker
     (((E (false (()))) => (E (false ())))
      ((E (true (()))) => (E (true ())))
      ((F (false (()))) => (F (false ())))
      ((F (true (()))) => (F (true ())))
      ((Z (())) => (Z ())))) |}]
;;

module Record_type = Record_type

let%expect_test "record type" =
  let module Record_type' = struct
    type t = Record_type.t =
      { x : bool
      ; y : unit option
      }
    [@@deriving compare, enumerate, sexp_of]
  end
  in
  test (module Record_type) (m_all (module Record_type'));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker
     ((((x false) (y (()))) => ((x false) (y ())))
      (((x true) (y (()))) => ((x true) (y ()))))) |}]
;;

module Nullary_and_unary_variant = Nullary_and_unary_variant
module Binary_and_record_variant = Binary_and_record_variant

let%expect_test "variant type" =
  let module Nullary_and_unary_variant' = struct
    type t = Nullary_and_unary_variant.t =
      | A
      | B
      | C of unit
      | D of unit
    [@@deriving compare, enumerate, sexp_of]
  end
  in
  test
    ~shrinker:`atomic
    (module Nullary_and_unary_variant)
    (m_all (module Nullary_and_unary_variant'));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker atomic) |}];
  let module Binary_and_record_variant' = struct
    type t = Binary_and_record_variant.t =
      | A of bool * [ `X | `Y | `Z of unit ]
      | B of bool * [ `X | `Y | `Z of unit ]
      | C of
          { x : unit option
          ; mutable y : bool
          }
      | D of
          { x : unit option
          ; mutable y : bool
          }
    [@@deriving compare, enumerate, sexp_of]
  end
  in
  test (module Binary_and_record_variant) (m_all (module Binary_and_record_variant'));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker
     (((C (x (())) (y false)) => (C (x ()) (y false)))
      ((C (x (())) (y true)) => (C (x ()) (y true)))
      ((D (x (())) (y false)) => (D (x ()) (y false)))
      ((D (x (())) (y true)) => (D (x ()) (y true))))) |}]
;;

module Simple_arrow = Simple_arrow
module Named_arrow = Named_arrow
module Optional_arrow = Optional_arrow
module Curried_arrow = Curried_arrow

let%expect_test "first order arrow type" =
  let config = { Test.default_config with test_count = 1_000 } in
  let test ?cr ?(config = config) m = test ?cr ~config ~shrinker:`atomic m in
  test (module Simple_arrow) (m_arrow (m_option m_unit) m_bool);
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker atomic) |}];
  test (module Named_arrow) (m_arrow_named (m_option m_unit) m_bool);
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker atomic) |}];
  test (module Optional_arrow) (m_arrow_optional (m_option m_unit) m_bool);
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker atomic) |}];
  test
    (module Curried_arrow)
    (m_arrow (m_option m_unit) (m_arrow (m_option m_bool) m_bool));
  [%expect
    {|
    (generator "generated 64 distinct values in 1_000 iterations")
    (observer transparent)
    (shrinker atomic) |}]
;;

module Simple_higher_order = Simple_higher_order
module Named_higher_order = Named_higher_order
module Optional_higher_order = Optional_higher_order

let%expect_test ("higher order arrow type"[@tags "64-bits-only"]) =
  let config = { Test.default_config with test_count = 100 } in
  let test m = test ~config ~shrinker:`atomic m in
  test
    (module Simple_higher_order)
    (m_arrow (m_arrow (m_option m_unit) (m_option m_bool)) m_bool);
  [%expect
    {|
    (generator "generated 55 distinct values in 100 iterations")
    (observer transparent)
    (shrinker atomic) |}];
  test
    (module Named_higher_order)
    (m_arrow (m_arrow_named (m_option m_unit) (m_option m_bool)) m_bool);
  [%expect
    {|
    (generator "generated 55 distinct values in 100 iterations")
    (observer transparent)
    (shrinker atomic) |}];
  test
    (module Optional_higher_order)
    (m_arrow (m_arrow_optional (m_option m_unit) (m_option m_bool)) m_bool);
  [%expect
    {|
    (generator "generated 49 distinct values in 100 iterations")
    (observer transparent)
    (shrinker atomic) |}]
;;

module Poly_unary = Poly_unary
module Instance_of_unary = Instance_of_unary
module Poly_binary = Poly_binary
module Instance_of_binary = Instance_of_binary
module Poly_with_variance = Poly_with_variance
module Instance_with_variance = Instance_with_variance
module Poly_with_phantom = Poly_with_phantom
module Instance_with_phantom = Instance_with_phantom

let%expect_test "polymorphic type" =
  test (module Instance_of_unary) (m_list m_bool);
  [%expect
    {|
    (generator "generated 2_248 distinct values in 10_000 iterations")
    (observer transparent)
    (shrinker
     (((false) => ())
      ((true) => ())
      ((false true) => (true))
      ((false true) => (false))
      ((true false) => (false))
      ((true false) => (true)))) |}];
  test (module Instance_of_binary) (m_pair m_bool (m_option m_unit));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker (((false (())) => (false ())) ((true (())) => (true ())))) |}];
  test
    (module Instance_with_variance)
    (m_pair (m_option m_unit) (m_arrow m_bool (m_option m_unit)));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker
     ((((()) ((false ()) (true ()))) => (() ((false ()) (true ()))))
      (((()) ((false ()) (true (())))) => (() ((false ()) (true (())))))
      (((()) ((false (())) (true ()))) => (() ((false (())) (true ()))))
      (((()) ((false (())) (true (())))) => (() ((false (())) (true (()))))))) |}];
  test (module Instance_with_phantom) (m_option m_unit);
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker (((()) => ()))) |}]
;;

module Recursive = Recursive

let%expect_test "recursive type" =
  let module Recursive' = struct
    type t = Recursive.t =
      | Leaf
      | Node of t * t
    [@@deriving compare, hash, sexp_of]

    let rec create = function
      | 0 -> Leaf
      | n ->
        let t = create (n - 1) in
        Node (t, t)
    ;;

    let examples = List.init 3 ~f:create
  end
  in
  test ~shrinker:`atomic (module Recursive) (module Recursive');
  [%expect
    {|
    (generator "generated 1_954 distinct values in 10_000 iterations")
    (observer transparent)
    (shrinker atomic) |}]
;;

module Recursive_with_indirect_base_case = Recursive_with_indirect_base_case

let%expect_test "recursive type with indirect base case" =
  let module Recursive_with_indirect_base_case' = struct
    type t = Recursive_with_indirect_base_case.t = { children : t list }
    [@@deriving compare, hash, sexp_of]

    let examples =
      List.init 3 ~f:(fun n ->
        { children = List.init n ~f:(Fn.const { children = [] }) })
    ;;
  end
  in
  test
    (module Recursive_with_indirect_base_case)
    (module Recursive_with_indirect_base_case');
  [%expect
    {|
    (generator "generated 4_507 distinct values in 10_000 iterations")
    (observer transparent)
    (shrinker
     ((((children (((children ()))))) => ((children ())))
      (((children (((children ())) ((children ())))))
       =>
       ((children (((children ()))))))
      (((children (((children ())) ((children ())))))
       =>
       ((children (((children ())))))))) |}]
;;

module Mutually_recursive = Mutually_recursive

let%expect_test "mutually recursive types" =
  let module Mutually_recursive' = struct
    type expr = Mutually_recursive.expr =
      | Constant of int64
      | Operator of op
      | Application of expr * args

    and op =
      [ `plus
      | `minus
      | `abs
      ]

    and args = expr list [@@deriving compare, hash, sexp_of]
  end
  in
  test
    ~shrinker:`atomic
    (module struct
      type t = Mutually_recursive.expr [@@deriving quickcheck]
    end)
    (module struct
      type t = Mutually_recursive'.expr [@@deriving compare, hash, sexp_of]

      let examples : t list =
        [ Constant 0L; Operator `plus; Application (Operator `abs, []) ]
      ;;
    end);
  [%expect
    {|
    (generator "generated 5_895 distinct values in 10_000 iterations")
    (observer transparent)
    (shrinker atomic) |}]
;;

module Extensions = Extensions

let%expect_test "extensions" =
  let module Extensions' = struct
    type t =
      [ `A
      | `B of bool * unit option
      ]
    [@@deriving compare, enumerate, sexp_of]
  end
  in
  test (module Extensions) (m_all (module Extensions'));
  [%expect
    {|
    (generator exhaustive)
    (observer transparent)
    (shrinker
     (((B (false (()))) => (B (false ()))) ((B (true (()))) => (B (true ()))))) |}]
;;

module Escaped = Escaped

let%expect_test "escaped" =
  let module Escaped' = struct
    type t = int * char * bool option [@@deriving compare, sexp_of]

    let examples =
      List.concat_map [ 1; 2 ] ~f:(fun int ->
        List.concat_map [ 'a'; 'b' ] ~f:(fun string ->
          List.concat_map [ None; Some true ] ~f:(fun bool_option ->
            [ int, string, bool_option ])))
    ;;
  end
  in
  (* We disable CRs in test output because the observer is neither strictly transparent
     nor strictly opaque, so it will fail the test in either observer mode we give it. *)
  test ~cr:Comment ~shrinker:`atomic (module Escaped) (module Escaped');
  [%expect
    {|
    (generator "generated 4_992 distinct values in 10_000 iterations")
    (observer
     (partitions
      (((1 a ()) (1 b ()))
       ((1 a (true)) (1 b (true)))
       ((2 a ()) (2 b ()))
       ((2 a (true)) (2 b (true))))))
    (* require-failed: lib/base_quickcheck/test/helpers/base_quickcheck_test_helpers.ml:LINE:COL. *)
    "did not generate any single function that distinguishes all values"
    (shrinker atomic) |}]
;;

module Wildcard = Wildcard

let%expect_test "wildcard" =
  let module Instance = struct
    include Wildcard ((val m_bool))

    let compare = [%compare: bool list]
    let sexp_of_t = [%sexp_of: bool list]

    let examples =
      let module T = (val m_list m_bool) in
      T.examples
    ;;
  end
  in
  (* We disable CRs in test output because the observer is neither strictly transparent
     nor strictly opaque, so it will fail the test in either observer mode we give it. *)
  test ~cr:Comment (module Instance) (module Instance);
  [%expect
    {|
    (generator "generated 2_248 distinct values in 10_000 iterations")
    (observer (partitions ((()) ((false) (true)) ((false true) (true false)))))
    (* require-failed: lib/base_quickcheck/test/helpers/base_quickcheck_test_helpers.ml:LINE:COL. *)
    "did not generate any single function that distinguishes all values"
    (shrinker
     (((false) => ())
      ((true) => ())
      ((false true) => (true))
      ((false true) => (false))
      ((true false) => (false))
      ((true false) => (true)))) |}]
;;

module Attribute_override = Attribute_override

let%expect_test "attributes" =
  let module Attribute_override' = struct
    type t = Attribute_override.t =
      | Null
      | Text of string
      | Number of float
    [@@deriving compare, sexp_of]

    let examples = [ Null; Text "a"; Number 1. ]
  end
  in
  test (module Attribute_override) (module Attribute_override');
  [%expect
    {|
    (generator "generated 8_470 distinct values in 10_000 iterations")
    (observer transparent)
    (shrinker (((Text a) => (Text "")))) |}];
  show_distribution Attribute_override.quickcheck_generator (module Attribute_override');
  [%expect
    {|
    ((4.35% Null)
     (4.12% (Text ""))
     (50bp (Number 1))
     (27bp (Number 4.94065645841247E-324))
     (23bp (Number 2.2250738585072009E-308))
     (21bp (Text m))
     (19bp (Text q))
     (19bp (Text n))
     (16bp (Text h))
     (16bp (Text c))
     (15bp (Number 8.98846567431158E+307))
     (15bp (Text y))
     (15bp (Text v))
     (15bp (Text u))
     (15bp (Text o))
     (15bp (Text l))
     (15bp (Text e))
     (14bp (Number 0.5))
     (14bp (Text r))
     (14bp (Text i))) |}]
;;

module Attribute_override_recursive = Attribute_override_recursive

let%expect_test "attributes for recursive types" =
  let module Attribute_override_recursive' = struct
    type t = Attribute_override_recursive.t =
      | Leaf
      | Node1 of t * int64 * t
      | Node2 of t * int64 * t * int64 * t
    [@@deriving compare, sexp_of]

    let examples = [ Leaf; Node1 (Leaf, 0L, Leaf); Node2 (Leaf, 0L, Leaf, 0L, Leaf) ]
  end
  in
  test
    ~shrinker:`atomic
    (module Attribute_override_recursive)
    (module Attribute_override_recursive');
  [%expect
    {|
    (generator "generated 4_007 distinct values in 10_000 iterations")
    (observer transparent)
    (shrinker atomic) |}];
  show_distribution
    Attribute_override_recursive.quickcheck_generator
    (module Attribute_override_recursive');
  [%expect
    {|
    ((58.49% Leaf)
     (27bp (Node1 Leaf -1 Leaf))
     (26bp (Node1 Leaf -9223372036854775808 Leaf))
     (25bp (Node1 Leaf 0 Leaf))
     (24bp (Node1 Leaf 9223372036854775807 Leaf))
     (9bp (Node1 Leaf -2 Leaf))
     (7bp (Node1 Leaf 1 Leaf))
     (5bp (Node1 Leaf -3 Leaf))
     (4bp (Node1 Leaf 3 Leaf))
     (4bp (Node1 Leaf -4 Leaf))
     (4bp (Node1 Leaf -6 Leaf))
     (3bp (Node2 Leaf -9223372036854775808 Leaf -9223372036854775808 Leaf))
     (3bp (Node1 Leaf 15 Leaf))
     (3bp (Node1 Leaf 10 Leaf))
     (3bp (Node1 Leaf 8 Leaf))
     (3bp (Node1 Leaf 6 Leaf))
     (2bp (Node2 Leaf -1 Leaf 9223372036854775807 Leaf))
     (2bp (Node1 Leaf 98 Leaf))
     (2bp (Node1 Leaf 83 Leaf))
     (2bp (Node1 Leaf 29 Leaf))) |}]
;;

module Deriving_from_wildcard = Deriving_from_wildcard

let%expect_test "polymorphic wildcard" =
  let module Transparent = struct
    type t = int Deriving_from_wildcard.transparent [@@deriving quickcheck]

    let compare = [%compare: string]
    let sexp_of_t = [%sexp_of: string]
    let examples = [ ""; "a" ]
  end
  in
  test (module Transparent) (module Transparent);
  [%expect
    {|
    (generator "generated 8_583 distinct values in 10_000 iterations")
    (observer transparent)
    (shrinker ((a => ""))) |}];
  let module Opaque = struct
    type t = int64 Deriving_from_wildcard.opaque
    [@@deriving compare, quickcheck, sexp_of]

    let examples = Deriving_from_wildcard.opaque_examples
  end
  in
  test (module Opaque) (module Opaque);
  [%expect
    {|
    (generator "generated 4_207 distinct values in 10_000 iterations")
    (observer transparent)
    (shrinker (((0) => ()) ((1) => ()))) |}]
;;
