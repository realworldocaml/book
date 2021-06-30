open! Import
open! Base.Int_math
open! Base.Int_math.Private

let%test_unit _ =
  let x =
    match Word_size.word_size with
    | W32 -> 9
    | W64 -> 10
  in
  for i = 0 to x do
    for j = 0 to x do
      assert (int_pow i j = Caml.(int_of_float (float_of_int i ** float_of_int j)))
    done
  done
;;

module Test (X : Make_arg) : sig end = struct
  open X
  include Make (X)

  let%test_module "integer-rounding" =
    (module struct
      let check dir ~range:(lower, upper) ~modulus expected =
        let modulus = of_int_exn modulus in
        let expected = of_int_exn expected in
        for i = lower to upper do
          let observed = round ~dir ~to_multiple_of:modulus (of_int_exn i) in
          if observed <> expected then raise_s [%message "invalid result" (i : int)]
        done
      ;;

      let%test_unit _ = check ~modulus:10 `Down ~range:(10, 19) 10
      let%test_unit _ = check ~modulus:10 `Down ~range:(0, 9) 0
      let%test_unit _ = check ~modulus:10 `Down ~range:(-10, -1) (-10)
      let%test_unit _ = check ~modulus:10 `Down ~range:(-20, -11) (-20)
      let%test_unit _ = check ~modulus:10 `Up ~range:(11, 20) 20
      let%test_unit _ = check ~modulus:10 `Up ~range:(1, 10) 10
      let%test_unit _ = check ~modulus:10 `Up ~range:(-9, 0) 0
      let%test_unit _ = check ~modulus:10 `Up ~range:(-19, -10) (-10)
      let%test_unit _ = check ~modulus:10 `Zero ~range:(10, 19) 10
      let%test_unit _ = check ~modulus:10 `Zero ~range:(-9, 9) 0
      let%test_unit _ = check ~modulus:10 `Zero ~range:(-19, -10) (-10)
      let%test_unit _ = check ~modulus:10 `Nearest ~range:(15, 24) 20
      let%test_unit _ = check ~modulus:10 `Nearest ~range:(5, 14) 10
      let%test_unit _ = check ~modulus:10 `Nearest ~range:(-5, 4) 0
      let%test_unit _ = check ~modulus:10 `Nearest ~range:(-15, -6) (-10)
      let%test_unit _ = check ~modulus:10 `Nearest ~range:(-25, -16) (-20)
      let%test_unit _ = check ~modulus:5 `Nearest ~range:(8, 12) 10
      let%test_unit _ = check ~modulus:5 `Nearest ~range:(3, 7) 5
      let%test_unit _ = check ~modulus:5 `Nearest ~range:(-2, 2) 0
      let%test_unit _ = check ~modulus:5 `Nearest ~range:(-7, -3) (-5)
      let%test_unit _ = check ~modulus:5 `Nearest ~range:(-12, -8) (-10)
    end)
  ;;

  let%test_module "remainder-and-modulus" =
    (module struct
      let one = of_int_exn 1

      let check_integers x y =
        let sexp_of_t t = sexp_of_string (to_string t) in
        let check_raises f what =
          match f () with
          | exception _ -> ()
          | z ->
            raise_s
              [%message
                "produced result instead of raising"
                  (what : string)
                  (x : t)
                  (y : t)
                  (z : t)]
        in
        let check_true cond what =
          if not cond then raise_s [%message "failed" (what : string) (x : t) (y : t)]
        in
        if y = zero
        then (
          check_raises (fun () -> x / y) "division by zero";
          check_raises (fun () -> rem x y) "rem _ zero";
          check_raises (fun () -> x % y) "_ % zero";
          check_raises (fun () -> x /% y) "_ /% zero")
        else (
          if x < zero
          then check_true (rem x y <= zero) "non-positive remainder"
          else check_true (rem x y >= zero) "non-negative remainder";
          check_true (abs (rem x y) <= abs y - one) "range of remainder";
          if y < zero
          then (
            check_raises (fun () -> x % y) "_ % negative";
            check_raises (fun () -> x /% y) "_ /% negative")
          else (
            check_true (x = (x /% y * y) + (x % y)) "(/%) and (%) identity";
            check_true (x = (x / y * y) + rem x y) "(/) and rem identity";
            check_true (x % y >= zero) "non-negative (%)";
            check_true (x % y <= y - one) "range of (%)";
            if x > zero && y > zero
            then (
              check_true (x /% y = x / y) "(/%) and (/) identity";
              check_true (x % y = rem x y) "(%) and rem identity")))
      ;;

      let check_natural_numbers x y =
        List.iter
          [ x; -x; x + one; -(x + one) ]
          ~f:(fun x ->
            List.iter [ y; -y; y + one; -(y + one) ] ~f:(fun y -> check_integers x y))
      ;;

      let%test_unit "deterministic" =
        let big1 = of_int_exn 118_310_344 in
        let big2 = of_int_exn 828_172_408 in
        (* Important to test the case where one value is a multiple of the other.  Note that
           the [x + one] and [y + one] cases in [check_natural_numbers] ensure that we also
           test non-multiple cases. *)
        assert (big2 = big1 * of_int_exn 7);
        let values = [ zero; one; big1; big2 ] in
        List.iter values ~f:(fun x ->
          List.iter values ~f:(fun y -> check_natural_numbers x y))
      ;;

      let%test_unit "random" =
        let rand = Random.State.make [| 8; 67; -5_309 |] in
        for _ = 0 to 1_000 do
          let max_value = 1_000_000_000 in
          let x = of_int_exn (Random.State.int rand max_value) in
          let y = of_int_exn (Random.State.int rand max_value) in
          check_natural_numbers x y
        done
      ;;
    end)
  ;;
end

include Test (Int)
include Test (Int32)
include Test (Int63)
include Test (Int64)
include Test (Nativeint)

let%test_module "int rounding quickcheck tests" =
  (module struct
    module type With_quickcheck = sig
      type t [@@deriving sexp_of]

      include Make_arg with type t := t

      val min_value : t
      val max_value : t
      val quickcheck_generator_incl : t -> t -> t Base_quickcheck.Generator.t
      val quickcheck_generator_log_incl : t -> t -> t Base_quickcheck.Generator.t
    end

    module Rounding_direction = struct
      type t =
        [ `Up
        | `Down
        | `Zero
        | `Nearest
        ]
      [@@deriving enumerate, sexp_of]
    end

    module Rounding_pair (Integer : With_quickcheck) = struct
      type t =
        { number : Integer.t
        ; factor : Integer.t
        }
      [@@deriving sexp_of]

      let quickcheck_generator =
        (* This generator should frequently generate "interesting" numbers for rounding. *)
        let open Base_quickcheck.Generator.Let_syntax in
        (* First we choose a factor to round to. *)
        let%bind factor =
          Integer.quickcheck_generator_log_incl (Integer.of_int_exn 1) Integer.max_value
        in
        (* Then we choose a multiplier for that factor. *)
        let%map multiplier =
          Integer.quickcheck_generator_incl
            (Integer.( / ) Integer.min_value factor)
            (Integer.( / ) Integer.max_value factor)
        (* Then we choose an offset such that [multiplier * factor] is the nearest value
           to round to. [quickcheck_generator_incl] puts extra weight on the [-factor/2,
           factor/2] bounds, and we also weight 0 heavily. *)
        and offset =
          let half_factor = Integer.( / ) factor (Integer.of_int_exn 2) in
          Base_quickcheck.Generator.weighted_union
            [ 9., Integer.quickcheck_generator_incl (Integer.neg half_factor) half_factor
            ; 1., Base_quickcheck.Generator.return Integer.zero
            ]
        in
        let number = Integer.( + ) offset (Integer.( * ) factor multiplier) in
        { number; factor }
      ;;

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end

    let test_direction (module Integer : With_quickcheck) ~dir =
      let open Integer in
      (* Criterion for correct rounding: must be a multiple of the factor *)
      let is_multiple_of number ~factor = factor * (number / factor) = number in
      (* Criterion for correct rounding: must not reverse sign *)
      let is_compatible_sign number ~rounded =
        if number > zero
        then rounded >= zero
        else if number < zero
        then rounded <= zero
        else rounded = zero
      in
      (* Criterion for correct rounding: must be less than factor away from original *)
      let is_close_enough x y ~factor =
        if x > y
        then x - y > zero && x - y < factor
        else if x < y
        then y - x > zero && y - x < factor
        else true
      in
      (* Criterion for correct rounding: rounding direction must be respected *)
      let is_in_correct_direction number ~dir ~rounded ~factor =
        match dir with
        | `Down -> rounded <= number
        | `Up -> rounded >= number
        | `Zero ->
          if number < zero
          then rounded >= number
          else if number > zero
          then rounded <= number
          else rounded = zero
        | `Nearest ->
          if rounded > number
          then rounded - number <= number - (rounded - factor)
          else if rounded < number
          then number - rounded < rounded + factor - number
          else true
      in
      (* Correct rounding obeys all four criteria *)
      let is_rounded_correctly number ~dir ~factor ~rounded =
        is_multiple_of rounded ~factor
        && is_compatible_sign number ~rounded
        && is_close_enough number rounded ~factor
        && is_in_correct_direction number ~dir ~rounded ~factor
      in
      (* Round correctly by finding a multiple of the factor, and trying +/-factor away
         from that. If this returns [None], there should be no correct representable
         result. *)
      let round_correctly number ~dir ~factor =
        let rounded0 = factor * (number / factor) in
        match
          List.filter
            [ rounded0 - factor; rounded0; rounded0 + factor ]
            ~f:(fun rounded -> is_rounded_correctly number ~dir ~factor ~rounded)
        with
        | [] -> None
        | [ rounded ] -> Some rounded
        | multiple ->
          raise_s
            [%sexp
              "test bug: multiple correctly rounded values", (multiple : Integer.t list)]
      in
      let module Math = Make (Integer) in
      let module Pair = Rounding_pair (Integer) in
      require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Pair)
          ~f:(fun ({ number; factor } : Pair.t) ->
            let rounded = Math.round number ~dir ~to_multiple_of:factor in
            (* Test that if it is possible to round correctly, then we do. *)
            match round_correctly number ~dir ~factor with
            | None ->
              if is_rounded_correctly number ~dir ~factor ~rounded
              then
                raise_s
                  [%sexp
                    "test bug: did not find correctly rounded value"
                  , { rounded : Integer.t }]
            | Some rounded_correctly ->
              if rounded <> rounded_correctly
              then
                raise_s
                  [%sexp
                    "rounding failed"
                  , { rounded : Integer.t; rounded_correctly : Integer.t }]))
    ;;

    let test m =
      List.iter Rounding_direction.all ~f:(fun dir ->
        print_s [%sexp "testing", (dir : Rounding_direction.t)];
        test_direction m ~dir)
    ;;

    let%expect_test ("int"[@tags "no-js", "64-bits-only"]) =
      test
        (module struct
          include Int

          let quickcheck_generator_incl = Base_quickcheck.Generator.int_inclusive
          let quickcheck_generator_log_incl = Base_quickcheck.Generator.int_log_inclusive
        end);
      [%expect
        {|
        (testing Up)
        (testing Down)
        (testing Zero)
        (testing Nearest) |}]
    ;;

    let%expect_test "int32" =
      test
        (module struct
          include Int32

          let quickcheck_generator_incl = Base_quickcheck.Generator.int32_inclusive

          let quickcheck_generator_log_incl =
            Base_quickcheck.Generator.int32_log_inclusive
          ;;
        end);
      [%expect
        {|
        (testing Up)
        (testing Down)
        (testing Zero)
        (testing Nearest) |}]
    ;;

    let%expect_test "int63" =
      test
        (module struct
          include Int63

          let quickcheck_generator_incl = Base_quickcheck.Generator.int63_inclusive

          let quickcheck_generator_log_incl =
            Base_quickcheck.Generator.int63_log_inclusive
          ;;
        end);
      [%expect
        {|
        (testing Up)
        (testing Down)
        (testing Zero)
        (testing Nearest) |}]
    ;;

    let%expect_test "int64" =
      test
        (module struct
          include Int64

          let quickcheck_generator_incl = Base_quickcheck.Generator.int64_inclusive

          let quickcheck_generator_log_incl =
            Base_quickcheck.Generator.int64_log_inclusive
          ;;
        end);
      [%expect
        {|
        (testing Up)
        (testing Down)
        (testing Zero)
        (testing Nearest) |}]
    ;;

    let%expect_test ("nativeint"[@tags "no-js", "64-bits-only"]) =
      test
        (module struct
          include Nativeint

          let quickcheck_generator_incl = Base_quickcheck.Generator.nativeint_inclusive

          let quickcheck_generator_log_incl =
            Base_quickcheck.Generator.nativeint_log_inclusive
          ;;
        end);
      [%expect
        {|
        (testing Up)
        (testing Down)
        (testing Zero)
        (testing Nearest) |}]
    ;;
  end)
;;

let%test_module "pow" =
  (module struct
    let%test _ = int_pow 0 0 = 1
    let%test _ = int_pow 0 1 = 0
    let%test _ = int_pow 10 1 = 10
    let%test _ = int_pow 10 2 = 100
    let%test _ = int_pow 10 3 = 1_000
    let%test _ = int_pow 10 4 = 10_000
    let%test _ = int_pow 10 5 = 100_000
    let%test _ = int_pow 2 10 = 1024
    let%test _ = int_pow 0 1_000_000 = 0
    let%test _ = int_pow 1 1_000_000 = 1
    let%test _ = int_pow (-1) 1_000_000 = 1
    let%test _ = int_pow (-1) 1_000_001 = -1

    let ( = ) = Int64.( = )

    let%test _ = int64_pow 0L 0L = 1L
    let%test _ = int64_pow 0L 1_000_000L = 0L
    let%test _ = int64_pow 1L 1_000_000L = 1L
    let%test _ = int64_pow (-1L) 1_000_000L = 1L
    let%test _ = int64_pow (-1L) 1_000_001L = -1L
    let%test _ = int64_pow 10L 1L = 10L
    let%test _ = int64_pow 10L 2L = 100L
    let%test _ = int64_pow 10L 3L = 1_000L
    let%test _ = int64_pow 10L 4L = 10_000L
    let%test _ = int64_pow 10L 5L = 100_000L
    let%test _ = int64_pow 2L 10L = 1_024L
    let%test _ = int64_pow 5L 27L = 7450580596923828125L

    let exception_thrown pow b e = Exn.does_raise (fun () -> pow b e)

    let%test _ = exception_thrown int_pow 10 60
    let%test _ = exception_thrown int64_pow 10L 60L
    let%test _ = exception_thrown int_pow 10 (-1)
    let%test _ = exception_thrown int64_pow 10L (-1L)
    let%test _ = exception_thrown int64_pow 2L 63L
    let%test _ = not (exception_thrown int64_pow 2L 62L)
    let%test _ = exception_thrown int64_pow (-2L) 63L
    let%test _ = not (exception_thrown int64_pow (-2L) 62L)
  end)
;;

let%test_module "overflow_bounds" =
  (module struct
    module Pow_overflow_bounds = Pow_overflow_bounds

    let%test _ = Int.equal Pow_overflow_bounds.overflow_bound_max_int_value Int.max_value

    let%test _ =
      Int64.equal Pow_overflow_bounds.overflow_bound_max_int64_value Int64.max_value
    ;;

    module Big_int = struct
      include Big_int

      let ( > ) = gt_big_int
      let ( = ) = eq_big_int
      let ( ^ ) = power_big_int_positive_int
      let ( + ) = add_big_int
      let one = unit_big_int
      let to_string = string_of_big_int
    end

    let test_overflow_table tbl conv max_val =
      assert (Array.length tbl = 64);
      let max_val = conv max_val in
      Array.iteri tbl ~f:(fun i max_base ->
        let max_base = conv max_base in
        let overflows b = Big_int.(b ^ i > max_val) in
        let is_ok =
          if i = 0
          then Big_int.(max_base = max_val)
          else (not (overflows max_base)) && overflows Big_int.(max_base + one)
        in
        if not is_ok
        then
          Printf.failwithf
            "overflow table check failed for %s (index %d)"
            (Big_int.to_string max_base)
            i
            ())
    ;;

    let%test_unit _ =
      test_overflow_table
        Pow_overflow_bounds.int_positive_overflow_bounds
        Big_int.big_int_of_int
        Int.max_value
    ;;

    let%test_unit _ =
      test_overflow_table
        Pow_overflow_bounds.int64_positive_overflow_bounds
        Big_int.big_int_of_int64
        Int64.max_value
    ;;
  end)
;;
