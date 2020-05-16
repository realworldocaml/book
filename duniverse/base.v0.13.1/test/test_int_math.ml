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

module Make (X : Make_arg) : sig end = struct
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

include Make (Int)
include Make (Int32)
include Make (Int63)
include Make (Int64)
include Make (Nativeint)

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
