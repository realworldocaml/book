open! Import
open! Nativeint

let examples = [ -1n; 0n; 1n; 2n; 3n; 4n; 5n; 7n; 8n; 9n; 63n; 64n; 65n ]
let examples_64_bit = [ min_value; succ min_value; pred max_value; max_value ]

let print_for ints f =
  List.iter ints ~f:(fun i ->
    print_s
      [%message
        "" ~_:(i : nativeint) ~_:(Or_error.try_with (fun () -> f i) : int Or_error.t)])
;;

let%expect_test "[floor_log2]" =
  print_for examples floor_log2;
  [%expect
    {|
    (-1 (Error ("[Nativeint.floor_log2] got invalid input" -1)))
    (0 (Error ("[Nativeint.floor_log2] got invalid input" 0)))
    (1 (Ok 0))
    (2 (Ok 1))
    (3 (Ok 1))
    (4 (Ok 2))
    (5 (Ok 2))
    (7 (Ok 2))
    (8 (Ok 3))
    (9 (Ok 3))
    (63 (Ok 5))
    (64 (Ok 6))
    (65 (Ok 6)) |}]
;;

let%expect_test ("[floor_log2]"[@tags "64-bits-only"]) =
  print_for examples_64_bit floor_log2;
  [%expect
    {|
    (-9_223_372_036_854_775_808 (
      Error ("[Nativeint.floor_log2] got invalid input" -9223372036854775808)))
    (-9_223_372_036_854_775_807 (
      Error ("[Nativeint.floor_log2] got invalid input" -9223372036854775807)))
    (9_223_372_036_854_775_806 (Ok 62))
    (9_223_372_036_854_775_807 (Ok 62)) |}]
;;

let%expect_test "[ceil_log2]" =
  print_for examples ceil_log2;
  [%expect
    {|
    (-1 (Error ("[Nativeint.ceil_log2] got invalid input" -1)))
    (0 (Error ("[Nativeint.ceil_log2] got invalid input" 0)))
    (1 (Ok 0))
    (2 (Ok 1))
    (3 (Ok 2))
    (4 (Ok 2))
    (5 (Ok 3))
    (7 (Ok 3))
    (8 (Ok 3))
    (9 (Ok 4))
    (63 (Ok 6))
    (64 (Ok 6))
    (65 (Ok 7)) |}]
;;

let%expect_test ("[ceil_log2]"[@tags "64-bits-only"]) =
  print_for examples_64_bit ceil_log2;
  [%expect
    {|
    (-9_223_372_036_854_775_808 (
      Error ("[Nativeint.ceil_log2] got invalid input" -9223372036854775808)))
    (-9_223_372_036_854_775_807 (
      Error ("[Nativeint.ceil_log2] got invalid input" -9223372036854775807)))
    (9_223_372_036_854_775_806 (Ok 63))
    (9_223_372_036_854_775_807 (Ok 63)) |}]
;;

let%test_module "nativeint_math" =
  (module struct
    let test_cases () =
      let cases =
        [ 0b10101010n
        ; 0b1010101010101010n
        ; 0b101010101010101010101010n
        ; 0b10000000n
        ; 0b1000000000001000n
        ; 0b100000000000000000001000n
        ]
      in
      match Word_size.word_size with
      | W64 ->
        (* create some >32 bit values... *)
        (* We can't use literals directly because the compiler complains on 32 bits. *)
        let cases =
          cases
          @ [ (0b1010101010101010n lsl 16) lor 0b1010101010101010n
            ; (0b1000000000000000n lsl 16) lor 0b0000000000001000n
            ]
        in
        let added_cases = List.map cases ~f:(fun x -> x lsl 16) in
        List.concat [ cases; added_cases ]
      | W32 -> cases
    ;;

    let%test_unit "ceil_pow2" =
      List.iter (test_cases ()) ~f:(fun x ->
        let p2 = ceil_pow2 x in
        assert (is_pow2 p2 && p2 >= x && x >= p2 / of_int 2))
    ;;

    let%test_unit "floor_pow2" =
      List.iter (test_cases ()) ~f:(fun x ->
        let p2 = floor_pow2 x in
        assert (is_pow2 p2 && of_int 2 * p2 >= x && x >= p2))
    ;;
  end)
;;
