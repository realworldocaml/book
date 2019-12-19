open! Import
open! Int32

let of_ints = List.map ~f:of_int_exn
let examples = of_ints [ -1; 0; 1; 2; 3; 4; 5; 7; 8; 9; 63; 64; 65 ]
let examples_64_bit = [ min_value; succ min_value; pred max_value; max_value ]

let print_for ints f =
  List.iter ints ~f:(fun i ->
    print_s
      [%message
        "" ~_:(i : int32) ~_:(Or_error.try_with (fun () -> f i) : int Or_error.t)])
;;

let%expect_test "[floor_log2]" =
  print_for examples floor_log2;
  [%expect
    {|
    (-1 (Error ("[Int32.floor_log2] got invalid input" -1)))
    (0 (Error ("[Int32.floor_log2] got invalid input" 0)))
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
    (-2_147_483_648 (Error ("[Int32.floor_log2] got invalid input" -2147483648)))
    (-2_147_483_647 (Error ("[Int32.floor_log2] got invalid input" -2147483647)))
    (2_147_483_646 (Ok 30))
    (2_147_483_647 (Ok 30)) |}]
;;

let%expect_test "[ceil_log2]" =
  print_for examples ceil_log2;
  [%expect
    {|
    (-1 (Error ("[Int32.ceil_log2] got invalid input" -1)))
    (0 (Error ("[Int32.ceil_log2] got invalid input" 0)))
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
    (-2_147_483_648 (Error ("[Int32.ceil_log2] got invalid input" -2147483648)))
    (-2_147_483_647 (Error ("[Int32.ceil_log2] got invalid input" -2147483647)))
    (2_147_483_646 (Ok 31))
    (2_147_483_647 (Ok 31)) |}]
;;

let%test_module "int_math" =
  (module struct
    let test_cases () =
      of_ints
        [ 0b10101010
        ; 0b1010101010101010
        ; 0b101010101010101010101010
        ; 0b10000000
        ; 0b1000000000001000
        ; 0b100000000000000000001000
        ]
    ;;

    let%test_unit "ceil_pow2" =
      List.iter (test_cases ()) ~f:(fun x ->
        let p2 = ceil_pow2 x in
        assert (is_pow2 p2 && p2 >= x && x >= p2 / of_int_exn 2))
    ;;

    let%test_unit "floor_pow2" =
      List.iter (test_cases ()) ~f:(fun x ->
        let p2 = floor_pow2 x in
        assert (is_pow2 p2 && of_int_exn 2 * p2 >= x && x >= p2))
    ;;
  end)
;;
