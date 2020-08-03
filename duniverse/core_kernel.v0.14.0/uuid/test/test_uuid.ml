open! Core_kernel
open! Uuid
open! Uuid.Private
open! Expect_test_helpers_core

let%expect_test _ =
  Quickcheck.test_distinct_values
    ~sexp_of:Unstable.sexp_of_t
    quickcheck_generator
    ~trials:1_024
    ~distinct_values:512
    ~compare;
  [%expect {||}]
;;

let%expect_test _ =
  Quickcheck.test ~sexp_of:sexp_of_t quickcheck_generator ~f:invariant;
  [%expect {||}]
;;

let%expect_test "All hex digits are correctly encoded in lowercase" =
  let conversions = List.init 16 ~f:(fun int -> int, bottom_4_bits_to_hex_char int) in
  print_s [%sexp (conversions : (int * Char.t) list)];
  [%expect
    {|
    ((0  0)
     (1  1)
     (2  2)
     (3  3)
     (4  4)
     (5  5)
     (6  6)
     (7  7)
     (8  8)
     (9  9)
     (10 a)
     (11 b)
     (12 c)
     (13 d)
     (14 e)
     (15 f))|}]
;;

let%test_module "create_rand does a reasonable creation" =
  (module struct
    let all_bytes_are_hex_or_dash t =
      String.for_all t ~f:(function
        | '-' | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    ;;

    let%expect_test "almost v4" =
      let state = Random.State.make [||] in
      let create_rand_and_verify () =
        let t = create_random state in
        is_valid_exn t;
        [%test_pred: String.t] all_bytes_are_hex_or_dash (t |> to_string);
        print_endline (t |> to_string)
      in
      for _ = 0 to 10 do
        create_rand_and_verify ()
      done;
      [%expect
        {|
      0b90ffe5-a5a2-4da5-2b62-a5d151b81f94
      4f772c29-2a7a-449b-9a9a-87c8da18e436
      6090f17f-c5b7-4fdf-9d9e-8bc29d3604c5
      cc6ee6e0-0c6d-47aa-4f42-a5d9ee94051a
      a1185b42-098e-45a3-6013-97758b2c4036
      628dfcfb-e6e7-42a9-bc53-0e69decd0bee
      9b5130dc-1c44-4810-e9f6-9801606b6716
      180cff8f-ab16-4081-d3cc-870fcff725ba
      330195b9-3a70-49a3-88ce-cf0e41f664a7
      ad20430d-84bf-4be2-1535-a8b753eb47dc
      166fca01-529d-4016-2173-a47fdfe432b9 |}]
    ;;
  end)
;;
