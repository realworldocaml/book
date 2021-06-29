open! Import
open! Int

let%expect_test ("hash coherence"[@tags "64-bits-only"]) =
  check_int_hash_coherence [%here] (module Int);
  [%expect {| |}]
;;

let%expect_test "[max_value_30_bits]" =
  print_s [%sexp (max_value_30_bits : t)];
  [%expect {|
    1_073_741_823 |}]
;;

let%expect_test "hex" =
  let test x =
    let n = Or_error.try_with (fun () -> Int.Hex.of_string x) in
    print_s [%message (n : int Or_error.t)]
  in
  test "0x1c5f";
  [%expect {|
    (n (Ok 7_263))
  |}];
  test "0x1c5f NON-HEX-GARBAGE";
  [%expect
    {|
    (n (
      Error (
        Failure
        "Base.Int.Hex.of_string: invalid input \"0x1c5f NON-HEX-GARBAGE\"")))
  |}]
;;

let%test_module "Hex" =
  (module struct
    let f (i, s_hum) =
      let s = String.filter s_hum ~f:(fun c -> not (Char.equal c '_')) in
      let sexp_hum = Sexp.Atom s_hum in
      let sexp = Sexp.Atom s in
      [%test_result: Sexp.t] ~message:"sexp_of_t" ~expect:sexp (Hex.sexp_of_t i);
      [%test_result: int] ~message:"t_of_sexp" ~expect:i (Hex.t_of_sexp sexp);
      [%test_result: int] ~message:"t_of_sexp[human]" ~expect:i (Hex.t_of_sexp sexp_hum);
      [%test_result: string] ~message:"to_string" ~expect:s (Hex.to_string i);
      [%test_result: string] ~message:"to_string_hum" ~expect:s_hum (Hex.to_string_hum i);
      [%test_result: int] ~message:"of_string" ~expect:i (Hex.of_string s);
      [%test_result: int] ~message:"of_string[human]" ~expect:i (Hex.of_string s_hum)
    ;;

    let%test_unit _ =
      List.iter
        ~f
        [ 0, "0x0"
        ; 1, "0x1"
        ; 2, "0x2"
        ; 5, "0x5"
        ; 10, "0xa"
        ; 16, "0x10"
        ; 254, "0xfe"
        ; 65_535, "0xffff"
        ; 65_536, "0x1_0000"
        ; 1_000_000, "0xf_4240"
        ; -1, "-0x1"
        ; -2, "-0x2"
        ; -1_000_000, "-0xf_4240"
        ; ( max_value
          , match num_bits with
          | 31 -> "0x3fff_ffff"
          | 32 -> "0x7fff_ffff"
          | 63 -> "0x3fff_ffff_ffff_ffff"
          | _ -> assert false )
        ; ( min_value
          , match num_bits with
          | 31 -> "-0x4000_0000"
          | 32 -> "-0x8000_0000"
          | 63 -> "-0x4000_0000_0000_0000"
          | _ -> assert false )
        ]
    ;;

    let%test_unit _ = [%test_result: int] (Hex.of_string "0XA") ~expect:10

    let%test_unit _ =
      match Option.try_with (fun () -> Hex.of_string "0") with
      | None -> ()
      | Some _ -> failwith "Hex must always have a 0x prefix."
    ;;

    let%test_unit _ =
      match Option.try_with (fun () -> Hex.of_string "0x_0") with
      | None -> ()
      | Some _ -> failwith "Hex may not have '_' before the first digit."
    ;;
  end)
;;

let%test _ = neg 5 + 5 = 0
let%test _ = pow min_value 1 = min_value
let%test _ = pow max_value 1 = max_value

let%test "comparisons" =
  let original_compare (x : int) y = Caml.compare x y in
  let valid_compare x y =
    let result = compare x y in
    let expect = original_compare x y in
    assert (Bool.( = ) (result < 0) (expect < 0));
    assert (Bool.( = ) (result > 0) (expect > 0));
    assert (Bool.( = ) (result = 0) (expect = 0));
    assert (result = expect)
  in
  valid_compare min_value min_value;
  valid_compare min_value (-1);
  valid_compare (-1) min_value;
  valid_compare min_value 0;
  valid_compare 0 min_value;
  valid_compare max_value (-1);
  valid_compare (-1) max_value;
  valid_compare max_value min_value;
  valid_compare max_value max_value;
  true
;;

let test f x = test_conversion ~to_string:Int.Hex.to_string_hum f x
let numbers = [ 0x10_20; 0x11_22_33; 0x11_22_33_1F; 0x11_22_33_44 ]

let%expect_test "bswap16" =
  List.iter numbers ~f:(test bswap16);
  [%expect
    {|
    0x1020 --> 0x2010
    0x11_2233 --> 0x3322
    0x1122_331f --> 0x1f33
    0x1122_3344 --> 0x4433 |}]
;;
