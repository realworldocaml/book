[%%import "config.h"]

open Base
open Stdio

let test ~op ~op_name ~to_string x = printf "%s %s = %d\n" op_name (to_string x) (op x)
let numbers = [ 0 (* Int.num_bits *); 1; 7; 2; 4; 12; 18; -1 ]

let%expect_test "ctz int64" =
  let open Int64 in
  let numbers = List.map numbers ~f:of_int in
  let f =
    test
      ~op:Ocaml_intrinsics.Int64.count_trailing_zeros
      ~op_name:"ctz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x7fff_ffff_ffff_ffff = 0
    ctz -0x8000_0000_0000_0000 = 63
    ctz 0x0 = 64
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz -0x1 = 0
    |}]
;;

let%expect_test "ctz int32" =
  let open Int32 in
  let numbers = List.map numbers ~f:of_int_trunc in
  let f =
    test
      ~op:Ocaml_intrinsics.Int32.count_trailing_zeros
      ~op_name:"ctz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x7fff_ffff = 0
    ctz -0x8000_0000 = 31
    ctz 0x0 = 32
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz -0x1 = 0
    |}]
;;

[%%ifdef JSC_ARCH_SIXTYFOUR]

let%expect_test "ctz int" =
  let open Int in
  let f =
    test
      ~op:Ocaml_intrinsics.Int.count_trailing_zeros
      ~op_name:"ctz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x3fff_ffff_ffff_ffff = 0
    ctz -0x4000_0000_0000_0000 = 62
    ctz 0x0 = 63
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz -0x1 = 0
    |}]
;;

let%expect_test "ctz nativeint" =
  let open Nativeint in
  let numbers = List.map numbers ~f:of_int in
  let f =
    test
      ~op:Ocaml_intrinsics.Nativeint.count_trailing_zeros
      ~op_name:"ctz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x7fff_ffff_ffff_ffff = 0
    ctz -0x8000_0000_0000_0000 = 63
    ctz 0x0 = 64
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz -0x1 = 0
    |}]
;;

[%%else]

let%expect_test "ctz int" =
  let open Int in
  let f =
    test
      ~op:Ocaml_intrinsics.Int.count_trailing_zeros
      ~op_name:"ctz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x3fff_ffff = 0
    ctz -0x4000_0000 = 30
    ctz 0x0 = 31
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz -0x1 = 0
    |}]
;;

let%expect_test "ctz nativeint" =
  let open Nativeint in
  let numbers = List.map numbers ~f:of_int in
  let f =
    test
      ~op:Ocaml_intrinsics.Nativeint.count_trailing_zeros
      ~op_name:"ctz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x7fff_ffff = 0
    ctz -0x8000_0000 = 31
    ctz 0x0 = 32
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz -0x1 = 0
    |}]
;;

[%%endif]
