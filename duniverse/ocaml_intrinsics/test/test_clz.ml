[%%import "config.h"]

open Base
open Stdio

let test ~op ~op_name ~to_string x = printf "%s %s = %d\n" op_name (to_string x) (op x)

let%expect_test "clz int64" =
  let open Int64 in
  let numbers =
    [ 0L (* Int.num_bits *)
    ; 1L (* Int.num_bits - 1 *)
    ; 7L (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1L
    ]
  in
  let f =
    test
      ~op:Ocaml_intrinsics.Int64.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 64
    clz 0x1 = 63
    clz 0x7 = 61
    clz 0x7fff_ffff_ffff_ffff = 1
    clz -0x8000_0000_0000_0000 = 0
    clz -0x1 = 0
    |}]
;;

let%expect_test "clz int32" =
  let open Int32 in
  let numbers =
    [ 0l (* Int.num_bits *)
    ; 1l (* Int.num_bits - 1 *)
    ; 7l (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1l
    ]
  in
  let f =
    test
      ~op:Ocaml_intrinsics.Int32.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 32
    clz 0x1 = 31
    clz 0x7 = 29
    clz 0x7fff_ffff = 1
    clz -0x8000_0000 = 0
    clz -0x1 = 0
    |}]
;;

[%%ifdef JSC_ARCH_SIXTYFOUR]

let%expect_test "clz int" =
  let open Int in
  let numbers =
    [ 0 (* Int.num_bits *)
    ; 1 (* Int.num_bits - 1 *)
    ; 7 (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1
    ]
  in
  let f =
    test
      ~op:Ocaml_intrinsics.Int.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 63
    clz 0x1 = 62
    clz 0x7 = 60
    clz 0x3fff_ffff_ffff_ffff = 1
    clz -0x4000_0000_0000_0000 = 0
    clz -0x1 = 0
    |}];
  let f =
    test
      ~op:Ocaml_intrinsics.Int.count_leading_zeros2
      ~op_name:"clz2"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz2 0x0 = 63
    clz2 0x1 = 62
    clz2 0x7 = 60
    clz2 0x3fff_ffff_ffff_ffff = 1
    clz2 -0x4000_0000_0000_0000 = 0
    clz2 -0x1 = 0
    |}]
;;

let%expect_test "clz nativeint" =
  let open Nativeint in
  let numbers =
    [ 0n (* Int.num_bits *)
    ; 1n (* Int.num_bits - 1 *)
    ; 7n (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1n
    ]
  in
  let f =
    test
      ~op:Ocaml_intrinsics.Nativeint.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 64
    clz 0x1 = 63
    clz 0x7 = 61
    clz 0x7fff_ffff_ffff_ffff = 1
    clz -0x8000_0000_0000_0000 = 0
    clz -0x1 = 0
    |}]
;;

[%%else]

let%expect_test "clz int" =
  let open Int in
  let numbers =
    [ 0 (* Int.num_bits *)
    ; 1 (* Int.num_bits - 1 *)
    ; 7 (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1
    ]
  in
  let f =
    test
      ~op:Ocaml_intrinsics.Int.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 31
    clz 0x1 = 30
    clz 0x7 = 28
    clz 0x3fff_ffff = 1
    clz -0x4000_0000 = 0
    clz -0x1 = 0
    |}];
  let f =
    test
      ~op:Ocaml_intrinsics.Int.count_leading_zeros2
      ~op_name:"clz2"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz2 0x0 = 31
    clz2 0x1 = 30
    clz2 0x7 = 28
    clz2 0x3fff_ffff = 1
    clz2 -0x4000_0000 = 0
    clz2 -0x1 = 0
    |}]
;;

let%expect_test "clz nativeint" =
  let open Nativeint in
  let numbers =
    [ 0n (* Int.num_bits *)
    ; 1n (* Int.num_bits - 1 *)
    ; 7n (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1n
    ]
  in
  let f =
    test
      ~op:Ocaml_intrinsics.Nativeint.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 32
    clz 0x1 = 31
    clz 0x7 = 29
    clz 0x7fff_ffff = 1
    clz -0x8000_0000 = 0
    clz -0x1 = 0
    |}]
;;

[%%endif]
