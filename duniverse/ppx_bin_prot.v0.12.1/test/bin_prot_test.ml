open Bigarray

open Printf
open OUnit

open Bin_prot
open Common
open Utils
open ReadError
open Type_class
open Bin_prot.Std

module Bigstring = struct
  type t = buf

  let create = create_buf

  let of_string str =
    let len = String.length str in
    let buf = create len in
    blit_string_buf str buf ~len;
    buf

  let length buf = Array1.dim buf
end

let expect_exc test_exc f =
  try
    ignore (f ());
    false
  with
  | exc -> test_exc exc

let expect_bounds_error f =
  let test_exc = function
    | Invalid_argument "index out of bounds" -> true
    | _ -> false
  in
  expect_exc test_exc f

let expect_buffer_short f =
  let exc = Buffer_short in
  expect_exc ((=) exc) f

let expect_read_error exp_re exp_pos f =
  let test_exc = function
    | Read_error (re, pos) -> exp_re = re && exp_pos = pos
    | _ -> false
  in
  expect_exc test_exc f

let expect_no_error f =
  try
    ignore (f ());
    true
  with
  | _ -> false

let check_write_bounds_checks name buf write arg =
  (name ^ ": negative bound") @?
    expect_bounds_error (fun () -> write buf ~pos:~-1 arg);
  (name ^ ": positive bound") @?
    expect_buffer_short (fun () -> write buf ~pos:(Bigstring.length buf) arg)

let check_read_bounds_checks name buf read =
  (name ^ ": negative bound") @?
    expect_bounds_error (fun () -> read buf ~pos_ref:(ref ~-1));
  (name ^ ": positive bound") @?
    expect_buffer_short (fun () -> read buf ~pos_ref:(ref (Bigstring.length buf)))

let check_write_result name buf pos write arg exp_len =
  let res_pos = write buf ~pos arg in
  sprintf "%s: returned wrong write position (%d, expected %d)"
    name res_pos (pos + exp_len)
    @? (res_pos = pos + exp_len)

let check_read_result name buf pos read exp_ret exp_len =
  let pos_ref = ref pos in
  (name ^ ": returned wrong result") @? (read buf ~pos_ref = exp_ret);
  sprintf "%s: returned wrong read position (%d, expected %d)"
    name !pos_ref (pos + exp_len)
    @? (!pos_ref - pos = exp_len)

let check_all_args tp_name read write buf args =
  let write_name = "write_" ^ tp_name ^ " " in
  let read_name = "read_" ^ tp_name ^ " " in
  let buf_len = Bigstring.length buf in
  let act (arg, str_arg, arg_len) =
    let write_name_arg = write_name ^ str_arg in
    let read_name_arg = read_name ^ str_arg in
    for pos = 0 to 8 do
      check_write_bounds_checks write_name buf write arg;
      check_read_bounds_checks read_name buf read;
      check_write_result write_name_arg buf pos write arg arg_len;
      check_read_result read_name_arg buf pos read arg arg_len;
    done;
    (write_name_arg ^ ": write failed near bound") @? expect_no_error (fun () ->
      write buf ~pos:(buf_len - arg_len) arg);
    (read_name_arg ^ ": read failed near bound") @? expect_no_error (fun () ->
      if read buf ~pos_ref:(ref (buf_len - arg_len)) <> arg then
        failwith (read_name_arg ^ ": read near bound returned wrong result"));
    let small_buf = Array1.sub buf 0 (buf_len - 1) in
    (write_name_arg ^ ": write exceeds bound") @? expect_buffer_short (fun () ->
      write small_buf ~pos:(buf_len - arg_len) arg);
    (read_name_arg ^ ": read exceeds bound") @? expect_buffer_short (fun () ->
      read small_buf ~pos_ref:(ref (buf_len - arg_len)))
  in
  List.iter act args

let mk_buf n =
  let bstr = Bigstring.create n in
  for i = 0 to n - 1 do bstr.{i} <- '\255' done;
  bstr

let check_all extra_buf_size tp_name read write args =
  let buf_len = extra_buf_size + 8 in
  let buf = mk_buf buf_len in
  match args with
  | [] -> assert false
  | (arg, _, _) :: _ ->
      let write_name = "write_" ^ tp_name in
      check_write_bounds_checks write_name buf write arg;
      let read_name = "read_" ^ tp_name in
      check_read_bounds_checks read_name buf read;
      check_all_args tp_name read write buf args

let random_string n =
  String.init n (fun _ -> Char.chr (Random.int 256))

let mk_int_test ~n ~len = n, Printf.sprintf "%x" n, len
let mk_nat0_test ~n ~len = Nat0.of_int n, Printf.sprintf "%x" n, len
let mk_float_test n = n, Printf.sprintf "%g" n, 8
let mk_int32_test ~n ~len = n, Printf.sprintf "%lx" n, len
let mk_int64_test ~n ~len = n, Printf.sprintf "%Lx" n, len
let mk_nativeint_test ~n ~len = n, Printf.sprintf "%nx" n, len

let mk_gen_float_vec tp n =
  let vec = Array1.create tp fortran_layout n in
  for i = 1 to n do
    vec.{i} <- float i
  done;
  vec

let mk_float32_vec = mk_gen_float_vec float32
let mk_float64_vec = mk_gen_float_vec float64

let mk_bigstring n =
  let bstr = Array1.create char c_layout n in
  for i = 0 to n - 1 do
    bstr.{i} <- Char.chr (Random.int 256)
  done;
  bstr

let mk_gen_float_mat tp m n =
  let mat = Array2.create tp fortran_layout m n in
  let fn = float m in
  for c = 1 to n do
    let ofs = float (c - 1) *. fn in
    for r = 1 to m do
      mat.{r, c} <- ofs +. float r
    done;
  done;
  mat

let mk_float32_mat = mk_gen_float_mat float32
let mk_float64_mat = mk_gen_float_mat float64

let test =
  "Bin_prot" >:::
    [
      "unit" >::
        (fun () ->
          check_all 1 "unit" Read.bin_read_unit Write.bin_write_unit
            [
              ((), "()", 1);
            ];
        );

      "bool" >::
        (fun () ->
          check_all 1 "bool" Read.bin_read_bool Write.bin_write_bool
            [
              (true, "true", 1);
              (false, "false", 1);
            ];
        );

      "string" >::
        (fun () ->
          check_all 66000 "string" Read.bin_read_string Write.bin_write_string
            [
              ("", "\"\"", 1);
              (random_string 1, "random 1", 1 + 1);
              (random_string 10, "random 10", 10 + 1);
              (random_string 127, "random 127", 127 + 1);
              (random_string 128, "long 128", 128 + 3);
              (random_string 65535, "long 65535", 65535 + 3);
              (random_string 65536, "long 65536", 65536 + 5);
            ];

          if Sys.word_size = 32 then
            let bad_buf = Bigstring.of_string "\253\252\255\255\000" in
            "String_too_long" @? expect_read_error String_too_long 0 (fun () ->
              Read.bin_read_string bad_buf ~pos_ref:(ref 0));
            let bad_buf = Bigstring.of_string "\253\251\255\255\000" in
            "StringMaximimum" @? expect_buffer_short (fun () ->
              Read.bin_read_string bad_buf ~pos_ref:(ref 0))
          else
            let bad_buf = Bigstring.of_string "\252\248\255\255\255\255\255\255\001" in
            "String_too_long" @? expect_read_error String_too_long 0 (fun () ->
              Read.bin_read_string bad_buf ~pos_ref:(ref 0));
            let bad_buf = Bigstring.of_string "\252\247\255\255\255\255\255\255\001" in
            "StringMaximimum" @? expect_buffer_short (fun () ->
              Read.bin_read_string bad_buf ~pos_ref:(ref 0))

        );

      "char" >::
        (fun () ->
          check_all 1 "char" Read.bin_read_char Write.bin_write_char
            [
              ('x', "x", 1);
              ('y', "y", 1);
            ];
        );

      "int" >::
        (fun () ->
          let small_int_tests =
            [
              mk_int_test ~n:~-0x01 ~len:2;
              mk_int_test ~n:  0x00 ~len:1;
              mk_int_test ~n:  0x01 ~len:1;

              mk_int_test ~n:0x7e ~len:1;
              mk_int_test ~n:0x7f ~len:1;
              mk_int_test ~n:0x80 ~len:3;
              mk_int_test ~n:0x81 ~len:3;

              mk_int_test ~n:0x7ffe ~len:3;
              mk_int_test ~n:0x7fff ~len:3;
              mk_int_test ~n:0x8000 ~len:5;
              mk_int_test ~n:0x8001 ~len:5;

              mk_int_test ~n:0x3ffffffe ~len:5;
              mk_int_test ~n:0x3fffffff ~len:5;

              mk_int_test ~n:~-0x7f ~len:2;
              mk_int_test ~n:~-0x80 ~len:2;
              mk_int_test ~n:~-0x81 ~len:3;
              mk_int_test ~n:~-0x82 ~len:3;

              mk_int_test ~n:~-0x7fff ~len:3;
              mk_int_test ~n:~-0x8000 ~len:3;
              mk_int_test ~n:~-0x8001 ~len:5;
              mk_int_test ~n:~-0x8002 ~len:5;

              mk_int_test ~n:~-0x40000001 ~len:5;
              mk_int_test ~n:~-0x40000000 ~len:5;
            ]
          in
          let all_int_tests =
            if Sys.word_size = 32 then small_int_tests
            else
              mk_int_test ~n:(int_of_string "0x7ffffffe") ~len:5 ::
                mk_int_test ~n:(int_of_string "0x7fffffff") ~len:5 ::
                mk_int_test ~n:(int_of_string "0x80000000") ~len:9 ::
                mk_int_test ~n:(int_of_string "0x80000001") ~len:9 ::
                mk_int_test ~n:max_int ~len:9 ::

                mk_int_test ~n:(int_of_string "-0x000000007fffffff") ~len:5 ::
                mk_int_test ~n:(int_of_string "-0x0000000080000000") ~len:5 ::
                mk_int_test ~n:(int_of_string "-0x0000000080000001") ~len:9 ::
                mk_int_test ~n:(int_of_string "-0x0000000080000002") ~len:9 ::
                mk_int_test ~n:min_int ~len:9 ::

                small_int_tests
          in
          check_all 9 "int" Read.bin_read_int Write.bin_write_int
            all_int_tests;

          let bad_buf = Bigstring.of_string "\132" in
          "Int_code" @? expect_read_error Int_code 0 (fun () ->
            Read.bin_read_int bad_buf ~pos_ref:(ref 0));
          if Sys.word_size = 32 then
            let bad_buf = Bigstring.of_string "\253\255\255\255\064" in
            "Int_overflow (positive)" @? expect_read_error Int_overflow 0 (fun () ->
              Read.bin_read_int bad_buf ~pos_ref:(ref 0));
            let bad_buf = Bigstring.of_string "\253\255\255\255\191" in
            "Int_overflow (negative)" @? expect_read_error Int_overflow 0 (fun () ->
              Read.bin_read_int bad_buf ~pos_ref:(ref 0))
          else
            let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\255\064" in
            "Int_overflow (positive)" @? expect_read_error Int_overflow 0 (fun () ->
              Read.bin_read_int bad_buf ~pos_ref:(ref 0));
            let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\255\191" in
            "Int_overflow (negative)" @? expect_read_error Int_overflow 0 (fun () ->
              Read.bin_read_int bad_buf ~pos_ref:(ref 0))

        );

      "nat0" >::
        (fun () ->
          let small_int_tests =
            [
              mk_nat0_test ~n:0x00 ~len:1;
              mk_nat0_test ~n:0x01 ~len:1;

              mk_nat0_test ~n:0x7e ~len:1;
              mk_nat0_test ~n:0x7f ~len:1;
              mk_nat0_test ~n:0x80 ~len:3;
              mk_nat0_test ~n:0x81 ~len:3;

              mk_nat0_test ~n:0x7fff ~len:3;
              mk_nat0_test ~n:0x8000 ~len:3;
              mk_nat0_test ~n:0xffff ~len:3;
              mk_nat0_test ~n:0x10000 ~len:5;
              mk_nat0_test ~n:0x10001 ~len:5;

              mk_nat0_test ~n:0x3ffffffe ~len:5;
              mk_nat0_test ~n:0x3fffffff ~len:5;
            ]
          in
          let all_int_tests =
            if Sys.word_size = 32 then small_int_tests
            else
              mk_nat0_test ~n:(int_of_string "0x7fffffff") ~len:5 ::
                mk_nat0_test ~n:(int_of_string "0x80000000") ~len:5 ::
                mk_nat0_test ~n:(int_of_string "0xffffffff") ~len:5 ::
                mk_nat0_test ~n:(int_of_string "0x100000000") ~len:9 ::
                mk_nat0_test ~n:(int_of_string "0x100000001") ~len:9 ::
                mk_nat0_test ~n:max_int ~len:9 ::

                small_int_tests
          in
          check_all 9 "nat0" Read.bin_read_nat0 Write.bin_write_nat0
            all_int_tests;

          let bad_buf = Bigstring.of_string "\128" in
          "Nat0_code" @? expect_read_error Nat0_code 0 (fun () ->
            Read.bin_read_nat0 bad_buf ~pos_ref:(ref 0));

          if Sys.word_size = 32 then
            let bad_buf = Bigstring.of_string "\253\255\255\255\064" in
            "Nat0_overflow" @? expect_read_error Nat0_overflow 0 (fun () ->
              Read.bin_read_nat0 bad_buf ~pos_ref:(ref 0))
          else
            let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\255\064" in
            "Nat0_overflow" @? expect_read_error Nat0_overflow 0 (fun () ->
              Read.bin_read_nat0 bad_buf ~pos_ref:(ref 0))

        );

      "float" >::
        (fun () ->
          let float_tests =
            [
              mk_float_test 0.;
              mk_float_test (-0.);
              mk_float_test (-1.);
              mk_float_test 1.;
              mk_float_test infinity;
              mk_float_test (-.infinity);
              mk_float_test 1e-310;  (* subnormal *)
              mk_float_test (-1e-310);  (* subnormal *)
              mk_float_test 3.141595;
            ]
          in
          check_all 8 "float" Read.bin_read_float Write.bin_write_float
            float_tests
        );

      "int32" >::
        (fun () ->
          let int32_tests =
            [
              mk_int32_test ~n:(-0x01l) ~len:2;
              mk_int32_test ~n:  0x00l  ~len:1;
              mk_int32_test ~n:  0x01l  ~len:1;

              mk_int32_test ~n:0x7el ~len:1;
              mk_int32_test ~n:0x7fl ~len:1;
              mk_int32_test ~n:0x80l ~len:3;
              mk_int32_test ~n:0x81l ~len:3;

              mk_int32_test ~n:0x7ffel ~len:3;
              mk_int32_test ~n:0x7fffl ~len:3;
              mk_int32_test ~n:0x8000l ~len:5;
              mk_int32_test ~n:0x8001l ~len:5;

              mk_int32_test ~n:0x7ffffffel ~len:5;
              mk_int32_test ~n:0x7fffffffl ~len:5;

              mk_int32_test ~n:(-0x7fl) ~len:2;
              mk_int32_test ~n:(-0x80l) ~len:2;
              mk_int32_test ~n:(-0x81l) ~len:3;
              mk_int32_test ~n:(-0x82l) ~len:3;

              mk_int32_test ~n:(-0x7fffl) ~len:3;
              mk_int32_test ~n:(-0x8000l) ~len:3;
              mk_int32_test ~n:(-0x8001l) ~len:5;
              mk_int32_test ~n:(-0x8002l) ~len:5;

              mk_int32_test ~n:(-0x80000001l) ~len:5;
              mk_int32_test ~n:(-0x80000000l) ~len:5;
            ]
          in
          check_all 5 "int32" Read.bin_read_int32 Write.bin_write_int32
            int32_tests;

          let bad_buf = Bigstring.of_string "\132" in
          "Int32_code" @? expect_read_error Int32_code 0 (fun () ->
            Read.bin_read_int32 bad_buf ~pos_ref:(ref 0))
        );

      "int64" >::
        (fun () ->
          let int64_tests =
            [
              mk_int64_test ~n:(-0x01L) ~len:2;
              mk_int64_test ~n:  0x00L  ~len:1;
              mk_int64_test ~n:  0x01L  ~len:1;

              mk_int64_test ~n:0x7eL ~len:1;
              mk_int64_test ~n:0x7fL ~len:1;
              mk_int64_test ~n:0x80L ~len:3;
              mk_int64_test ~n:0x81L ~len:3;

              mk_int64_test ~n:0x7ffeL ~len:3;
              mk_int64_test ~n:0x7fffL ~len:3;
              mk_int64_test ~n:0x8000L ~len:5;
              mk_int64_test ~n:0x8001L ~len:5;

              mk_int64_test ~n:0x7ffffffeL ~len:5;
              mk_int64_test ~n:0x7fffffffL ~len:5;
              mk_int64_test ~n:0x80000000L ~len:9;
              mk_int64_test ~n:0x80000001L ~len:9;

              mk_int64_test ~n:0x7ffffffffffffffeL ~len:9;
              mk_int64_test ~n:0x7fffffffffffffffL ~len:9;

              mk_int64_test ~n:(-0x7fL) ~len:2;
              mk_int64_test ~n:(-0x80L) ~len:2;
              mk_int64_test ~n:(-0x81L) ~len:3;
              mk_int64_test ~n:(-0x82L) ~len:3;

              mk_int64_test ~n:(-0x7fffL) ~len:3;
              mk_int64_test ~n:(-0x8000L) ~len:3;
              mk_int64_test ~n:(-0x8001L) ~len:5;
              mk_int64_test ~n:(-0x8002L) ~len:5;

              mk_int64_test ~n:(-0x7fffffffL) ~len:5;
              mk_int64_test ~n:(-0x80000000L) ~len:5;
              mk_int64_test ~n:(-0x80000001L) ~len:9;
              mk_int64_test ~n:(-0x80000002L) ~len:9;

              mk_int64_test ~n:(-0x8000000000000001L) ~len:9;
              mk_int64_test ~n:(-0x8000000000000000L) ~len:9;
            ]
          in
          check_all 9 "int64" Read.bin_read_int64 Write.bin_write_int64
            int64_tests;

          let bad_buf = Bigstring.of_string "\132" in
          "Int64_code" @? expect_read_error Int64_code 0 (fun () ->
            Read.bin_read_int64 bad_buf ~pos_ref:(ref 0))
        );

      "nativeint" >::
        (fun () ->
          let small_nativeint_tests =
            [
              mk_nativeint_test ~n:(-0x01n) ~len:2;
              mk_nativeint_test ~n:  0x00n  ~len:1;
              mk_nativeint_test ~n:  0x01n  ~len:1;

              mk_nativeint_test ~n:0x7en ~len:1;
              mk_nativeint_test ~n:0x7fn ~len:1;
              mk_nativeint_test ~n:0x80n ~len:3;
              mk_nativeint_test ~n:0x81n ~len:3;

              mk_nativeint_test ~n:0x7ffen ~len:3;
              mk_nativeint_test ~n:0x7fffn ~len:3;
              mk_nativeint_test ~n:0x8000n ~len:5;
              mk_nativeint_test ~n:0x8001n ~len:5;

              mk_nativeint_test ~n:0x7ffffffen ~len:5;
              mk_nativeint_test ~n:0x7fffffffn ~len:5;

              mk_nativeint_test ~n:(-0x7fn) ~len:2;
              mk_nativeint_test ~n:(-0x80n) ~len:2;
              mk_nativeint_test ~n:(-0x81n) ~len:3;
              mk_nativeint_test ~n:(-0x82n) ~len:3;

              mk_nativeint_test ~n:(-0x7fffn) ~len:3;
              mk_nativeint_test ~n:(-0x8000n) ~len:3;
              mk_nativeint_test ~n:(-0x8001n) ~len:5;
              mk_nativeint_test ~n:(-0x8002n) ~len:5;

              mk_nativeint_test ~n:(-0x7fffffffn) ~len:5;
              mk_nativeint_test ~n:(-0x80000000n) ~len:5;
            ]
          in
          let nativeint_tests =
            if Sys.word_size = 32 then small_nativeint_tests
            else
              mk_nativeint_test ~n:0x80000000n ~len:9 ::
                mk_nativeint_test ~n:0x80000001n ~len:9 ::

                mk_nativeint_test ~n:(-0x80000001n) ~len:9 ::
                mk_nativeint_test ~n:(-0x80000002n) ~len:9 ::

                mk_nativeint_test ~n:(Nativeint.of_string "0x7ffffffffffffffe") ~len:9 ::
                mk_nativeint_test ~n:(Nativeint.of_string "0x7fffffffffffffff") ~len:9 ::

                mk_nativeint_test ~n:(Nativeint.of_string "-0x8000000000000001") ~len:9 ::
                mk_nativeint_test ~n:(Nativeint.of_string "-0x8000000000000000") ~len:9 ::

                small_nativeint_tests
          in

          let size = if Sys.word_size = 32 then 5 else 9 in
          check_all size "nativeint"
            Read.bin_read_nativeint Write.bin_write_nativeint
            nativeint_tests;

          let bad_buf = Bigstring.of_string "\251" in
          "Nativeint_code" @? expect_read_error Nativeint_code 0 (fun () ->
            Read.bin_read_nativeint bad_buf ~pos_ref:(ref 0));

          if Sys.word_size = 32 then
            let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\255\255" in
            "Nativeint_code (overflow)" @? expect_read_error Nativeint_code 0 (fun () ->
              Read.bin_read_nativeint bad_buf ~pos_ref:(ref 0))
        );

      "ref" >::
        (fun () ->
          check_all 1 "ref"
            (Read.bin_read_ref Read.bin_read_int)
            (Write.bin_write_ref Write.bin_write_int)
            [(ref 42, "ref 42", 1)];
        );

      "option" >::
        (fun () ->
          check_all 2 "option"
            (Read.bin_read_option Read.bin_read_int)
            (Write.bin_write_option Write.bin_write_int)
            [
              (Some 42, "Some 42", 2);
              (None, "None", 1);
            ];
        );

      "pair" >::
        (fun () ->
          check_all 9 "pair"
            (Read.bin_read_pair Read.bin_read_float Read.bin_read_int)
            (Write.bin_write_pair Write.bin_write_float Write.bin_write_int)
            [((3.141, 42), "(3.141, 42)", 9)];
        );

      "triple" >::
        (fun () ->
          check_all 14 "triple"
            (Read.bin_read_triple
               Read.bin_read_float Read.bin_read_int Read.bin_read_string)
            (Write.bin_write_triple
               Write.bin_write_float Write.bin_write_int Write.bin_write_string)
            [((3.141, 42, "test"), "(3.141, 42, \"test\")", 14)];
        );

      "list" >::
        (fun () ->
          check_all 12 "list"
            (Read.bin_read_list Read.bin_read_int)
            (Write.bin_write_list Write.bin_write_int)
            [
              ([42; -1; 200; 33000], "[42; -1; 200; 33000]", 12);
              ([], "[]", 1);
            ];
        );

      "array" >::
        (fun () ->
          let bin_read_int_array = Read.bin_read_array Read.bin_read_int in
          check_all 12 "array"
            bin_read_int_array
            (Write.bin_write_array Write.bin_write_int)
            [
              ([| 42; -1; 200; 33000 |], "[|42; -1; 200; 33000|]", 12);
              ([||], "[||]", 1);
            ];

          if Sys.word_size = 32 then
            let bad_buf = Bigstring.of_string "\253\000\000\064\000" in
            "Array_too_long" @? expect_read_error Array_too_long 0 (fun () ->
              bin_read_int_array bad_buf ~pos_ref:(ref 0));
            let bad_buf = Bigstring.of_string "\253\255\255\063\000" in
            "ArrayMaximimum" @? expect_buffer_short (fun () ->
              bin_read_int_array bad_buf ~pos_ref:(ref 0))
          else
            let bad_buf = Bigstring.of_string "\252\000\000\000\000\000\000\064\000" in
            "Array_too_long" @? expect_read_error Array_too_long 0 (fun () ->
              bin_read_int_array bad_buf ~pos_ref:(ref 0));
            let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\063\000" in
            "ArrayMaximimum" @? expect_buffer_short (fun () ->
              bin_read_int_array bad_buf ~pos_ref:(ref 0))

        );

      "float_array" >::
        (fun () ->
          check_all 33 "float_array"
            Read.bin_read_float_array Write.bin_write_float_array
            [
              ([| 42.; -1.; 200.; 33000. |], "[|42.; -1.; 200.; 33000.|]", 33);
              ([||], "[||]", 1);
            ];

          if Sys.word_size = 32 then
            let bad_buf = Bigstring.of_string "\253\000\000\032\000" in
            "Array_too_long (float)" @? expect_read_error Array_too_long 0 (fun () ->
              Read.bin_read_float_array bad_buf ~pos_ref:(ref 0));
            let bad_buf = Bigstring.of_string "\253\255\255\031\000" in
            "ArrayMaximimum (float)" @? expect_buffer_short (fun () ->
              Read.bin_read_float_array bad_buf ~pos_ref:(ref 0))
          else
            let bad_buf = Bigstring.of_string "\252\000\000\000\000\000\000\064\000" in
            "Array_too_long (float)" @? expect_read_error Array_too_long 0 (fun () ->
              Read.bin_read_float_array bad_buf ~pos_ref:(ref 0));
            let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\063\000" in
            "ArrayMaximimum (float)" @? expect_buffer_short (fun () ->
              Read.bin_read_float_array bad_buf ~pos_ref:(ref 0));

          (* Test that the binary forms of [float array] and [float_array] are the same *)
          let arrays =
            let rec loop acc len =
              if len < 0 then acc
              else
                let a = Array.init len (fun i -> float_of_int (i + len)) in
                let txt = Printf.sprintf "float array, len = %d" len in
                let buf = len * 8 + Size.bin_size_nat0 (Nat0.unsafe_of_int len) in
                loop ((a, txt, buf) :: acc) (len - 1)
            in
            loop [] 255 in
          let len = 255 * 8 + Size.bin_size_nat0 (Nat0.unsafe_of_int 255) in
          check_all len "float array -> float_array"
            Read.bin_read_float_array (Write.bin_write_array Write.bin_write_float)
            arrays;
          check_all len "float_array -> float array"
            (Read.bin_read_array Read.bin_read_float) (Write.bin_write_float_array)
            arrays;

          (* Check that the canonical closures used in the short circuit test of float
             arrays are indeed allocated closures as opposed to [compare] for example
             which is a primitive.  Even if it looks like a tautology, it is not. (for
             example, [compare == compare] is false. *)
          assert (bin_write_float == bin_write_float);
          assert (bin_read_float  == bin_read_float);
          assert (bin_size_float  == bin_size_float);
        );

      "hashtbl" >::
        (fun () ->
          let bindings = List.rev [(42, 3.); (17, 2.); (42, 4.)] in
          let htbl = Hashtbl.create (List.length bindings) in
          List.iter (fun (k, v) -> Hashtbl.add htbl k v) bindings;
          check_all 28 "hashtbl"
            (Read.bin_read_hashtbl Read.bin_read_int Read.bin_read_float)
            (Write.bin_write_hashtbl Write.bin_write_int Write.bin_write_float)
            [
              (htbl, "[(42, 3.); (17, 2.); (42, 4.)]", 28);
              (Hashtbl.create 0, "[]", 1)
            ];
        );

      "float32_vec" >::
        (fun () ->
          let n = 128 in
          let header = 3 in
          let size = header + n * 4 in
          let vec = mk_float32_vec n in
          check_all size "float32_vec"
            Read.bin_read_float32_vec
            Write.bin_write_float32_vec
            [
              (vec, "[| ... |]", size);
              (mk_float32_vec 0, "[||]", 1);
            ]
        );

      "float64_vec" >::
        (fun () ->
          let n = 127 in
          let header = 1 in
          let size = header + n * 8 in
          let vec = mk_float64_vec n in
          check_all size "float64_vec"
            Read.bin_read_float64_vec
            Write.bin_write_float64_vec
            [
              (vec, "[| ... |]", size);
              (mk_float64_vec 0, "[||]", 1);
            ]
        );

      "vec" >::
        (fun () ->
          let n = 128 in
          let header = 3 in
          let size = header + n * 8 in
          let vec = mk_float64_vec n in
          check_all size "vec"
            Read.bin_read_vec
            Write.bin_write_vec
            [
              (vec, "[| ... |]", size);
              (mk_float64_vec 0, "[||]", 1);
            ]
        );

      "float32_mat" >::
        (fun () ->
          let m = 128 in
          let n = 127 in
          let header = 3 + 1 in
          let size = header + m * n * 4 in
          let mat = mk_float32_mat m n in
          check_all size "float32_mat"
            Read.bin_read_float32_mat
            Write.bin_write_float32_mat
            [
              (mat, "[| ... |]", size);
              (mk_float32_mat 0 0, "[||]", 2);
            ]
        );

      "float64_mat" >::
        (fun () ->
          let m = 10 in
          let n = 12 in
          let header = 1 + 1 in
          let size = header + m * n * 8 in
          let mat = mk_float64_mat m n in
          check_all size "float64_mat"
            Read.bin_read_float64_mat
            Write.bin_write_float64_mat
            [
              (mat, "[| ... |]", size);
              (mk_float64_mat 0 0, "[||]", 2);
            ]
        );

      "mat" >::
        (fun () ->
          let m = 128 in
          let n = 128 in
          let header = 3 + 3 in
          let size = header + m * n * 8 in
          let mat = mk_float64_mat m n in
          check_all size "mat"
            Read.bin_read_mat
            Write.bin_write_mat
            [
              (mat, "[| ... |]", size);
              (mk_float64_mat 0 0, "[||]", 2);
            ]
        );

      "bigstring" >::
        (fun () ->
          let n = 128 in
          let header = 3 in
          let size = header + n in
          let bstr = mk_bigstring n in
          check_all size "bigstring"
            Read.bin_read_bigstring
            Write.bin_write_bigstring
            [
              (bstr, "[| ... |]", size);
              (mk_bigstring 0, "[||]", 1);
            ]
        );

      "bigstring (big)" >::
        (fun () ->
          (* [n] is a 16bits integer that will be serialized differently depending on
             whether it is considered as an integer or an unsigned integer. *)
          let n = 40_000 in
          let header = 3 in
          let size = header + n in
          let bstr = mk_bigstring n in
          check_all size "bigstring"
            Read.bin_read_bigstring
            Write.bin_write_bigstring
            [
              (bstr, "[| ... |]", size);
              (mk_bigstring 0, "[||]", 1);
            ]
        );

      "variant_tag" >::
        (fun () ->
          check_all 4 "variant_tag"
            Read.bin_read_variant_int
            Write.bin_write_variant_int
            [
              ((Obj.magic `Foo : int), "`Foo", 4);
              ((Obj.magic `Bar : int), "`Bar", 4);
            ];
          let bad_buf = Bigstring.of_string "\000\000\000\000" in
          "Variant_tag" @? expect_read_error Variant_tag 0 (fun () ->
            Read.bin_read_variant_int bad_buf ~pos_ref:(ref 0))
        );

      "int64_bits" >::
        (fun () ->
          check_all 8 "int64_bits"
            Read.bin_read_int64_bits
            Write.bin_write_int64_bits
            [
              (Int64.min_int, "min_int", 8);
              (Int64.add Int64.min_int Int64.one, "min_int + 1", 8);
              (Int64.minus_one, "-1", 8);
              (Int64.zero, "0", 8);
              (Int64.one, "1", 8);
              (Int64.sub Int64.max_int Int64.one, "max_int - 1", 8);
              (Int64.max_int, "max_int", 8);
            ];
        );

      "int_64bit" >::
        (fun () ->
          check_all 8 "int_64bit"
            Read.bin_read_int_64bit
            Write.bin_write_int_64bit
            [
              (min_int, "min_int", 8);
              (min_int + 1, "min_int + 1", 8);
              (-1, "-1", 8);
              (0, "0", 8);
              (1, "1", 8);
              (max_int - 1, "max_int - 1", 8);
              (max_int, "max_int", 8);
            ];
          let bad_buf_max =
            bin_dump bin_int64_bits.writer (Int64.succ (Int64.of_int max_int))
          in
          "Int_overflow (positive)" @? expect_read_error Int_overflow 0 (fun () ->
            Read.bin_read_int_64bit bad_buf_max ~pos_ref:(ref 0));
          let bad_buf_min =
            bin_dump bin_int64_bits.writer (Int64.pred (Int64.of_int min_int))
          in
          "Int_overflow (negative)" @? expect_read_error Int_overflow 0 (fun () ->
            Read.bin_read_int_64bit bad_buf_min ~pos_ref:(ref 0));
        );

      "network16_int" >::
        (fun () ->
          check_all 2 "network16_int"
            Read.bin_read_network16_int
            Write.bin_write_network16_int
            [
                (* No negative numbers - ambiguous on 64bit platforms *)
              (0, "0", 2);
              (1, "1", 2);
            ];
        );

      "network32_int" >::
        (fun () ->
          check_all 4 "network32_int"
            Read.bin_read_network32_int
            Write.bin_write_network32_int
            [
                (* No negative numbers - ambiguous on 64bit platforms *)
              (0, "0", 4);
              (1, "1", 4);
            ];
        );

      "network32_int32" >::
        (fun () ->
          check_all 4 "network32_int32"
            Read.bin_read_network32_int32
            Write.bin_write_network32_int32
            [
              (-1l, "-1", 4);
              (0l, "0", 4);
              (1l, "1", 4);
            ];
        );

      "network64_int" >::
        (fun () ->
          check_all 8 "network64_int"
            Read.bin_read_network64_int
            Write.bin_write_network64_int
            [
              (-1, "-1", 8);
              (0, "0", 8);
              (1, "1", 8);
            ];
        );

      "network64_int64" >::
        (fun () ->
          check_all 8 "network64_int64"
            Read.bin_read_network64_int64
            Write.bin_write_network64_int64
            [
              (-1L, "-1", 8);
              (0L, "0", 8);
              (1L, "1", 8);
            ];
        );
    ]

module Common = struct
  type tuple = float * string * int64
  [@@deriving bin_io]

  type 'a record = { a : int; b : 'a; c : 'a option }
  [@@deriving bin_io]

  type 'a singleton_record = { y : 'a }
  [@@deriving bin_io]

  type 'a inline_record =
    | IR of { mutable ir_a : int; ir_b : 'a; ir_c : 'a option }
    | Other of int
  [@@deriving bin_io]

  type 'a sum = Foo | Bar of int | Bla of 'a * string
  [@@deriving bin_io]

  type 'a variant = [ `Foo | `Bar of int | `Bla of 'a * string ]
  [@@deriving bin_io]

  type 'a poly_app = (tuple * int singleton_record * 'a record * 'a inline_record) variant sum list
  [@@deriving bin_io]

  type 'a rec_t1 = RecFoo1 of 'a rec_t2
  and 'a rec_t2 = RecFoo2 of 'a poly_app * 'a rec_t1 | RecNone
  [@@deriving bin_io]

  type 'a poly_id = 'a rec_t1
  [@@deriving bin_io]

  type el = float poly_id
  [@@deriving bin_io]

  type els = el array
  [@@deriving bin_io]

  let test =
    "Bin_prot_common" >:::
      [
        "Utils.bin_dump" >::
          (fun () ->
            let el =
              let record = { a = 17; b = 2.78; c = None } in
              let inline_record = IR {ir_a = 18; ir_b = 43210.; ir_c = None} in
              let arg = (3.1, "foo", 42L), { y = 4321 }, record, inline_record in
              let variant = `Bla (arg, "fdsa") in
              let sum = Bla (variant, "asdf") in
              let poly_app = [ sum ] in
              RecFoo1 (RecFoo2 (poly_app, RecFoo1 RecNone))
            in
            let els = Array.make 10 el in
            let buf = bin_dump ~header:true bin_els.writer els in

            let pos_ref = ref 0 in
            let els_len = Read.bin_read_int_64bit buf ~pos_ref in
            "pos_ref for length incorrect" @? (!pos_ref = 8);
            "els_len disagrees with bin_size" @? (els_len = bin_size_els els);
            let new_els = bin_read_els buf ~pos_ref in
            "new_els and els not equal" @? (els = new_els)
          );
      ]
end

module Inline = struct
  let compatible xs derived_tc inline_writer inline_reader inline_tc =
    ListLabels.map xs ~f:(fun x ->
      "" >:: fun () ->
        "incorrect size from inline writer"
        @? (derived_tc.writer.size x = inline_writer.size x);
        "incorrect size from inline type class"
        @? (derived_tc.writer.size x = inline_tc.writer.size x);

        let buf = bin_dump derived_tc.writer x in

        "incorrect bin dump from inline writer"
        @? (buf = bin_dump inline_writer x);
        "incorrect bin dump from inline type class"
        @? (buf = bin_dump inline_tc.writer x);

        let val_and_len reader =
          let pos_ref = ref 0 in
          let x = reader.read buf ~pos_ref in
          (x, !pos_ref)
        in
        let (_, len) = val_and_len derived_tc.reader in
        let (x', len') = val_and_len inline_reader in
        "incorrect value from inline reader" @? (x = x');
        "incorrect length from inline reader" @? (len = len');
        let (x', len') = val_and_len inline_tc.reader in
        "incorrect value from inline type class" @? (x = x');
        "incorrect length from inline type class" @? (len = len');
    )
  ;;

  type variant_extension = [ float Common.variant | `Baz of int * float ]
  [@@deriving bin_io]

  let test =
    "Bin_prot.Inline" >::: [
      "simple tuple" >:::
      compatible
        [(50.5, "hello", 1234L)]
        Common.bin_tuple
        [%bin_writer     : Common.tuple]
        [%bin_reader     : Common.tuple]
        [%bin_type_class : Common.tuple];

      "redefine tuple" >:::
      compatible
        [(50.5, "hello", 1234L)]
        Common.bin_tuple
        [%bin_writer     : float * string * int64]
        [%bin_reader     : float * string * int64]
        [%bin_type_class : float * string * int64];

      "simple variant" >:::
      compatible
        [`Foo; `Bar 8; `Bla (33.3, "world")]
        (Common.bin_variant bin_float)
        [%bin_writer     : float Common.variant]
        [%bin_reader     : float Common.variant]
        [%bin_type_class : float Common.variant];

      "redefine variant" >:::
      compatible
        [`Foo; `Bar 8; `Bla (33.3, "world")]
        (Common.bin_variant bin_float)
        [%bin_writer     : [`Foo | `Bar of int | `Bla of float * string]]
        [%bin_reader     : [`Foo | `Bar of int | `Bla of float * string]]
        [%bin_type_class : [`Foo | `Bar of int | `Bla of float * string]];

      "variant_extension" >:::
      compatible
        [`Foo; `Bar 8; `Bla (33.3, "world"); `Baz (17, 17.71)]
        bin_variant_extension
        [%bin_writer     : [ float Common.variant | `Baz of int * float ]]
        [%bin_reader     : [ float Common.variant | `Baz of int * float ]]
        [%bin_type_class : [ float Common.variant | `Baz of int * float ]];

      "sub variant" >:::
      compatible
        [ { Common. y = `Foo }; { y = `Bar 42 }; { y = `Bla (42, "world") } ]
        (Common.bin_singleton_record (Common.bin_variant bin_int))
        [%bin_writer     : [`Foo | `Bar of int | `Bla of int * string] Common.singleton_record]
        [%bin_reader     : [`Foo | `Bar of int | `Bla of int * string] Common.singleton_record]
        [%bin_type_class : [`Foo | `Bar of int | `Bla of int * string] Common.singleton_record];
    ]
  ;;
end
