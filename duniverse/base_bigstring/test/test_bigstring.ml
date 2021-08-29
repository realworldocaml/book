open! Import
open Base_bigstring

let length = length
let create = create

module Bigstring_sequence = struct
  type nonrec t = t

  let create ~len = create len
  let get = get
  let set = set
  let length = length
end

module Bytes_sequence = struct
  type t = bytes [@@deriving sexp_of]

  let create ~len = Bytes.create len
  let get = Bytes.get
  let set = Bytes.set
  let length = Bytes.length
end

module Blit_elt = struct
  include Char

  let of_bool b = if b then 'a' else 'b'
end

module Blit_s : Blit.S with type t := t = Base_bigstring
include Blit_s
include Base_for_tests.Test_blit.Test (Blit_elt) (Bigstring_sequence) (Blit_s)
module From_bytes = From_bytes

include Base_for_tests.Test_blit.Test_distinct (Blit_elt) (Bytes_sequence)
    (Bigstring_sequence)
    (From_bytes)

module To_bytes = To_bytes

include Base_for_tests.Test_blit.Test_distinct (Blit_elt) (Bigstring_sequence)
    (Bytes_sequence)
    (To_bytes)

module From_string = From_string
module To_string = To_string

let copy = copy

let%test_unit "copy" =
  let equal a b = String.equal (to_string a) (to_string b) in
  let a = create 1 in
  set a 0 'a';
  let b = copy a in
  assert (equal a b);
  set b 0 'b';
  assert (not (equal a b))
;;

(* The above are produced by simple [Blit] functor applications, so just smoke-test the
   [unsafe_blit] functions we pass to the functors. *)
let%expect_test "basic unsafe_blits" =
  let test unsafe_blit src_of_string dst_of_string sexp_of_dst =
    let dst = dst_of_string "DEFGHIJK" in
    unsafe_blit ~src:(src_of_string "bcdefghi") ~src_pos:1 ~dst ~dst_pos:2 ~len:3;
    print_s [%sexp (dst : dst)];
    [%expect {| DEcdeIJK |}]
  in
  test From_string.unsafe_blit Fn.id of_string sexp_of_t
;;

let to_bytes = to_bytes
let to_string = to_string
let sexp_of_t = sexp_of_t
let of_string = of_string
let of_bytes = of_bytes
let t_of_sexp = t_of_sexp

let%test_unit "roundtrip" =
  let string_gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Quickcheck.Generator.small_non_negative_int in
    Quickcheck.Generator.list_with_length length Quickcheck.Generator.char_print
    >>| String.of_char_list
  in
  Quickcheck.test string_gen ~f:(fun str ->
    let bstr = of_string str in
    [%test_eq: t] bstr (of_string (to_string bstr));
    [%test_eq: t] bstr (of_bytes (to_bytes bstr));
    [%test_eq: t] bstr (t_of_sexp (sexp_of_t bstr)))
;;

external is_mmapped : t_frozen -> bool = "bigstring_is_mmapped_stub" [@@noalloc]

let%test "bigstring created with create are not mmapped" = not (is_mmapped (create 2))

let init = init

let try_setters z =
  List.map ~f:(fun setter ->
    let t = init 8 ~f:(fun _ -> '\000') in
    Or_error.try_with (fun () ->
      setter t ~pos:0 z;
      to_string t |> String.to_list |> List.map ~f:Char.to_int))
;;

let set_int8_exn = set_int8_exn
let set_uint8_exn = set_uint8_exn
let set_int16_le_exn = set_int16_le_exn
let set_int16_be_exn = set_int16_be_exn
let set_uint16_le_exn = set_uint16_le_exn
let set_uint16_be_exn = set_uint16_be_exn
let set_int32_le_exn = set_int32_le_exn
let set_int32_be_exn = set_int32_be_exn
let set_uint32_le_exn = set_uint32_le_exn
let set_uint32_be_exn = set_uint32_be_exn
let set_uint64_le_exn = set_uint64_le_exn
let set_uint64_be_exn = set_uint64_be_exn

let%expect_test "checking setters (should end in [_exn])" =
  let test setters z =
    try_setters z setters
    |> List.iteri ~f:(fun i ->
      function
      | Ok bytes ->
        raise_s [%message "didn't raise" (z : Int.Hex.t) (i : int) (bytes : int list)]
      | Error _ -> ())
  in
  test [ set_int8_exn ] 0x80;
  test [ set_uint8_exn ] (-1);
  test [ set_int16_le_exn; set_int16_be_exn ] 0x8000;
  test [ set_uint16_le_exn; set_uint16_be_exn ] (-1);
  Option.iter (Int64.to_int 0x8000_0000L) ~f:(fun z ->
    test [ set_int32_le_exn; set_int32_be_exn ] z);
  test [ set_uint32_le_exn; set_uint32_be_exn ] (-1);
  test [ set_uint64_le_exn; set_uint64_be_exn ] (-1)
;;

let unsafe_set_int8 = unsafe_set_int8
let unsafe_set_uint8 = unsafe_set_uint8
let unsafe_set_int16_le = unsafe_set_int16_le
let unsafe_set_int16_be = unsafe_set_int16_be
let unsafe_set_uint16_le = unsafe_set_uint16_le
let unsafe_set_uint16_be = unsafe_set_uint16_be
let unsafe_set_uint32_le = unsafe_set_uint32_le
let unsafe_set_uint32_be = unsafe_set_uint32_be
let unsafe_set_uint64_le = unsafe_set_uint64_le
let unsafe_set_uint64_be = unsafe_set_uint64_be
let unsafe_set_int32_le = unsafe_set_int32_le
let unsafe_set_int32_be = unsafe_set_int32_be

let%test_module "truncating setters (should end in [_trunc] or begin with [unsafe_])" =
  (module struct
    let test setters z =
      try_setters z setters
      |> List.iter ~f:(fun t ->
        Or_error.ok_exn t |> List.iter ~f:(printf "%x ");
        printf "; ")
    ;;

    let%expect_test "all word sizes" =
      test [ unsafe_set_int8 ] 0x9080;
      [%expect {| 80 0 0 0 0 0 0 0 ; |}];
      test [ unsafe_set_uint8 ] (-1);
      [%expect {| ff 0 0 0 0 0 0 0 ; |}];
      test [ unsafe_set_int16_le; unsafe_set_int16_be ] 0x90_8070;
      [%expect {| 70 80 0 0 0 0 0 0 ; 80 70 0 0 0 0 0 0 ; |}];
      test [ unsafe_set_uint16_le; unsafe_set_uint16_be ] (-1);
      [%expect {| ff ff 0 0 0 0 0 0 ; ff ff 0 0 0 0 0 0 ; |}];
      test [ unsafe_set_uint32_le; unsafe_set_uint32_be ] (-1);
      [%expect {| ff ff ff ff 0 0 0 0 ; ff ff ff ff 0 0 0 0 ; |}];
      test [ unsafe_set_uint64_le; unsafe_set_uint64_be ] (-1);
      [%expect {| ff ff ff ff ff ff ff ff ; ff ff ff ff ff ff ff ff ; |}]
    ;;

    let%expect_test (_[@tags "64-bits-only"]) =
      Option.iter (Int64.to_int 0x90_8070_6050L) ~f:(fun z ->
        test [ unsafe_set_int32_le; unsafe_set_int32_be ] z);
      [%expect {| 50 60 70 80 0 0 0 0 ; 80 70 60 50 0 0 0 0 ; |}]
    ;;
  end)
;;

let getter_t ~first_byte = init 8 ~f:(fun i -> i + first_byte |> Char.of_int_exn)
let get_int64_le_trunc = get_int64_le_trunc
let get_int64_be_trunc = get_int64_be_trunc
let unsafe_get_int64_le_trunc = unsafe_get_int64_le_trunc
let unsafe_get_int64_be_trunc = unsafe_get_int64_be_trunc

let%test_module "truncating getters (should end in [_trunc] or begin with [unsafe_])" =
  (module struct
    let test getter =
      List.iter
        [ 0x81 (* positive if top bit truncated *)
        ; 0xc1
          (* negative if top bit truncated *)
        ]
        ~f:(fun first_byte ->
          let i = getter (getter_t ~first_byte) ~pos:0 in
          (* Signed hex is not clear; make sure the hex is unsigned.  Include the signed
             decimal form mainly to indicate the sign. *)
          printf !"0x%x (= %d)\n" i i)
    ;;

    let%expect_test ("63-bit int"[@tags "64-bits-only"]) =
      test get_int64_le_trunc;
      [%expect
        {|
          0x887868584838281 (= 614607782171345537)
          0x48c7c6c5c4c3c2c1 (= -3978993193046523199) |}];
      test get_int64_be_trunc;
      [%expect
        {|
          0x182838485868788 (= 108793946209421192)
          0x41c2c3c4c5c6c7c8 (= -4484807029008447544) |}];
      test unsafe_get_int64_le_trunc;
      [%expect
        {|
          0x887868584838281 (= 614607782171345537)
          0x48c7c6c5c4c3c2c1 (= -3978993193046523199) |}];
      test unsafe_get_int64_be_trunc;
      [%expect
        {|
          0x182838485868788 (= 108793946209421192)
          0x41c2c3c4c5c6c7c8 (= -4484807029008447544) |}]
    ;;

    let%expect_test ("31-bit int"[@tags "32-bits-only", "no-js"]) =
      test get_int64_le_trunc;
      [%expect
        {|
          0x4838281 (= 75727489)
          0x44c3c2c1 (= -993803583) |}];
      test get_int64_be_trunc;
      [%expect
        {|
          0x5868788 (= 92702600)
          0x45c6c7c8 (= -976828472) |}];
      test unsafe_get_int64_le_trunc;
      [%expect
        {|
          0x4838281 (= 75727489)
          0x44c3c2c1 (= -993803583) |}];
      test unsafe_get_int64_be_trunc;
      [%expect
        {|
          0x5868788 (= 92702600)
          0x45c6c7c8 (= -976828472) |}]
    ;;

    let%expect_test ("32-bit int"[@tags "js-only"]) =
      test get_int64_le_trunc;
      [%expect
        {|
          0x84838281 (= -2071756159)
          0xc4c3c2c1 (= -993803583) |}];
      test get_int64_be_trunc;
      [%expect
        {|
          0x85868788 (= -2054781048)
          0xc5c6c7c8 (= -976828472) |}];
      test unsafe_get_int64_le_trunc;
      [%expect
        {|
          0x84838281 (= -2071756159)
          0xc4c3c2c1 (= -993803583) |}];
      test unsafe_get_int64_be_trunc;
      [%expect
        {|
          0x85868788 (= -2054781048)
          0xc5c6c7c8 (= -976828472) |}]
    ;;
  end)
;;

let get_int64_le_exn = get_int64_le_exn
let get_int64_be_exn = get_int64_be_exn
let unsafe_get_int64_le_exn = unsafe_get_int64_le_exn
let unsafe_get_int64_be_exn = unsafe_get_int64_be_exn
let get_uint64_le_exn = get_uint64_le_exn
let get_uint64_be_exn = get_uint64_be_exn
let unsafe_get_uint64_le_exn = unsafe_get_uint64_le_exn
let unsafe_get_uint64_be_exn = unsafe_get_uint64_be_exn

let try_getters ~first_bigstring_byte =
  List.map ~f:(fun getter ->
    let t = getter_t ~first_byte:first_bigstring_byte in
    Or_error.try_with (fun () -> getter t ~pos:0))
;;

let%expect_test "checking getters (should end in [_exn])" =
  let test getters ~first_bigstring_byte =
    try_getters getters ~first_bigstring_byte
    |> List.iteri ~f:(fun i ->
      function
      | Ok z -> raise_s [%message "didn't raise" (i : int) (z : Int.Hex.t)]
      | Error _ -> ())
  in
  test
    (* These should check that the 64th bit in the string representation is redundant, so
       the value is representable as a 63-bit [int] result. *)
    [ get_int64_le_exn
    ; get_int64_be_exn
    ; unsafe_get_int64_le_exn
    ; unsafe_get_int64_be_exn
    ; get_uint64_le_exn
    ; get_uint64_be_exn
    ; unsafe_get_uint64_le_exn
    ; unsafe_get_uint64_be_exn
    ]
    ~first_bigstring_byte:0x80;
  test
    (* These should additionally check that the represented integer is not negative. *)
    [ get_uint64_le_exn
    ; get_uint64_be_exn
    ; unsafe_get_uint64_le_exn
    ; unsafe_get_uint64_be_exn
    ]
    ~first_bigstring_byte:0xc0
;;

module Private = struct
  open Private

  let sign_extend_16 = sign_extend_16

  let%expect_test "sign_extend_16" =
    List.iter [ 32768; 32767; -32768; -32769 ] ~f:(fun i ->
      sign_extend_16 i |> printf "%d ");
    [%expect {| -32768 32767 -32768 32767 |}]
  ;;
end

let unsafe_set_int64_t_be = unsafe_set_int64_t_be
let unsafe_set_int64_t_le = unsafe_set_int64_t_le
let set_int64_t_be = set_int64_t_be
let set_int64_t_le = set_int64_t_le

let%expect_test "basic int64 setters" =
  try_setters
    0x0102030405060708L
    [ unsafe_set_int64_t_be; unsafe_set_int64_t_le; set_int64_t_be; set_int64_t_le ]
  |> printf !"%{sexp#hum:int list Or_error.t list}\n";
  [%expect
    {|
      ((Ok (1 2 3 4 5 6 7 8)) (Ok (8 7 6 5 4 3 2 1)) (Ok (1 2 3 4 5 6 7 8))
       (Ok (8 7 6 5 4 3 2 1))) |}]
;;

let unsafe_get_int64_t_be = unsafe_get_int64_t_be
let unsafe_get_int64_t_le = unsafe_get_int64_t_le
let get_int64_t_be = get_int64_t_be
let get_int64_t_le = get_int64_t_le

let%expect_test "basic int64 getters" =
  try_getters
    ~first_bigstring_byte:1
    [ unsafe_get_int64_t_be; unsafe_get_int64_t_le; get_int64_t_be; get_int64_t_le ]
  |> printf !"%{sexp#hum:Int64.Hex.t Or_error.t list}\n";
  [%expect
    {|
      ((Ok 0x102030405060708) (Ok 0x807060504030201) (Ok 0x102030405060708)
       (Ok 0x807060504030201)) |}]
;;

let unsafe_set_int32_t_be = unsafe_set_int32_t_be
let unsafe_set_int32_t_le = unsafe_set_int32_t_le
let set_int32_t_be = set_int32_t_be
let set_int32_t_le = set_int32_t_le

let%expect_test "basic int32 setters" =
  try_setters
    0x01020304l
    [ unsafe_set_int32_t_be; unsafe_set_int32_t_le; set_int32_t_be; set_int32_t_le ]
  |> printf !"%{sexp#hum:int list Or_error.t list}\n";
  [%expect
    {|
      ((Ok (1 2 3 4 0 0 0 0)) (Ok (4 3 2 1 0 0 0 0)) (Ok (1 2 3 4 0 0 0 0))
       (Ok (4 3 2 1 0 0 0 0))) |}]
;;

let unsafe_get_int32_t_be = unsafe_get_int32_t_be
let unsafe_get_int32_t_le = unsafe_get_int32_t_le
let get_int32_t_be = get_int32_t_be
let get_int32_t_le = get_int32_t_le

let%expect_test "basic int32 getters" =
  try_getters
    ~first_bigstring_byte:1
    [ unsafe_get_int32_t_be; unsafe_get_int32_t_le; get_int32_t_be; get_int32_t_le ]
  |> printf !"%{sexp#hum:Int32.Hex.t Or_error.t list}\n";
  [%expect {| ((Ok 0x1020304) (Ok 0x4030201) (Ok 0x1020304) (Ok 0x4030201)) |}]
;;

let unsafe_set_int64_be = unsafe_set_int64_be
let unsafe_set_int64_le = unsafe_set_int64_le
let set_int64_be = set_int64_be
let set_int64_le = set_int64_le

let%expect_test "basic int setters" =
  try_setters
    0x01020304
    [ unsafe_set_int64_be; unsafe_set_int64_le; set_int64_be; set_int64_le ]
  |> printf !"%{sexp#hum:int list Or_error.t list}\n";
  [%expect
    {|
      ((Ok (0 0 0 0 1 2 3 4)) (Ok (4 3 2 1 0 0 0 0)) (Ok (0 0 0 0 1 2 3 4))
       (Ok (4 3 2 1 0 0 0 0))) |}]
;;

let unsafe_get_uint32_be = unsafe_get_uint32_be
let unsafe_get_uint32_le = unsafe_get_uint32_le
let get_uint32_be = get_uint32_be
let get_uint32_le = get_uint32_le
let unsafe_get_int32_be = unsafe_get_int32_be
let unsafe_get_int32_le = unsafe_get_int32_le
let get_int32_be = get_int32_be
let get_int32_le = get_int32_le
let unsafe_get_uint16_be = unsafe_get_uint16_be
let unsafe_get_uint16_le = unsafe_get_uint16_le
let get_uint16_be = get_uint16_be
let get_uint16_le = get_uint16_le
let unsafe_get_int16_be = unsafe_get_int16_be
let unsafe_get_int16_le = unsafe_get_int16_le
let get_int16_be = get_int16_be
let get_int16_le = get_int16_le
let unsafe_get_uint8 = unsafe_get_uint8
let unsafe_get_int8 = unsafe_get_int8
let get_uint8 = get_uint8
let get_int8 = get_int8

let%expect_test "basic int getters" =
  try_getters
    ~first_bigstring_byte:1
    [ unsafe_get_uint32_be
    ; unsafe_get_uint32_le
    ; get_uint32_be
    ; get_uint32_le
    ; unsafe_get_int32_be
    ; unsafe_get_int32_le
    ; get_int32_be
    ; get_int32_le
    ; unsafe_get_uint16_be
    ; unsafe_get_uint16_le
    ; get_uint16_be
    ; get_uint16_le
    ; unsafe_get_int16_be
    ; unsafe_get_int16_le
    ; get_int16_be
    ; get_int16_le
    ; unsafe_get_uint8
    ; unsafe_get_int8
    ; get_uint8
    ; get_int8
    ]
  |> printf !"%{sexp#hum:Int.Hex.t Or_error.t list}\n";
  [%expect
    {|
      ((Ok 0x1020304) (Ok 0x4030201) (Ok 0x1020304) (Ok 0x4030201) (Ok 0x1020304)
       (Ok 0x4030201) (Ok 0x1020304) (Ok 0x4030201) (Ok 0x102) (Ok 0x201)
       (Ok 0x102) (Ok 0x201) (Ok 0x102) (Ok 0x201) (Ok 0x102) (Ok 0x201) (Ok 0x1)
       (Ok 0x1) (Ok 0x1) (Ok 0x1)) |}]
;;

external unsafe_find : t_frozen -> char -> pos:int -> len:int -> int = "bigstring_find"
[@@noalloc]

let%expect_test "basic unsafe_find" =
  let t = of_string "abc" in
  [%test_result: int] ~expect:1 (unsafe_find t 'b' ~pos:1 ~len:1);
  [%test_result: int] ~expect:1 (unsafe_find t 'b' ~pos:1 ~len:2);
  [%test_pred: int] Int.is_negative (unsafe_find t 'd' ~pos:1 ~len:2)
;;

let find = find

let%expect_test "basic find" =
  let t = of_string "abc" in
  [%test_result: int option] ~expect:(Some 1) (find 'b' t ~pos:1 ~len:1);
  [%test_result: int option] ~expect:None (find 'b' t ~pos:1 ~len:0);
  require_does_raise [%here] (fun () -> find 'b' t ~len:4);
  [%expect {| (Invalid_argument "Bigstring.find: length(bstr) < pos + len") |}];
  require_does_raise [%here] (fun () -> find 'd' t ~len:4);
  [%expect {| (Invalid_argument "Bigstring.find: length(bstr) < pos + len") |}];
  [%test_result: int option] ~expect:(Some 1) (find 'b' t ~pos:1 ~len:2);
  [%test_result: int option] ~expect:None (find 'd' t ~pos:1 ~len:2);
  [%test_result: int option] ~expect:None (find 'b' t ~len:1);
  [%test_result: int option] ~expect:(Some 1) (find 'b' t ~pos:1);
  [%test_result: int option] ~expect:None (find 'd' t);
  require_does_raise [%here] (fun () -> find 'b' t ~pos:4);
  [%expect {| (Invalid_argument "find: len < 0") |}]
;;

let get_opt_len = get_opt_len

let%expect_test "basic get_opt_len" =
  let t = of_string "abc" in
  [%test_result: int] ~expect:2 (get_opt_len t ~pos:1 None);
  [%test_result: int] ~expect:5 (get_opt_len t ~pos:1 (Some 5));
  [%test_result: int] ~expect:1 (get_opt_len t ~pos:1 (Some 1));
  [%test_result: int] ~expect:0 (get_opt_len t ~pos:1 (Some 0))
;;

let memcmp = memcmp

let%expect_test "basic memcmp" =
  let t1 = of_string "221007247563588720" in
  let t2 = of_string "1650905272620466461" in
  [%test_result: int] ~expect:0 (memcmp t1 ~pos1:0 t2 ~pos2:0 ~len:0);
  [%test_pred: int] Int.is_positive (memcmp t1 ~pos1:0 t2 ~pos2:0 ~len:3);
  [%test_pred: int] Int.is_negative (memcmp t1 ~pos1:0 t2 ~pos2:1 ~len:3)
;;

let memset = memset

external set : t_frozen -> int -> char -> unit = "%caml_ba_set_1"

let%expect_test "basic char setters" =
  try_setters
    'x'
    [ memset ~len:0; memset ~len:1; memset ~len:2; (fun t ~pos -> set t pos) ]
  |> printf !"%{sexp#hum:int list Or_error.t list}\n";
  [%expect
    {|
      ((Ok (0 0 0 0 0 0 0 0)) (Ok (120 0 0 0 0 0 0 0)) (Ok (120 120 0 0 0 0 0 0))
       (Ok (120 0 0 0 0 0 0 0))) |}]
;;

external get : t_frozen -> int -> char = "%caml_ba_ref_1"

let%expect_test "basic char getters" =
  try_getters ~first_bigstring_byte:1 [ (fun t ~pos -> get t pos) ]
  |> printf !"%{sexp#hum:char Or_error.t list}\n";
  [%expect {| ((Ok "\001")) |}]
;;

let check_args = check_args

let%expect_test "basic check_args" =
  require_does_raise [%here] (fun () ->
    check_args ~loc:"LOC" ~pos:Int.max_value ~len:2 (of_string "abc"));
  [%expect {| (Invalid_argument "Bigstring.LOC: length(bstr) < pos + len") |}];
  check_args ~loc:"LOC" ~pos:0 ~len:0 (of_string "");
  require_does_raise [%here] (fun () ->
    check_args ~loc:"LOC" ~pos:1 ~len:0 (of_string ""));
  [%expect {| (Invalid_argument "Bigstring.LOC: length(bstr) < pos + len") |}];
  require_does_raise [%here] (fun () ->
    check_args ~loc:"LOC" ~pos:0 ~len:1 (of_string ""));
  [%expect {| (Invalid_argument "Bigstring.LOC: length(bstr) < pos + len") |}];
  check_args ~loc:"LOC" ~pos:0 ~len:0 (of_string "STRING")
;;

let concat = concat

let%expect_test "basic concat" =
  List.iter
    [ None; Some (of_string "; ") ]
    ~f:(fun sep ->
      List.iter
        [ []; [ "fst1" ]; [ "fst"; "snd" ]; [ "fst3"; "snd3"; "trd" ] ]
        ~f:(fun ts ->
          let ts = List.map ts ~f:of_string in
          print_s [%message (sep : t option) (ts : t list) ~concat:(concat ?sep ts : t)]));
  [%expect
    {|
      ((sep ())
       (ts  ())
       (concat ""))
      ((sep ()) (ts (fst1)) (concat fst1))
      ((sep ()) (ts (fst snd)) (concat fstsnd))
      ((sep ()) (ts (fst3 snd3 trd)) (concat fst3snd3trd))
      ((sep ("; ")) (ts ()) (concat ""))
      ((sep ("; "))
       (ts  (fst1))
       (concat fst1))
      ((sep ("; ")) (ts (fst snd)) (concat "fst; snd"))
      ((sep ("; ")) (ts (fst3 snd3 trd)) (concat "fst3; snd3; trd")) |}]
;;

let equal = equal
let compare = compare

let%expect_test "basic equal" =
  let strings = [ ""; "a"; "aa"; "ab"; "b"; "ba"; "bb" ] in
  List.iter strings ~f:(fun s1 ->
    let t1 = of_string s1 in
    let test s2 =
      let t2 = of_string s2 in
      require_equal
        [%here]
        (module Bool)
        (equal t1 t2)
        (String.equal s1 s2)
        ~if_false_then_print_s:(lazy [%message (s1 : string) (s2 : string)]);
      require_equal
        [%here]
        (module Int)
        (compare t1 t2)
        (String.compare s1 s2)
        ~if_false_then_print_s:(lazy [%message (s1 : string) (s2 : string)])
    in
    test s1;
    List.iter strings ~f:test)
;;

type nonrec t = t
type nonrec t_frozen = t_frozen [@@deriving compare, hash, sexp]
