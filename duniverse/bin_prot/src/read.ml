(* Read_ml: reading values from the binary protocol using (mostly) OCaml. *)

(* Note: the code is this file is carefully written to avoid unnecessary allocations. When
   touching this code, be sure to run the benchmarks to check for regressions. *)

open Bigarray
open Common

type 'a reader = buf -> pos_ref:pos_ref -> 'a
type ('a, 'b) reader1 = 'a reader -> 'b reader
type ('a, 'b, 'c) reader2 = 'a reader -> ('b, 'c) reader1
type ('a, 'b, 'c, 'd) reader3 = 'a reader -> ('b, 'c, 'd) reader2

external unsafe_get : buf -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_get8 : buf -> int -> int = "%caml_ba_unsafe_ref_1"

let unsafe_get8_signed buf pos =
  let c = unsafe_get8 buf pos in
  if c >= 128 then c - 256 else c
;;

(*$ open Bin_prot_cinaps $*)

let arch_sixtyfour = Sys.word_size = 64
let arch_big_endian = Sys.big_endian
let max_int_int32 = if arch_sixtyfour then Int32.max_int else Int32.of_int max_int
let min_int_int32 = if arch_sixtyfour then Int32.max_int else Int32.of_int min_int
let max_int_int64 = Int64.of_int max_int
let min_int_int64 = Int64.of_int min_int

let safe_int_of_int32 pos x =
  if arch_sixtyfour
  then Int32.to_int x
  else if x >= min_int_int32 && x <= max_int_int32
  then Int32.to_int x
  else raise_read_error ReadError.Int_overflow pos
;;

let safe_int_of_int64 pos x =
  if x >= min_int_int64 && x <= max_int_int64
  then Int64.to_int x
  else raise_read_error ReadError.Int_overflow pos
;;

let safe_nativeint_of_int64 =
  if arch_sixtyfour
  then fun _pos x -> Int64.to_nativeint x
  else
    fun pos x ->
      if x >= Int64.of_nativeint Nativeint.min_int
      && x <= Int64.of_nativeint Nativeint.max_int
      then Int64.to_nativeint x
      else raise_read_error ReadError.Int_overflow pos
;;

external unsafe_get16 : buf -> int -> int = "%caml_bigstring_get16u"
external unsafe_get32 : buf -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_get64 : buf -> int -> int64 = "%caml_bigstring_get64u"
external bswap16 : int -> int = "%bswap16"
external bswap32 : int32 -> int32 = "%bswap_int32"
external bswap64 : int64 -> int64 = "%bswap_int64"

let unsafe_get16be_unsigned =
  if arch_big_endian
  then unsafe_get16
  else fun buf pos -> unsafe_get16 buf pos |> bswap16
;;

let unsafe_get32be =
  if arch_big_endian
  then unsafe_get32
  else fun buf pos -> unsafe_get32 buf pos |> bswap32
;;

let unsafe_get64be =
  if arch_big_endian
  then unsafe_get64
  else fun buf pos -> unsafe_get64 buf pos |> bswap64
;;

let unsafe_get16le_unsigned =
  if arch_big_endian
  then fun buf pos -> unsafe_get16 buf pos |> bswap16
  else unsafe_get16
;;

let unsafe_get32le =
  if arch_big_endian
  then fun buf pos -> unsafe_get32 buf pos |> bswap32
  else unsafe_get32
;;

let unsafe_get64le =
  if arch_big_endian
  then fun buf pos -> unsafe_get64 buf pos |> bswap64
  else unsafe_get64
;;

let unsafe_get16le_signed buf pos =
  let x = unsafe_get16le_unsigned buf pos in
  if x > 32767 then x - 65536 else x
;;

let bin_read_unit buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  if unsafe_get buf pos = '\000'
  then pos_ref := pos + 1
  else raise_read_error ReadError.Unit_code pos
;;

let bin_read_bool buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\000' ->
    pos_ref := pos + 1;
    false
  | '\001' ->
    pos_ref := pos + 1;
    true
  | _ -> raise_read_error ReadError.Bool_code pos
;;

let safe_bin_read_neg_int8 buf ~pos_ref ~pos =
  let next = pos + 1 in
  check_next buf next;
  let n = unsafe_get8_signed buf pos in
  if n >= 0 then raise_read_error ReadError.Neg_int8 !pos_ref;
  pos_ref := next;
  n
;;

let safe_bin_read_int16 buf ~pos_ref ~pos =
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  (* Can be above next line (no errors possible with 16bit).
     This should improve the generated code. *)
  unsafe_get16le_signed buf pos
;;

let safe_bin_read_int32 buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  (* No error possible either. *)
  unsafe_get32le buf pos
;;

let safe_bin_read_int64 buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  (* No error possible either. *)
  unsafe_get64le buf pos
;;

let safe_bin_read_int32_as_int buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  let n = unsafe_get32le buf pos in
  let n = safe_int_of_int32 !pos_ref n in
  pos_ref := next;
  n
;;

let safe_bin_read_int64_as_int buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = unsafe_get64le buf pos in
  let n = safe_int_of_int64 !pos_ref n in
  pos_ref := next;
  n
;;

let safe_bin_read_int32_as_int64 buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32le buf pos in
  Int64.of_int32 n
;;

let safe_bin_read_int32_as_nativeint buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32le buf pos in
  Nativeint.of_int32 n
;;

let safe_bin_read_int64_as_nativeint buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = unsafe_get64le buf pos in
  let n = safe_nativeint_of_int64 pos n in
  pos_ref := next;
  n
;;

let safe_bin_read_nat0_16 buf ~pos_ref ~pos =
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  Nat0.unsafe_of_int (unsafe_get16le_unsigned buf pos)
;;

let safe_bin_read_nat0_32 =
  if arch_sixtyfour
  then (
    let mask_32bit = Int64.to_int 0xffff_ffffL in
    fun buf ~pos_ref ~pos ->
      let next = pos + 4 in
      check_next buf next;
      pos_ref := next;
      let n = Int32.to_int (unsafe_get32le buf pos) in
      if n >= 0
      then Nat0.unsafe_of_int n
      else
        (* Erase the upper bits that were set to 1 during the int32 -> int conversion. *)
        Nat0.unsafe_of_int (n land mask_32bit))
  else
    fun buf ~pos_ref ~pos ->
      let next = pos + 4 in
      check_next buf next;
      let n = unsafe_get32le buf pos in
      if n >= 0l && n <= max_int_int32
      then (
        let n = Nat0.unsafe_of_int (Int32.to_int n) in
        pos_ref := next;
        n)
      else raise_read_error ReadError.Nat0_overflow !pos_ref
;;

let safe_bin_read_nat0_64 buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = unsafe_get64le buf pos in
  if n >= 0L && n <= max_int_int64
  then (
    let n = Nat0.unsafe_of_int (Int64.to_int n) in
    pos_ref := next;
    n)
  else raise_read_error ReadError.Nat0_overflow !pos_ref
;;

let bin_read_nat0 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Nat0.unsafe_of_int (Char.code ch)
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    safe_bin_read_nat0_16 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_nat0_32 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT64 *)'\xfc'(*$*) ->
    if arch_sixtyfour then
      safe_bin_read_nat0_64 buf ~pos_ref ~pos:(pos + 1)
    else
      raise_read_error ReadError.Nat0_overflow pos
  | _ ->
    raise_read_error ReadError.Nat0_code pos
[@@ocamlformat "disable"]

let bin_read_bytes buf ~pos_ref =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > Sys.max_string_length
  then raise_read_error ReadError.String_too_long start_pos;
  let pos = !pos_ref in
  let next = pos + len in
  check_next buf next;
  pos_ref := next;
  let str = Bytes.create len in
  unsafe_blit_buf_bytes ~src_pos:pos buf ~dst_pos:0 str ~len;
  str
;;

let bin_read_string buf ~pos_ref =
  let str = bin_read_bytes buf ~pos_ref in
  Bytes.unsafe_to_string str
;;

let bin_read_char buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  pos_ref := pos + 1;
  unsafe_get buf pos
;;

let bin_read_int buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Char.code ch
  | (*$ Code.char NEG_INT8 *)'\xff'(*$*) ->
    safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_int32_as_int buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT64 *)'\xfc'(*$*) ->
    if arch_sixtyfour then
      safe_bin_read_int64_as_int buf ~pos_ref ~pos:(pos + 1)
    else
      raise_read_error ReadError.Int_overflow pos
  | _ ->
    raise_read_error ReadError.Int_code pos
[@@ocamlformat "disable"]

let bin_read_float buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  (* No error possible either. *)
  Int64.float_of_bits (unsafe_get64le buf pos)
;;

let bin_read_int32 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Int32.of_int (Char.code ch)
  | (*$ Code.char NEG_INT8 *)'\xff'(*$*) ->
    Int32.of_int (safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    Int32.of_int (safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_int32 buf ~pos_ref ~pos:(pos + 1)
  | _ ->
    raise_read_error ReadError.Int32_code pos
[@@ocamlformat "disable"]

let bin_read_int64 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Int64.of_int (Char.code ch)
  | (*$ Code.char NEG_INT8 *)'\xff'(*$*) ->
    Int64.of_int (safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    Int64.of_int (safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_int32_as_int64 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT64 *)'\xfc'(*$*) ->
    safe_bin_read_int64 buf ~pos_ref ~pos:(pos + 1)
  | _ ->
    raise_read_error ReadError.Int64_code pos
[@@ocamlformat "disable"]

let bin_read_nativeint buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Nativeint.of_int (Char.code ch)
  | (*$ Code.char NEG_INT8 *)'\xff'(*$*) ->
    Nativeint.of_int (safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    Nativeint.of_int (safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_int32_as_nativeint buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT64 *)'\xfc'(*$*) when arch_sixtyfour ->
    safe_bin_read_int64_as_nativeint buf ~pos_ref ~pos:(pos + 1)
  | _ ->
    raise_read_error ReadError.Nativeint_code pos
[@@ocamlformat "disable"]

let bin_read_ref bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  ref el
;;

let bin_read_lazy bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  Lazy.from_val el
;;

let bin_read_option bin_read_el buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\000' ->
    pos_ref := pos + 1;
    None
  | '\001' ->
    pos_ref := pos + 1;
    let el = bin_read_el buf ~pos_ref in
    Some el
  | _ -> raise_read_error ReadError.Option_code pos
;;

let bin_read_pair bin_read_a bin_read_b buf ~pos_ref =
  let a = bin_read_a buf ~pos_ref in
  let b = bin_read_b buf ~pos_ref in
  a, b
;;

let bin_read_triple bin_read_a bin_read_b bin_read_c buf ~pos_ref =
  let a = bin_read_a buf ~pos_ref in
  let b = bin_read_b buf ~pos_ref in
  let c = bin_read_c buf ~pos_ref in
  a, b, c
;;

let bin_read_n_rev_list bin_read_el buf ~pos_ref len =
  let rec loop n acc =
    if n = 0 then acc else loop (n - 1) (bin_read_el buf ~pos_ref :: acc)
  in
  loop len []
;;

let bin_read_list_with_max_len ~max_len bin_read_el buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > max_len then raise_read_error (List_too_long { len; max_len }) !pos_ref;
  let rev_lst = bin_read_n_rev_list bin_read_el buf ~pos_ref len in
  List.rev rev_lst
;;

let bin_read_list bin_read_el buf ~pos_ref =
  bin_read_list_with_max_len ~max_len:max_int bin_read_el buf ~pos_ref
;;

let dummy_float_buf = create_buf 8
let () = ignore (Write.bin_write_float dummy_float_buf ~pos:0 3.1)

let max_float_array_length =
  if arch_sixtyfour then Sys.max_array_length else Sys.max_array_length / 2
;;

let bin_read_float_array buf ~pos_ref =
  let pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > max_float_array_length then raise_read_error ReadError.Array_too_long pos;
  let size = len * 8 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let arr = Array.create_float len in
  unsafe_blit_buf_float_array buf arr ~src_pos:pos ~dst_pos:0 ~len;
  pos_ref := next;
  arr
;;

let bin_read_array (type a) bin_read_el buf ~pos_ref =
  if (Obj.magic (bin_read_el : a reader) : float reader) == bin_read_float
  then (Obj.magic (bin_read_float_array buf ~pos_ref : float array) : a array)
  else (
    let start_pos = !pos_ref in
    let len = (bin_read_nat0 buf ~pos_ref :> int) in
    if len = 0
    then [||]
    else (
      if arch_sixtyfour
      then (
        if len > Sys.max_array_length
        then raise_read_error ReadError.Array_too_long start_pos)
      else if len > Sys.max_array_length / 2
      then (
        let maybe_float =
          try
            let el = bin_read_el dummy_float_buf ~pos_ref:(ref 0) in
            Some el
          with
          | _ -> None
        in
        match maybe_float with
        | None ->
          if len > Sys.max_array_length
          then raise_read_error ReadError.Array_too_long start_pos
        | Some el ->
          if Obj.tag (Obj.repr el) = Obj.double_tag || len > Sys.max_array_length
          then raise_read_error ReadError.Array_too_long start_pos);
      let first = bin_read_el buf ~pos_ref in
      let res = Array.make len first in
      for i = 1 to len - 1 do
        let el = bin_read_el buf ~pos_ref in
        Array.unsafe_set res i el
      done;
      res))
;;

let bin_read_hashtbl bin_read_key bin_read_val buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let htbl = Hashtbl.create len in
  let read_kv_pair = bin_read_pair bin_read_key bin_read_val in
  let els = bin_read_n_rev_list read_kv_pair buf ~pos_ref len in
  copy_htbl_list htbl els
;;

external buf_of_vec32 : vec32 -> buf = "%identity"
external buf_of_vec64 : vec64 -> buf = "%identity"
external buf_of_mat32 : mat32 -> buf = "%identity"
external buf_of_mat64 : mat64 -> buf = "%identity"

let bin_read_float32_vec buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len * 4 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let vec = Array1.create float32 fortran_layout len in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_vec32 vec) ~dst_pos:0 ~len:size;
  pos_ref := next;
  vec
;;

let bin_read_float64_vec buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len * 8 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let vec = Array1.create float64 fortran_layout len in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_vec64 vec) ~dst_pos:0 ~len:size;
  pos_ref := next;
  vec
;;

let bin_read_vec = bin_read_float64_vec

let bin_read_float32_mat buf ~pos_ref =
  let len1 = (bin_read_nat0 buf ~pos_ref :> int) in
  let len2 = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len1 * len2 * 4 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let mat = Array2.create float32 fortran_layout len1 len2 in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_mat32 mat) ~dst_pos:0 ~len:size;
  pos_ref := next;
  mat
;;

let bin_read_float64_mat buf ~pos_ref =
  let len1 = (bin_read_nat0 buf ~pos_ref :> int) in
  let len2 = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len1 * len2 * 8 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let mat = Array2.create float64 fortran_layout len1 len2 in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_mat64 mat) ~dst_pos:0 ~len:size;
  pos_ref := next;
  mat
;;

let bin_read_mat = bin_read_float64_mat

let bin_read_bigstring buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let pos = !pos_ref in
  let next = pos + len in
  check_next buf next;
  let str = create_buf len in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:str ~dst_pos:0 ~len;
  pos_ref := next;
  str
;;

let bin_read_variant_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  let n = unsafe_get32le buf pos in
  (* [n] must contain an integer already encoded, i.e. [n = 2 * k + 1]. *)
  if Int32.logand n 1l = 0l
  then raise (Read_error (ReadError.Variant_tag, pos))
  else (
    (* We shift it by one bit to the right se we get back [2 * k + 1] in the end. *)
    pos_ref := next;
    Int32.to_int (Int32.shift_right n 1))
;;

let bin_read_int_8bit buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  pos_ref := pos + 1;
  unsafe_get8 buf pos
;;

let bin_read_int_16bit buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  unsafe_get16le_unsigned buf pos
;;

let bin_read_int_32bit buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32le buf pos in
  safe_int_of_int32 pos n
;;

let bin_read_int_64bit buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get64le buf pos in
  safe_int_of_int64 pos n
;;

let bin_read_int64_bits buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  unsafe_get64le buf pos
;;

let bin_read_network16_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  unsafe_get16be_unsigned buf pos
;;

let bin_read_network32_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32be buf pos in
  safe_int_of_int32 pos n
;;

let bin_read_network32_int32 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  unsafe_get32be buf pos
;;

let bin_read_network64_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get64be buf pos in
  safe_int_of_int64 pos n
;;

let bin_read_network64_int64 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  unsafe_get64be buf pos
;;

[%%if
  ocaml_version < (4, 07, 0)]

external unsafe_bytes_set32 : bytes -> int -> int32 -> unit = "%caml_string_set32u"
external unsafe_bytes_set64 : bytes -> int -> int64 -> unit = "%caml_string_set64u"

[%%else]

external unsafe_bytes_set32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external unsafe_bytes_set64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"

[%%endif]

let bin_read_md5 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 16 in
  check_next buf next;
  pos_ref := next;
  let res = Bytes.create 16 in
  if arch_sixtyfour
  then (
    let a = unsafe_get64 buf pos in
    let b = unsafe_get64 buf (pos + 8) in
    unsafe_bytes_set64 res 0 a;
    unsafe_bytes_set64 res 8 b)
  else (
    let a = unsafe_get32 buf pos in
    let b = unsafe_get32 buf (pos + 4) in
    let c = unsafe_get32 buf (pos + 8) in
    let d = unsafe_get32 buf (pos + 12) in
    unsafe_bytes_set32 res 0 a;
    unsafe_bytes_set32 res 4 b;
    unsafe_bytes_set32 res 8 c;
    unsafe_bytes_set32 res 12 d);
  Md5_lib.unsafe_of_binary (Bytes.unsafe_to_string res)
;;
