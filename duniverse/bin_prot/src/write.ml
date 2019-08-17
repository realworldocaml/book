(* Write_ml: writing values to the binary protocol using (mostly) OCaml. *)

(* Note: the code is this file is carefully written to avoid unnecessary allocations. When
   touching this code, be sure to run the benchmarks to check for regressions. *)

#include "int_codes.mlh"

open Bigarray
open Common

type 'a writer = buf -> pos : pos -> 'a -> pos
type ('a, 'b) writer1 = 'a writer -> 'b writer
type ('a, 'b, 'c) writer2 = 'a writer -> ('b, 'c) writer1
type ('a, 'b, 'c, 'd) writer3 = 'a writer -> ('b, 'c, 'd) writer2

external unsafe_set   : buf -> int -> char  -> unit = "%caml_ba_unsafe_set_1";;
external unsafe_set8  : buf -> int -> int   -> unit = "%caml_ba_unsafe_set_1";;
external unsafe_set16 : buf -> int -> int   -> unit = "%caml_bigstring_set16u";;
external unsafe_set32 : buf -> int -> int32 -> unit = "%caml_bigstring_set32u";;
external unsafe_set64 : buf -> int -> int64 -> unit = "%caml_bigstring_set64u";;

external bswap16 : int -> int = "%bswap16";;
external bswap32 : int32 -> int32 = "%bswap_int32";;
external bswap64 : int64 -> int64 = "%bswap_int64";;

let arch_sixtyfour = Sys.word_size = 64
let arch_big_endian = Sys.big_endian

let unsafe_set16be =
  if arch_big_endian then
    unsafe_set16
  else
    fun buf pos x -> unsafe_set16 buf pos (bswap16 x)
let unsafe_set32be =
  if arch_big_endian then
    unsafe_set32
  else
    fun buf pos x -> unsafe_set32 buf pos (bswap32 x)
let unsafe_set64be =
  if arch_big_endian then
    unsafe_set64
  else
    fun buf pos x -> unsafe_set64 buf pos (bswap64 x)

let unsafe_set16le =
  if arch_big_endian then
    fun buf pos x -> unsafe_set16 buf pos (bswap16 x)
  else
    unsafe_set16
let unsafe_set32le =
  if arch_big_endian then
    fun buf pos x -> unsafe_set32 buf pos (bswap32 x)
  else
    unsafe_set32
let unsafe_set64le =
  if arch_big_endian then
    fun buf pos x -> unsafe_set64 buf pos (bswap64 x)
  else
    unsafe_set64

let bin_write_unit buf ~pos () =
  assert_pos pos;
  check_pos buf pos;
  unsafe_set buf pos '\000';
  pos + 1

let bin_write_bool buf ~pos b =
  assert_pos pos;
  check_pos buf pos;
  unsafe_set buf pos (if b then '\001' else '\000');
  pos + 1

let all_bin_write_small_int buf pos n =
  check_pos buf pos;
  unsafe_set8 buf pos n;
  pos + 1

let all_bin_write_neg_int8 buf pos n =
  let next = pos + 2 in
  check_next buf next;
  unsafe_set buf pos CODE_NEG_INT8;
  unsafe_set8 buf (pos + 1) n;
  next

let all_bin_write_int16 buf pos n =
  let next = pos + 3 in
  check_next buf next;
  unsafe_set buf pos CODE_INT16;
  unsafe_set16le buf (pos + 1) n;
  next

let all_bin_write_int32 buf pos n =
  let next = pos + 5 in
  check_next buf next;
  unsafe_set buf pos CODE_INT32;
  unsafe_set32le buf (pos + 1) n;
  next
[@@inline]

let all_bin_write_int64 buf pos n =
  let next = pos + 9 in
  check_next buf next;
  unsafe_set buf pos CODE_INT64;
  unsafe_set64le buf (pos + 1) n;
  next
[@@inline]

let bin_write_char buf ~pos c =
  assert_pos pos;
  check_pos buf pos;
  unsafe_set buf pos c;
  pos + 1

let bin_write_int buf ~pos n =
  assert_pos pos;
  if n >= 0 then begin
    if n < 0x00000080 then
      all_bin_write_small_int buf pos n
    else if n < 0x00008000 then
      all_bin_write_int16 buf pos n
    else if arch_sixtyfour && n >= 1 lsl 31 then
      all_bin_write_int64 buf pos (Int64.of_int n)
    else
      all_bin_write_int32 buf pos (Int32.of_int n)
  end else begin
    if n >= -0x00000080 then
      all_bin_write_neg_int8 buf pos n
    else if n >= -0x00008000 then
      all_bin_write_int16 buf pos n
    else if arch_sixtyfour && n < -(1 lsl 31) then
      all_bin_write_int64 buf pos (Int64.of_int n)
    else
      all_bin_write_int32 buf pos (Int32.of_int n)
  end

let bin_write_nat0 buf ~pos nat0 =
  assert_pos pos;
  let n = (nat0 : Nat0.t :> int) in
  if n < 0x00000080 then
    all_bin_write_small_int buf pos n
  else if n < 0x00010000 then
    all_bin_write_int16 buf pos n
  else if arch_sixtyfour && n >= 1 lsl 32 then
    all_bin_write_int64 buf pos (Int64.of_int n)
  else
    all_bin_write_int32 buf pos (Int32.of_int n)

let bin_write_string buf ~pos str =
  let len = String.length str in
  let plen = Nat0.unsafe_of_int len in
  let new_pos = bin_write_nat0 buf ~pos plen in
  let next = new_pos + len in
  check_next buf next;
  (* TODO: optimize for small strings *)
  unsafe_blit_string_buf ~src_pos:0 str ~dst_pos:new_pos buf ~len;
  next

let bin_write_bytes buf ~pos str =
  let len = Bytes.length str in
  let plen = Nat0.unsafe_of_int len in
  let new_pos = bin_write_nat0 buf ~pos plen in
  let next = new_pos + len in
  check_next buf next;
  (* TODO: optimize for small bytes *)
  unsafe_blit_bytes_buf ~src_pos:0 str ~dst_pos:new_pos buf ~len;
  next

let bin_write_float buf ~pos x =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64le buf pos (Int64.bits_of_float x);
  next
[@@inline]

let bin_write_int32 =
  if arch_sixtyfour then
    (fun buf ~pos n -> bin_write_int buf ~pos (Int32.to_int n)) [@inline]
  else
    (fun buf ~pos n ->
       if n >= 0x00008000l || n < -0x00008000l then begin
         assert_pos pos;
         all_bin_write_int32 buf pos n
       end else
         bin_write_int buf ~pos (Int32.to_int n)) [@inline]

let bin_write_int64 buf ~pos n =
  if n >= 0x80000000L || n < -0x80000000L then begin
    assert_pos pos;
    all_bin_write_int64 buf pos n
  end else if arch_sixtyfour then
    bin_write_int buf ~pos (Int64.to_int n)
  else if n >= 0x00008000L || n < -0x00008000L then begin
    assert_pos pos;
    all_bin_write_int32 buf pos (Int64.to_int32 n)
  end else
    bin_write_int buf ~pos (Int64.to_int n)
[@@inline]

let bin_write_nativeint buf ~pos n =
  if arch_sixtyfour &&
     (n >= (* 0x80000000n *) (Nativeint.shift_left 1n 31) ||
      n < (* -0x80000000n *) Nativeint.neg (Nativeint.shift_left 1n 31)) then begin
    assert_pos pos;
    all_bin_write_int64 buf pos (Int64.of_nativeint n)
  end else if not arch_sixtyfour && n >= 0x8000n || n < -0x8000n then begin
    assert_pos pos;
    all_bin_write_int32 buf pos (Nativeint.to_int32 n)
  end else
    bin_write_int buf ~pos (Nativeint.to_int n)
[@@inline]

let bin_write_ref bin_write_el buf ~pos r = bin_write_el buf ~pos !r

let bin_write_lazy bin_write_el buf ~pos lv =
  let v = Lazy.force lv in
  bin_write_el buf ~pos v

let bin_write_option bin_write_el buf ~pos = function
  | None -> bin_write_bool buf ~pos false
  | Some v ->
      let next = bin_write_bool buf ~pos true in
      bin_write_el buf ~pos:next v

let bin_write_pair bin_write_a bin_write_b buf ~pos (a, b) =
  let next = bin_write_a buf ~pos a in
  bin_write_b buf ~pos:next b

let bin_write_triple bin_write_a bin_write_b bin_write_c buf ~pos (a, b, c) =
  let next1 = bin_write_a buf ~pos a in
  let next2 = bin_write_b buf ~pos:next1 b in
  bin_write_c buf ~pos:next2 c

let bin_write_list bin_write_el buf ~pos lst =
  let rec loop els_pos = function
    | [] -> els_pos
    | h :: t ->
        let new_els_pos = bin_write_el buf ~pos:els_pos h in
        loop new_els_pos t
  in
  let len = Nat0.unsafe_of_int (List.length lst) in
  let els_pos = bin_write_nat0 buf ~pos len in
  loop els_pos lst

let bin_write_float_array buf ~pos a =
  let len = Array.length a in
  let plen = Nat0.unsafe_of_int len in
  let pos = bin_write_nat0 buf ~pos plen in
  let size = len * 8 in
  let next = pos + size in
  check_next buf next;
  unsafe_blit_float_array_buf a buf ~src_pos:0 ~dst_pos:pos ~len;
  next

let bin_write_array_loop bin_write_el buf ~els_pos ~n ar =
  let els_pos_ref = ref els_pos in
  for i = 0 to n - 1 do
    els_pos_ref := bin_write_el buf ~pos:!els_pos_ref (Array.unsafe_get ar i)
  done;
  !els_pos_ref

let bin_write_array (type a) bin_write_el buf ~pos ar =
  if (Obj.magic (bin_write_el : a writer) : float writer) == bin_write_float then
    bin_write_float_array buf ~pos (Obj.magic (ar : a array) : float array)
  else
    let n = Array.length ar in
    let pn = Nat0.unsafe_of_int n in
    let els_pos = bin_write_nat0 buf ~pos pn in
    bin_write_array_loop bin_write_el buf ~els_pos ~n ar

let bin_write_hashtbl bin_write_key bin_write_val buf ~pos htbl =
  let len = Hashtbl.length htbl in
  let plen = Nat0.unsafe_of_int len in
  let els_pos = bin_write_nat0 buf ~pos plen in
  let cnt_ref = ref 0 in
  let coll_htbl k v els_pos =
    incr cnt_ref;
    let new_els_pos = bin_write_key buf ~pos:els_pos k in
    bin_write_val buf ~pos:new_els_pos v
  in
  let res_pos = Hashtbl.fold coll_htbl htbl els_pos in
  if !cnt_ref <> len then raise_concurrent_modification "bin_write_hashtbl";
  res_pos

external buf_of_vec32 : vec32 -> buf = "%identity"
external buf_of_vec64 : vec64 -> buf = "%identity"
external buf_of_mat32 : mat32 -> buf = "%identity"
external buf_of_mat64 : mat64 -> buf = "%identity"

let bin_write_float32_vec buf ~pos v =
  let len = Array1.dim v in
  let plen = Nat0.unsafe_of_int len in
  let pos = bin_write_nat0 buf ~pos plen in
  let size = len * 4 in
  let next = pos + size in
  check_next buf next;
  unsafe_blit_buf ~src:(buf_of_vec32 v) ~src_pos:0 ~dst:buf ~dst_pos:pos ~len:size;
  next

let bin_write_float64_vec buf ~pos v =
  let len = Array1.dim v in
  let plen = Nat0.unsafe_of_int len in
  let pos = bin_write_nat0 buf ~pos plen in
  let size = len * 8 in
  let next = pos + size in
  check_next buf next;
  unsafe_blit_buf ~src:(buf_of_vec64 v) ~src_pos:0 ~dst:buf ~dst_pos:pos ~len:size;
  next

let bin_write_vec = bin_write_float64_vec

let bin_write_float32_mat buf ~pos m =
  let len1 = Array2.dim1 m in
  let len2 = Array2.dim2 m in
  let pos = bin_write_nat0 buf ~pos (Nat0.unsafe_of_int len1) in
  let pos = bin_write_nat0 buf ~pos (Nat0.unsafe_of_int len2) in
  let size = len1 * len2 * 4 in
  let next = pos + size in
  check_next buf next;
  unsafe_blit_buf ~src:(buf_of_mat32 m) ~src_pos:0 ~dst:buf ~dst_pos:pos ~len:size;
  next

let bin_write_float64_mat buf ~pos m =
  let len1 = Array2.dim1 m in
  let len2 = Array2.dim2 m in
  let pos = bin_write_nat0 buf ~pos (Nat0.unsafe_of_int len1) in
  let pos = bin_write_nat0 buf ~pos (Nat0.unsafe_of_int len2) in
  let size = len1 * len2 * 8 in
  let next = pos + size in
  check_next buf next;
  unsafe_blit_buf ~src:(buf_of_mat64 m) ~src_pos:0 ~dst:buf ~dst_pos:pos ~len:size;
  next

let bin_write_mat = bin_write_float64_mat

let bin_write_bigstring buf ~pos s =
  let len = Array1.dim s in
  let plen = Nat0.unsafe_of_int len in
  let pos = bin_write_nat0 buf ~pos plen in
  let next = pos + len in
  check_next buf next;
  unsafe_blit_buf ~src:s ~src_pos:0 ~dst:buf ~dst_pos:pos ~len;
  next

let bin_write_variant_int buf ~pos x =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32le buf pos (Int32.logor (Int32.shift_left (Int32.of_int x) 1) 1l);
  next

let bin_write_int_8bit buf ~pos n =
  assert_pos pos;
  check_pos buf pos;
  unsafe_set8 buf pos n;
  pos + 1

let bin_write_int_16bit buf ~pos n =
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  unsafe_set16le buf pos n;
  next

let bin_write_int_32bit buf ~pos n =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32le buf pos (Int32.of_int n);
  next

let bin_write_int_64bit buf ~pos n =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64le buf pos (Int64.of_int n);
  next

let bin_write_int64_bits buf ~pos n =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64le buf pos n;
  next

let bin_write_network16_int buf ~pos n =
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  unsafe_set16be buf pos n;
  next

let bin_write_network32_int buf ~pos n =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32be buf pos (Int32.of_int n);
  next

let bin_write_network32_int32 buf ~pos n =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32be buf pos n;
  next

let bin_write_network64_int buf ~pos n =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64be buf pos (Int64.of_int n);
  next

let bin_write_network64_int64 buf ~pos n =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64be buf pos n;
  next

let bin_write_array_no_length bin_write_el buf ~pos ar =
  bin_write_array_loop bin_write_el buf ~els_pos:pos ~n:(Array.length ar) ar

external unsafe_string_get32 : string -> int -> int32 = "%caml_string_get32u";;
external unsafe_string_get64 : string -> int -> int64 = "%caml_string_get64u";;

let bin_write_md5 buf ~pos x =
  let x = Md5_lib.to_binary x in
  assert (String.length x = 16);
  assert_pos pos;
  let next = pos + 16 in
  check_next buf next;
  if arch_sixtyfour then begin
    let a = unsafe_string_get64 x 0 in
    let b = unsafe_string_get64 x 8 in
    unsafe_set64 buf  pos      a;
    unsafe_set64 buf (pos + 8) b;
  end else begin
    let a = unsafe_string_get32 x  0 in
    let b = unsafe_string_get32 x  4 in
    let c = unsafe_string_get32 x  8 in
    let d = unsafe_string_get32 x 12 in
    unsafe_set32 buf  pos       a;
    unsafe_set32 buf (pos +  4) b;
    unsafe_set32 buf (pos +  8) c;
    unsafe_set32 buf (pos + 12) d;
  end;
  next
