(* Size: compute size of values in the binary protocol. *)

open Bigarray

let arch_sixtyfour = Sys.word_size = 64

open Common

module Maximum = struct
  let bin_size_unit            = 1
  let bin_size_bool            = 1
  let bin_size_char            = 1
  let bin_size_md5             = 16
  let bin_size_int_nat0        = if arch_sixtyfour then 9 else 5
  let bin_size_int_negative    = if arch_sixtyfour then 9 else 5
  let bin_size_int             = max bin_size_int_nat0 bin_size_int_negative
  let bin_size_float           = 8
  let bin_size_int32           = 5
  let bin_size_int64           = 9
  let bin_size_nativeint       = bin_size_int
  let bin_size_nat0            = bin_size_int_nat0
  let bin_size_variant_int     = 4
  let bin_size_int_8bit        = 1
  let bin_size_int_16bit       = 2
  let bin_size_int_32bit       = 4
  let bin_size_int_64bit       = 8
  let bin_size_int64_bits      = 8
  let bin_size_network16_int   = 2
  let bin_size_network32_int   = 4
  let bin_size_network32_int32 = 4
  let bin_size_network64_int   = 8
  let bin_size_network64_int64 = 8
end

module Minimum = struct
  let bin_size_unit            = Maximum.bin_size_unit
  let bin_size_bool            = Maximum.bin_size_bool
  let bin_size_char            = Maximum.bin_size_char
  let bin_size_md5             = 16
  let bin_size_int_nat0        = 1
  let bin_size_int_negative    = 2
  let bin_size_int             = min bin_size_int_nat0 bin_size_int_negative
  let bin_size_float           = Maximum.bin_size_float
  let bin_size_int32           = bin_size_int
  let bin_size_int64           = bin_size_int
  let bin_size_nativeint       = bin_size_int
  let bin_size_nat0            = 1
  let bin_size_ref             = 1
  let bin_size_lazy_t          = 1
  let bin_size_option          = 1
  let bin_size_pair            = 1 + 1
  let bin_size_triple          = 1 + 1 + 1
  let bin_size_len             = bin_size_nat0
  let bin_size_list            = bin_size_len
  let bin_size_array           = bin_size_len
  let bin_size_hashtbl         = bin_size_len
  let bin_size_string          = bin_size_len
  let bin_size_bytes           = bin_size_len
  let bin_size_vec             = bin_size_len
  let bin_size_float32_vec     = bin_size_vec
  let bin_size_float64_vec     = bin_size_vec
  let bin_size_mat             = bin_size_len + bin_size_len
  let bin_size_float32_mat     = bin_size_mat
  let bin_size_float64_mat     = bin_size_mat
  let bin_size_bigstring       = bin_size_len
  let bin_size_float_array     = bin_size_len
  let bin_size_variant_int     = Maximum.bin_size_variant_int
  let bin_size_int_8bit        = Maximum.bin_size_int_8bit
  let bin_size_int_16bit       = Maximum.bin_size_int_16bit
  let bin_size_int_32bit       = Maximum.bin_size_int_32bit
  let bin_size_int_64bit       = Maximum.bin_size_int_64bit
  let bin_size_int64_bits      = Maximum.bin_size_int64_bits
  let bin_size_network16_int   = Maximum.bin_size_network16_int
  let bin_size_network32_int   = Maximum.bin_size_network32_int
  let bin_size_network32_int32 = Maximum.bin_size_network32_int32
  let bin_size_network64_int   = Maximum.bin_size_network64_int
  let bin_size_network64_int64 = Maximum.bin_size_network64_int64
end

type 'a sizer = 'a -> int
type ('a, 'b) sizer1 = 'a sizer -> 'b sizer
type ('a, 'b, 'c) sizer2 = 'a sizer -> ('b, 'c) sizer1
type ('a, 'b, 'c, 'd) sizer3 = 'a sizer -> ('b, 'c, 'd) sizer2

let bin_size_unit () = 1

let bin_size_bool _ = 1

let bin_size_int_nat0 n =
  if      n  < 0x00000080 then 1
  else if n  < 0x00008000 then 3
  else if arch_sixtyfour && n >= (* 0x80000000 *) (1 lsl 31) then 9
  else 5

let bin_size_int_negative n =
  if      n >= -0x00000080 then 2
  else if n >= -0x00008000 then 3
  else if arch_sixtyfour && n  < (* -0x80000000 *) -(1 lsl 31) then 9
  else 5

let bin_size_char _ = 1

let bin_size_int n =
  if n >= 0 then bin_size_int_nat0 n
  else bin_size_int_negative n

let bin_size_nat0 nat0 =
  let n = (nat0 : Nat0.t :> int) in
  if      n <   0x00000080 then 1
  else if n <   0x00010000 then 3
  else if arch_sixtyfour && n >= (* 0x100000000 *) (1 lsl 32) then 9
  else 5

let bin_size_string_or_bytes len =
  let plen = Nat0.unsafe_of_int len in
  let size_len = bin_size_nat0 plen in
  size_len + len

let bin_size_string str = bin_size_string_or_bytes (String.length str)

let bin_size_bytes str = bin_size_string_or_bytes (Bytes.length str)

let bin_size_md5 _ = 16

let bin_size_float f =
  (* If we just ignore the argument the compiler will still require it to exist and be
     boxed. This means that if for instance we call this for a field of a float record,
     the compiler will allocate the float for nothing.

     With this line the compiler really ignores the float. *)
  ignore (truncate f);
  8
;;

let bin_size_int32 =
  if arch_sixtyfour
  then fun n -> bin_size_int (Int32.to_int n)
  else fun n ->
    if n >= 0x00008000l || n < -0x00008000l then 5
    else bin_size_int (Int32.to_int n)

let bin_size_int64 =
  if arch_sixtyfour
  then fun n ->
    if n >= 0x80000000L || n < -0x80000000L then 9
    else bin_size_int (Int64.to_int n)
  else fun n ->
    if n >= 0x80000000L || n < -0x80000000L then 9
    else bin_size_int32 (Int64.to_int32 n)

let bin_size_nativeint =
  if arch_sixtyfour
  then
    fun n -> bin_size_int64 (Int64.of_nativeint n)
  else
    fun n -> bin_size_int32 (Nativeint.to_int32 n)

let bin_size_ref bin_size_el r = bin_size_el !r
let bin_size_lazy_t bin_size_el lv = bin_size_el (Lazy.force lv)
let bin_size_lazy = bin_size_lazy_t

let bin_size_option bin_size_el = function
  | None   -> 1
  | Some v -> 1 + bin_size_el v

let bin_size_pair bin_size_a bin_size_b (a, b) = bin_size_a a + bin_size_b b

let bin_size_triple bin_size_a bin_size_b bin_size_c (a, b, c) =
  bin_size_a a + bin_size_b b + bin_size_c c

let bin_size_list bin_size_el lst =
  let rec loop len = function
    | [] -> len
    | h :: t -> loop (len + bin_size_el h) t
  in
  let len = Nat0.unsafe_of_int (List.length lst) in
  let size_len = bin_size_nat0 len in
  loop size_len lst

let bin_size_len len =
  let plen = Nat0.unsafe_of_int len in
  bin_size_nat0 plen

let bin_size_float_array ar =
  let len = Array.length ar in
  bin_size_len len + 8 * len

let bin_size_array_loop bin_size_el ar ~total_len ~n =
  let total_len_ref = ref total_len in
  for i = 0 to n - 1 do
    let el = Array.unsafe_get ar i in
    total_len_ref := !total_len_ref + bin_size_el el
  done;
  !total_len_ref

let bin_size_array (type a) bin_size_el ar =
  if (Obj.magic (bin_size_el : a sizer) : float sizer) == bin_size_float then
    bin_size_float_array (Obj.magic (ar : a array) : float array)
  else
    let n = Array.length ar in
    let total_len = bin_size_len n in
    bin_size_array_loop bin_size_el ar ~total_len ~n

let bin_size_hashtbl bin_size_key bin_size_val htbl =
  let cnt_ref = ref 0 in
  let coll_htbl k v total_len =
    incr cnt_ref;
    total_len + bin_size_key k + bin_size_val v
  in
  let len = Hashtbl.length htbl in
  let total_len = Hashtbl.fold coll_htbl htbl (bin_size_len len) in
  if !cnt_ref <> len then raise_concurrent_modification "bin_size_hashtbl";
  total_len

let bin_size_gen_vec vec multiplier =
  let len = Array1.dim vec in
  bin_size_len len + multiplier * len

let bin_size_float32_vec vec = bin_size_gen_vec vec 4
let bin_size_float64_vec vec = bin_size_gen_vec vec 8
let bin_size_vec = bin_size_float64_vec

let bin_size_gen_mat mat multiplier =
  let dim1 = Array2.dim1 mat in
  let dim2 = Array2.dim2 mat in
  let size = dim1 * dim2 in
  bin_size_len dim1 + bin_size_len dim2 + multiplier * size

let bin_size_float32_mat mat = bin_size_gen_mat mat 4
let bin_size_float64_mat mat = bin_size_gen_mat mat 8
let bin_size_mat = bin_size_float64_mat

let bin_size_bigstring buf = bin_size_gen_vec buf 1

let bin_size_variant_int _ = 4

let bin_size_int_8bit _ = 1
let bin_size_int_16bit _ = 2
let bin_size_int_32bit _ = 4
let bin_size_int_64bit _ = 8
let bin_size_int64_bits _ = 8

let bin_size_network16_int _ = 2
let bin_size_network32_int _ = 4
let bin_size_network32_int32 _ = 4
let bin_size_network64_int _ = 8
let bin_size_network64_int64 _ = 8

let bin_size_array_no_length bin_size_el ar =
  bin_size_array_loop bin_size_el ar ~total_len:0 ~n:(Array.length ar)
