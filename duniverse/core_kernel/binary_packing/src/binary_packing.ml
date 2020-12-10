open! Core_kernel
open! Import
module Core_char = Char
module Char = Caml.Char
module Int32 = Caml.Int32
module Int64 = Caml.Int64

let arch_sixtyfour = Sys.word_size = 64
let signed_max = Int32.to_int Int32.max_int
let unsigned_max = Int64.to_int 0xffff_ffffL

type endian =
  [ `Big_endian
  | `Little_endian
  ]
[@@deriving compare, hash, sexp]

(* Computes the offset based on the total number of bytes, the byte order, and the
   byte number. The byte number is ordered by decreasing significance starting at zero
   (big endian). So the most significant byte is 0, and the least significant byte is (len
   - 1). *)

exception Binary_packing_invalid_byte_number of int * int [@@deriving sexp]

let offset ~len ~byte_order byte_nr =
  if byte_nr >= len || byte_nr < 0
  then raise (Binary_packing_invalid_byte_number (byte_nr, len));
  match byte_order with
  | `Little_endian -> len - 1 - byte_nr
  | `Big_endian -> byte_nr
;;

exception Pack_unsigned_8_argument_out_of_range of int [@@deriving sexp]

let pack_unsigned_8 ~buf ~pos n =
  if n > 0xFF || n < 0
  then raise (Pack_unsigned_8_argument_out_of_range n)
  else Bytes.set buf pos (Char.unsafe_chr n)
;;

let unpack_unsigned_8 ~buf ~pos = Char.code (Bytes.get buf pos)

exception Pack_signed_8_argument_out_of_range of int [@@deriving sexp]

let pack_signed_8 ~buf ~pos n =
  if n > 0x7F || n < -0x80
  then raise (Pack_signed_8_argument_out_of_range n)
  else Bytes.set buf pos (Char.unsafe_chr n)
;;

let unpack_signed_8 ~buf ~pos =
  let n = unpack_unsigned_8 ~buf ~pos in
  if n >= 0x80 then -(0x100 - n) else n
;;

exception Pack_unsigned_16_argument_out_of_range of int [@@deriving sexp]

let pack_unsigned_16 ~byte_order ~buf ~pos n =
  if n >= 0x10000 || n < 0
  then raise (Pack_unsigned_16_argument_out_of_range n)
  else (
    Bytes.set
      buf
      (pos + offset ~len:2 ~byte_order 0)
      (Char.unsafe_chr (0xFF land (n asr 8)));
    Bytes.set buf (pos + offset ~len:2 ~byte_order 1) (Char.unsafe_chr (0xFF land n)))
;;

let pack_unsigned_16_big_endian ~buf ~pos n =
  if n >= 0x10000 || n < 0
  then raise (Pack_unsigned_16_argument_out_of_range n)
  else (
    Bytes.set buf pos (Char.unsafe_chr (0xFF land (n lsr 8)));
    Bytes.set buf (pos + 1) (Char.unsafe_chr (0xFF land n)))
;;

let pack_unsigned_16_little_endian ~buf ~pos n =
  if n >= 0x10000 || n < 0
  then raise (Pack_unsigned_16_argument_out_of_range n)
  else (
    Bytes.set buf (pos + 1) (Char.unsafe_chr (0xFF land (n lsr 8)));
    Bytes.set buf pos (Char.unsafe_chr (0xFF land n)))
;;

exception Pack_signed_16_argument_out_of_range of int [@@deriving sexp]

let pack_signed_16 ~byte_order ~buf ~pos n =
  if n > 0x7FFF || n < -0x8000
  then raise (Pack_signed_16_argument_out_of_range n)
  else (
    Bytes.set
      buf
      (pos + offset ~len:2 ~byte_order 0)
      (Char.unsafe_chr (0xFF land (n asr 8)));
    Bytes.set buf (pos + offset ~len:2 ~byte_order 1) (Char.unsafe_chr (0xFF land n)))
;;

let pack_signed_16_big_endian ~buf ~pos n =
  if n > 0x7FFF || n < -0x8000
  then raise (Pack_signed_16_argument_out_of_range n)
  else (
    Bytes.set buf pos (Char.unsafe_chr (0xFF land (n asr 8)));
    Bytes.set buf (pos + 1) (Char.unsafe_chr (0xFF land n)))
;;

let pack_signed_16_little_endian ~buf ~pos n =
  if n > 0x7FFF || n < -0x8000
  then raise (Pack_signed_16_argument_out_of_range n)
  else (
    Bytes.set buf (pos + 1) (Char.unsafe_chr (0xFF land (n asr 8)));
    Bytes.set buf pos (Char.unsafe_chr (0xFF land n)))
;;

let unpack_unsigned_16 ~byte_order ~buf ~pos =
  let b1 = Char.code (Bytes.get buf (pos + offset ~len:2 ~byte_order 0)) lsl 8 in
  let b2 = Char.code (Bytes.get buf (pos + offset ~len:2 ~byte_order 1)) in
  b1 lor b2
;;

let unpack_signed_16 ~byte_order ~buf ~pos =
  let n = unpack_unsigned_16 ~byte_order ~buf ~pos in
  if n >= 0x8000 then -(0x10000 - n) else n
;;

let unpack_unsigned_16_big_endian ~buf ~pos =
  let b1 = Char.code (Bytes.get buf pos) lsl 8 in
  let b2 = Char.code (Bytes.get buf (pos + 1)) in
  b1 lor b2
;;

let unpack_unsigned_16_little_endian ~buf ~pos =
  let b1 = Char.code (Bytes.get buf (pos + 1)) lsl 8 in
  let b2 = Char.code (Bytes.get buf pos) in
  b1 lor b2
;;

let unpack_signed_16_big_endian ~buf ~pos =
  let n = unpack_unsigned_16_big_endian ~buf ~pos in
  if n >= 0x8000 then -(0x10000 - n) else n
;;

let unpack_signed_16_little_endian ~buf ~pos =
  let n = unpack_unsigned_16_little_endian ~buf ~pos in
  if n >= 0x8000 then -(0x10000 - n) else n
;;

exception Pack_unsigned_32_argument_out_of_range of int [@@deriving sexp]

let check_unsigned_32_in_range n =
  if arch_sixtyfour
  then (
    if n > unsigned_max || n < 0 then raise (Pack_unsigned_32_argument_out_of_range n))
  else if n < 0
  then raise (Pack_unsigned_32_argument_out_of_range n)
;;

let pack_unsigned_32_int ~byte_order ~buf ~pos n =
  assert (Sys.word_size = 64);
  check_unsigned_32_in_range n;
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 0)
    (Char.unsafe_chr (0xFF land (n asr 24)));
  (* MSB *)
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 1)
    (Char.unsafe_chr (0xFF land (n asr 16)));
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 2)
    (Char.unsafe_chr (0xFF land (n asr 8)));
  Bytes.set buf (pos + offset ~len:4 ~byte_order 3) (Char.unsafe_chr (0xFF land n))
;;

(* LSB *)

let pack_unsigned_32_int_big_endian ~buf ~pos n =
  check_unsigned_32_in_range n;
  Bytes.set buf pos (Char.unsafe_chr (0xFF land (n lsr 24)));
  (* MSB *)
  Bytes.set buf (pos + 3) (Char.unsafe_chr (0xFF land n));
  (* LSB *)
  Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (n lsr 16)));
  Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (n lsr 8)))
;;

let pack_unsigned_32_int_little_endian ~buf ~pos n =
  check_unsigned_32_in_range n;
  Bytes.set buf (pos + 3) (Char.unsafe_chr (0xFF land (n lsr 24)));
  (* MSB *)
  Bytes.set buf pos (Char.unsafe_chr (0xFF land n));
  (* LSB *)
  Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (n lsr 16)));
  Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (n lsr 8)))
;;

exception Pack_signed_32_argument_out_of_range of int [@@deriving sexp]

let check_signed_32_in_range n =
  if arch_sixtyfour
  then
    if n > signed_max || n < -(signed_max + 1)
    then raise (Pack_signed_32_argument_out_of_range n)
;;

let pack_signed_32_int ~byte_order ~buf ~pos n =
  assert (Sys.word_size = 64);
  check_signed_32_in_range n;
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 0)
    (Char.unsafe_chr (0xFF land (n asr 24)));
  (* MSB *)
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 1)
    (Char.unsafe_chr (0xFF land (n asr 16)));
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 2)
    (Char.unsafe_chr (0xFF land (n asr 8)));
  Bytes.set buf (pos + offset ~len:4 ~byte_order 3) (Char.unsafe_chr (0xFF land n))
;;

(* LSB *)

let pack_signed_32_int_big_endian ~buf ~pos n =
  check_signed_32_in_range n;
  Bytes.set buf pos (Char.unsafe_chr (0xFF land (n asr 24)));
  (* MSB *)
  Bytes.set buf (pos + 3) (Char.unsafe_chr (0xFF land n));
  (* LSB *)
  Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (n asr 16)));
  Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (n asr 8)))
;;

let pack_signed_32_int_little_endian ~buf ~pos n =
  check_signed_32_in_range n;
  Bytes.set buf (pos + 3) (Char.unsafe_chr (0xFF land (n asr 24)));
  (* MSB *)
  Bytes.set buf pos (Char.unsafe_chr (0xFF land n));
  (* LSB *)
  Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (n asr 16)));
  Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (n asr 8)))
;;

let pack_signed_32 ~byte_order ~buf ~pos n =
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 0)
    (Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 24)));
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 1)
    (Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 16)));
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 2)
    (Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 8)));
  Bytes.set
    buf
    (pos + offset ~len:4 ~byte_order 3)
    (Char.unsafe_chr (0xFF land Int32.to_int n))
;;

let unpack_signed_32 ~byte_order ~buf ~pos =
  let b1 =
    (* MSB *)
    Int32.shift_left
      (Int32.of_int (Char.code (Bytes.get buf (pos + offset ~len:4 ~byte_order 0))))
      24
  in
  let b2 = Char.code (Bytes.get buf (pos + offset ~len:4 ~byte_order 1)) lsl 16 in
  let b3 = Char.code (Bytes.get buf (pos + offset ~len:4 ~byte_order 2)) lsl 8 in
  let b4 = Char.code (Bytes.get buf (pos + offset ~len:4 ~byte_order 3)) in
  (* LSB *)
  Int32.logor b1 (Int32.of_int (b2 lor b3 lor b4))
;;

let unpack_unsigned_32_int ~byte_order ~buf ~pos =
  assert (Sys.word_size = 64);
  let b1 = Char.code (Bytes.get buf (pos + offset ~len:4 ~byte_order 0)) lsl 24 in
  (* msb *)
  let b2 = Char.code (Bytes.get buf (pos + offset ~len:4 ~byte_order 1)) lsl 16 in
  let b3 = Char.code (Bytes.get buf (pos + offset ~len:4 ~byte_order 2)) lsl 8 in
  let b4 = Char.code (Bytes.get buf (pos + offset ~len:4 ~byte_order 3)) in
  (* lsb *)
  b1 lor b2 lor b3 lor b4
;;

let unpack_unsigned_32_int_big_endian ~buf ~pos =
  let b1 = Char.code (Bytes.get buf pos) lsl 24 in
  (* msb *)
  let b4 = Char.code (Bytes.get buf (pos + 3)) in
  (* lsb *)
  let b2 = Char.code (Bytes.unsafe_get buf (pos + 1)) lsl 16 in
  let b3 = Char.code (Bytes.unsafe_get buf (pos + 2)) lsl 8 in
  b1 lor b2 lor b3 lor b4
;;

let unpack_unsigned_32_int_little_endian ~buf ~pos =
  let b1 = Char.code (Bytes.get buf (pos + 3)) lsl 24 in
  (* msb *)
  let b4 = Char.code (Bytes.get buf pos) in
  (* lsb *)
  let b2 = Char.code (Bytes.unsafe_get buf (pos + 2)) lsl 16 in
  let b3 = Char.code (Bytes.unsafe_get buf (pos + 1)) lsl 8 in
  b1 lor b2 lor b3 lor b4
;;

let unpack_signed_32_int ~byte_order ~buf ~pos =
  let n = unpack_unsigned_32_int ~byte_order ~buf ~pos in
  if arch_sixtyfour && n > signed_max then -(((signed_max + 1) lsl 1) - n) else n
;;

let unpack_signed_32_int_big_endian ~buf ~pos =
  let n = unpack_unsigned_32_int_big_endian ~buf ~pos in
  if arch_sixtyfour && n > signed_max then n - (unsigned_max + 1) else n
;;

let unpack_signed_32_int_little_endian ~buf ~pos =
  let n = unpack_unsigned_32_int_little_endian ~buf ~pos in
  if arch_sixtyfour && n > signed_max then n - (unsigned_max + 1) else n
;;

let pack_signed_64 ~byte_order ~buf ~pos v =
  let top3 = Int64.to_int (Int64.shift_right v 40) in
  let mid3 = Int64.to_int (Int64.shift_right v 16) in
  let bot2 = Int64.to_int v in
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 0)
    (Char.unsafe_chr (0xFF land (top3 lsr 16)));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 1)
    (Char.unsafe_chr (0xFF land (top3 lsr 8)));
  Bytes.set buf (pos + offset ~len:8 ~byte_order 2) (Char.unsafe_chr (0xFF land top3));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 3)
    (Char.unsafe_chr (0xFF land (mid3 lsr 16)));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 4)
    (Char.unsafe_chr (0xFF land (mid3 lsr 8)));
  Bytes.set buf (pos + offset ~len:8 ~byte_order 5) (Char.unsafe_chr (0xFF land mid3));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 6)
    (Char.unsafe_chr (0xFF land (bot2 lsr 8)));
  Bytes.set buf (pos + offset ~len:8 ~byte_order 7) (Char.unsafe_chr (0xFF land bot2))
;;

let pack_signed_64_big_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  Bytes.set
    buf
    pos
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 56))));
  Bytes.set buf (pos + 7) (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL v)));
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  Bytes.unsafe_set
    buf
    (pos + 1)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 48))));
  Bytes.unsafe_set
    buf
    (pos + 2)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 40))));
  Bytes.unsafe_set
    buf
    (pos + 3)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 32))));
  Bytes.unsafe_set
    buf
    (pos + 4)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 24))));
  Bytes.unsafe_set
    buf
    (pos + 5)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 16))));
  Bytes.unsafe_set
    buf
    (pos + 6)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 8))))
;;

let pack_signed_64_little_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  Bytes.set buf pos (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL v)));
  Bytes.set
    buf
    (pos + 7)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 56))));
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  Bytes.unsafe_set
    buf
    (pos + 1)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 8))));
  Bytes.unsafe_set
    buf
    (pos + 2)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 16))));
  Bytes.unsafe_set
    buf
    (pos + 3)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 24))));
  Bytes.unsafe_set
    buf
    (pos + 4)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 32))));
  Bytes.unsafe_set
    buf
    (pos + 5)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 40))));
  Bytes.unsafe_set
    buf
    (pos + 6)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 48))))
;;

let unpack_signed_64 ~byte_order ~buf ~pos =
  Int64.logor
    (Int64.logor
       (Int64.shift_left
          (Int64.of_int
             ((Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 0)) lsl 16)
              lor (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 1)) lsl 8)
              lor Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 2))))
          40)
       (Int64.shift_left
          (Int64.of_int
             ((Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 3)) lsl 16)
              lor (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 4)) lsl 8)
              lor Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 5))))
          16))
    (Int64.of_int
       ((Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 6)) lsl 8)
        lor Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 7))))
;;

let unpack_signed_64_big_endian ~buf ~pos =
  (* Do bounds checking only on the first and last bytes *)
  let b1 = Char.code (Bytes.get buf pos)
  and b8 = Char.code (Bytes.get buf (pos + 7)) in
  let b2 = Char.code (Bytes.unsafe_get buf (pos + 1))
  and b3 = Char.code (Bytes.unsafe_get buf (pos + 2))
  and b4 = Char.code (Bytes.unsafe_get buf (pos + 3))
  and b5 = Char.code (Bytes.unsafe_get buf (pos + 4))
  and b6 = Char.code (Bytes.unsafe_get buf (pos + 5))
  and b7 = Char.code (Bytes.unsafe_get buf (pos + 6)) in
  if arch_sixtyfour
  then (
    let i1 = Int64.of_int b1
    and i2 =
      Int64.of_int
        ((b2 lsl 48)
         lor (b3 lsl 40)
         lor (b4 lsl 32)
         lor (b5 lsl 24)
         lor (b6 lsl 16)
         lor (b7 lsl 8)
         lor b8)
    in
    Int64.(logor i2 (shift_left i1 56)))
  else (
    let i1 = Int64.of_int ((b1 lsl 8) lor b2)
    and i2 = Int64.of_int ((b3 lsl 16) lor (b4 lsl 8) lor b5)
    and i3 = Int64.of_int ((b6 lsl 16) lor (b7 lsl 8) lor b8) in
    Int64.(logor i3 (logor (shift_left i2 24) (shift_left i1 48))))
;;

let unpack_signed_64_little_endian ~buf ~pos =
  (* Do bounds checking only on the first and last bytes *)
  let b1 = Char.code (Bytes.get buf pos)
  and b8 = Char.code (Bytes.get buf (pos + 7)) in
  let b2 = Char.code (Bytes.unsafe_get buf (pos + 1))
  and b3 = Char.code (Bytes.unsafe_get buf (pos + 2))
  and b4 = Char.code (Bytes.unsafe_get buf (pos + 3))
  and b5 = Char.code (Bytes.unsafe_get buf (pos + 4))
  and b6 = Char.code (Bytes.unsafe_get buf (pos + 5))
  and b7 = Char.code (Bytes.unsafe_get buf (pos + 6)) in
  if arch_sixtyfour
  then (
    let i1 =
      Int64.of_int
        (b1
         lor (b2 lsl 8)
         lor (b3 lsl 16)
         lor (b4 lsl 24)
         lor (b5 lsl 32)
         lor (b6 lsl 40)
         lor (b7 lsl 48))
    and i2 = Int64.of_int b8 in
    Int64.(logor i1 (shift_left i2 56)))
  else (
    let i1 = Int64.of_int (b1 lor (b2 lsl 8) lor (b3 lsl 16))
    and i2 = Int64.of_int (b4 lor (b5 lsl 8) lor (b6 lsl 16))
    and i3 = Int64.of_int (b7 lor (b8 lsl 8)) in
    Int64.(logor i1 (logor (shift_left i2 24) (shift_left i3 48))))
;;

let pack_signed_64_int ~byte_order ~buf ~pos n =
  assert (Sys.word_size = 64);
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 0)
    (Char.unsafe_chr (0xFF land (n asr 56)));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 1)
    (Char.unsafe_chr (0xFF land (n asr 48)));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 2)
    (Char.unsafe_chr (0xFF land (n asr 40)));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 3)
    (Char.unsafe_chr (0xFF land (n asr 32)));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 4)
    (Char.unsafe_chr (0xFF land (n asr 24)));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 5)
    (Char.unsafe_chr (0xFF land (n asr 16)));
  Bytes.set
    buf
    (pos + offset ~len:8 ~byte_order 6)
    (Char.unsafe_chr (0xFF land (n asr 8)));
  Bytes.set buf (pos + offset ~len:8 ~byte_order 7) (Char.unsafe_chr (0xFF land n))
;;

(* It's important to use [asr] not [lsr] in [pack_signed_64_int_big_endian] and
   [pack_signed_64_int_little_endian] so that the most significant byte is encoded
   correctly.  (It might be helpful to think about this as widening, i.e. sign
   extending, the number to 64 bits and then doing the right shift by 56.)
*)

let pack_signed_64_int_big_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  Bytes.set buf pos (Char.unsafe_chr (0xFF land (v asr 56)));
  Bytes.set buf (pos + 7) (Char.unsafe_chr (0xFF land v));
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (v asr 48)));
  Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (v asr 40)));
  Bytes.unsafe_set buf (pos + 3) (Char.unsafe_chr (0xFF land (v asr 32)));
  Bytes.unsafe_set buf (pos + 4) (Char.unsafe_chr (0xFF land (v asr 24)));
  Bytes.unsafe_set buf (pos + 5) (Char.unsafe_chr (0xFF land (v asr 16)));
  Bytes.unsafe_set buf (pos + 6) (Char.unsafe_chr (0xFF land (v asr 8)))
;;

let pack_signed_64_int_little_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  Bytes.set buf pos (Char.unsafe_chr (0xFF land v));
  Bytes.set buf (pos + 7) (Char.unsafe_chr (0xFF land (v asr 56)));
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (v asr 8)));
  Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (v asr 16)));
  Bytes.unsafe_set buf (pos + 3) (Char.unsafe_chr (0xFF land (v asr 24)));
  Bytes.unsafe_set buf (pos + 4) (Char.unsafe_chr (0xFF land (v asr 32)));
  Bytes.unsafe_set buf (pos + 5) (Char.unsafe_chr (0xFF land (v asr 40)));
  Bytes.unsafe_set buf (pos + 6) (Char.unsafe_chr (0xFF land (v asr 48)))
;;

let unpack_signed_64_int ~byte_order ~buf ~pos =
  assert (Sys.word_size = 64);
  (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 0)) lsl 56)
  lor (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 1)) lsl 48)
  lor (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 2)) lsl 40)
  lor (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 3)) lsl 32)
  lor (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 4)) lsl 24)
  lor (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 5)) lsl 16)
  lor (Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 6)) lsl 8)
  lor Char.code (Bytes.get buf (pos + offset ~len:8 ~byte_order 7))
;;

exception Unpack_signed_64_int_most_significant_byte_too_large of int [@@deriving sexp]

let check_highest_order_byte_range byte =
  if byte < 64 || byte >= 192
  then ()
  else raise (Unpack_signed_64_int_most_significant_byte_too_large byte)
;;

let unpack_signed_64_int_big_endian ~buf ~pos =
  assert (Sys.word_size = 64);
  (* Do bounds checking only on the first and last bytes *)
  let b1 = Char.code (Bytes.get buf pos)
  and b8 = Char.code (Bytes.get buf (pos + 7)) in
  let b2 = Char.code (Bytes.unsafe_get buf (pos + 1))
  and b3 = Char.code (Bytes.unsafe_get buf (pos + 2))
  and b4 = Char.code (Bytes.unsafe_get buf (pos + 3))
  and b5 = Char.code (Bytes.unsafe_get buf (pos + 4))
  and b6 = Char.code (Bytes.unsafe_get buf (pos + 5))
  and b7 = Char.code (Bytes.unsafe_get buf (pos + 6)) in
  check_highest_order_byte_range b1;
  (b1 lsl 56)
  lor (b2 lsl 48)
  lor (b3 lsl 40)
  lor (b4 lsl 32)
  lor (b5 lsl 24)
  lor (b6 lsl 16)
  lor (b7 lsl 8)
  lor b8
;;

let unpack_signed_64_int_little_endian ~buf ~pos =
  assert (Sys.word_size = 64);
  (* Do bounds checking only on the first and last bytes *)
  let b1 = Char.code (Bytes.get buf pos)
  and b8 = Char.code (Bytes.get buf (pos + 7)) in
  let b2 = Char.code (Bytes.unsafe_get buf (pos + 1))
  and b3 = Char.code (Bytes.unsafe_get buf (pos + 2))
  and b4 = Char.code (Bytes.unsafe_get buf (pos + 3))
  and b5 = Char.code (Bytes.unsafe_get buf (pos + 4))
  and b6 = Char.code (Bytes.unsafe_get buf (pos + 5))
  and b7 = Char.code (Bytes.unsafe_get buf (pos + 6)) in
  check_highest_order_byte_range b8;
  b1
  lor (b2 lsl 8)
  lor (b3 lsl 16)
  lor (b4 lsl 24)
  lor (b5 lsl 32)
  lor (b6 lsl 40)
  lor (b7 lsl 48)
  lor (b8 lsl 56)
;;

let pack_float ~byte_order ~buf ~pos f =
  pack_signed_64 ~byte_order ~buf ~pos (Int64.bits_of_float f)
;;

let unpack_float ~byte_order ~buf ~pos =
  Int64.float_of_bits (unpack_signed_64 ~byte_order ~buf ~pos)
;;

let rec last_nonmatch_plus_one ~buf ~min_pos ~pos ~char =
  let pos' = pos - 1 in
  if pos' >= min_pos && Core_char.( = ) (Bytes.get buf pos') char
  then last_nonmatch_plus_one ~buf ~min_pos ~pos:pos' ~char
  else pos
;;

let unpack_tail_padded_fixed_string ?(padding = '\x00') ~buf ~pos ~len () =
  let data_end =
    last_nonmatch_plus_one ~buf ~min_pos:pos ~pos:(pos + len) ~char:padding
  in
  Bytes.To_string.sub buf ~pos ~len:(data_end - pos)
;;

exception
  Pack_tail_padded_fixed_string_argument_too_long of
    [ `s of string ] * [ `longer_than ] * [ `len of int ]
[@@deriving sexp]

let pack_tail_padded_fixed_string ?(padding = '\x00') ~buf ~pos ~len s =
  let slen = String.length s in
  if slen > len
  then
    raise
      (Pack_tail_padded_fixed_string_argument_too_long (`s s, `longer_than, `len len))
  else (
    Bytes.From_string.blit ~src:s ~dst:buf ~src_pos:0 ~dst_pos:pos ~len:slen;
    if slen < len
    then (
      let diff = len - slen in
      Bytes.fill buf ~pos:(pos + slen) ~len:diff padding))
;;

module Private = struct
  let last_nonmatch_plus_one = last_nonmatch_plus_one

  exception
    Unpack_signed_64_int_most_significant_byte_too_large = Unpack_signed_64_int_most_significant_byte_too_large
end
