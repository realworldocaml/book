(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let format_typ fmt t = Ctypes.format_typ fmt t
let format_fn fmt fn = Ctypes.format_fn fmt fn
let format_sint fmt v =
  Format.fprintf fmt "<sint %s>" (Signed.SInt.to_string v)
let format_long fmt v =
  Format.fprintf fmt "<long %s>" (Signed.Long.to_string v)
let format_llong fmt v =
  Format.fprintf fmt "<llong %s>" (Signed.LLong.to_string v)
let format_uchar fmt v =
  Format.fprintf fmt "<uchar %s>" (Unsigned.UChar.to_string v)
let format_uint8 fmt v =
  Format.fprintf fmt "<uint8 %s>" (Unsigned.UInt8.to_string v)
let format_uint16 fmt v =
  Format.fprintf fmt "<uint16 %s>" (Unsigned.UInt16.to_string v)
let format_uint32 fmt v =
  Format.fprintf fmt "<uint32 %s>" (Unsigned.UInt32.to_string v)
let format_uint64 fmt v =
  Format.fprintf fmt "<uint64 %s>" (Unsigned.UInt64.to_string v)
let format_ushort fmt v =
  Format.fprintf fmt "<ushort %s>" (Unsigned.UShort.to_string v)
let format_uint fmt v =
  Format.fprintf fmt "<uint %s>" (Unsigned.UInt.to_string v)
let format_ulong fmt v =
  Format.fprintf fmt "<ulong %s>" (Unsigned.ULong.to_string v)
let format_ullong fmt v =
  Format.fprintf fmt "<ullong %s>" (Unsigned.ULLong.to_string v)
let format_pointer fmt v =
  let open Ctypes in
  let typ = ptr (reference_type v) in
  Format.fprintf fmt "(%a) %a" (fun fmt -> format_typ fmt) typ (format typ) v
let format_struct fmt v =
  Ctypes.(format (reference_type (addr v)) fmt v)
let format_union fmt v =
  Ctypes.(format (reference_type (addr v)) fmt v)
let format_array fmt v =
  Ctypes.(format CArray.(array (length v) (reference_type (start v))) fmt v)
let format_ocaml fmt (Ctypes_static.OCamlRef (_, _, ty) as v) =
  Ctypes.format (Ctypes_static.OCaml ty) fmt v
let format_clock_t fmt v =
  Ctypes.format PosixTypes.clock_t fmt v
let format_dev_t fmt v =
  Ctypes.format PosixTypes.dev_t fmt v
let format_ino_t fmt v =
  Ctypes.format PosixTypes.ino_t fmt v
let format_mode_t fmt v =
  Ctypes.format PosixTypes.mode_t fmt v
let format_nlink_t fmt v =
  Ctypes.format PosixTypes.nlink_t fmt v
let format_off_t fmt v =
  Ctypes.format PosixTypes.off_t fmt v
let format_pid_t fmt v =
  Ctypes.format PosixTypes.pid_t fmt v
let format_size_t fmt v =
  Ctypes.format PosixTypes.size_t fmt v
let format_ssize_t fmt v =
  Ctypes.format PosixTypes.ssize_t fmt v
let format_time_t fmt v =
  Ctypes.format PosixTypes.time_t fmt v
let format_useconds_t fmt v =
  Ctypes.format PosixTypes.useconds_t fmt v
let format_ldouble fmt v = 
  Format.fprintf fmt "<ldouble %s>" (LDouble.to_string v)
let format_complexld fmt v = 
  Format.fprintf fmt "<complexld %s + %si>" 
    (LDouble.to_string (ComplexL.re v))
    (LDouble.to_string (ComplexL.im v))
