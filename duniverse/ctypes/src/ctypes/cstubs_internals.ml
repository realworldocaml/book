(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Types and functions used by generated ML code.  This is an internal
   interface and subject to change. *)

type voidp = Ctypes_ptr.voidp
type managed_buffer = Ctypes_memory_stubs.managed_buffer
type 'a fatptr = 'a Ctypes.typ Ctypes_ptr.Fat.t
type 'a fatfunptr = 'a Ctypes.fn Ctypes_ptr.Fat.t

let make_structured reftyp buf =
  let open Ctypes_static in
  let managed = Obj.repr buf in
  let raw_ptr = Ctypes_memory_stubs.block_address buf in
  { structured = CPointer (Ctypes_ptr.Fat.make ~managed ~reftyp raw_ptr) }

include Ctypes_static
include Ctypes_primitive_types

let make_ptr reftyp raw_ptr = CPointer (Ctypes_ptr.Fat.make ~reftyp raw_ptr)
let make_fun_ptr reftyp raw_ptr = Static_funptr (Ctypes_ptr.Fat.make ~reftyp raw_ptr)

let cptr (CPointer p) = p
let fptr (Static_funptr p) = p

let mkView :
  type a b. string -> a typ -> typedef:bool -> unexpected:(a -> b) -> (b * a) list -> b typ =
  fun name typ ~typedef ~unexpected alist ->
    let typedef = if typedef then "" else "enum " in
    let rlist = List.map (fun (l, r) -> (r, l)) alist in
    let write k = List.assoc k alist
    and read k = try List.assoc k rlist with Not_found -> unexpected k
    and format_typ k fmt = Format.fprintf fmt "%s%s%t" typedef name k in
    view typ ~format_typ ~read ~write

let map_assocv f = List.map (fun (k, v) -> (k, f v))

let int8_of_int64 = Int64.to_int
let int64_of_int8 = Int64.of_int
let int16_of_int64 = Int64.to_int
let int64_of_int16 = Int64.of_int
let int32_of_int64 = Int64.to_int32
let int64_of_int32 = Int64.of_int32
let int64_of_int64 x = x
(* For now we use conversion via strings: there's certainly room for
   improvement.  The conversion from int64_t to uint8_t isn't safe in general,
   of course, so we don't have it available.  However, we can be confident
   that conversion will work in this particular case, since we know that the
   underlying type is actually uint8_t, so the value can certainly be
   represented.

   In mitigation, these conversions are performed once during "startup", not
   each time we read and write enum values.  *)
let uint8_of_int64 x = Unsigned.UInt8.of_string (Int64.to_string x)
let int64_of_uint8 x = Int64.of_int (Unsigned.UInt8.to_int x)
let uint16_of_int64 x = Unsigned.UInt16.of_string (Int64.to_string x)
let int64_of_uint16 x = Int64.of_int (Unsigned.UInt16.to_int x)
let uint32_of_int64 x = Unsigned.UInt32.of_string (Int64.to_string x)
let int64_of_uint32 x = Int64.of_string (Unsigned.UInt32.to_string x)
let uint64_of_int64 = Unsigned.UInt64.of_int64
let int64_of_uint64 = Unsigned.UInt64.to_int64

let build_enum_type name underlying ?(typedef=false) ?unexpected alist =
  let build_view t coerce uncoerce =
    let unexpected = match unexpected with
        Some u -> fun x -> u (uncoerce x)
      | None   -> fun x ->
        Printf.ksprintf failwith "Unexpected enum value for %s: %Ld"
          name (uncoerce x)
    in
    mkView name t ~typedef ~unexpected (map_assocv coerce alist) in
  match underlying with
    Ctypes_static.Int8 -> build_view Ctypes.int8_t int8_of_int64 int64_of_int8
  | Ctypes_static.Int16 -> build_view Ctypes.int16_t int16_of_int64 int64_of_int16
  | Ctypes_static.Int32 -> build_view Ctypes.int32_t int32_of_int64 int64_of_int32
  | Ctypes_static.Int64 -> build_view Ctypes.int64_t int64_of_int64 int64_of_int64
  | Ctypes_static.Uint8 -> build_view Ctypes.uint8_t uint8_of_int64 int64_of_uint8
  | Ctypes_static.Uint16 -> build_view Ctypes.uint16_t uint16_of_int64 int64_of_uint16
  | Ctypes_static.Uint32 -> build_view Ctypes.uint32_t uint32_of_int64 int64_of_uint32
  | Ctypes_static.Uint64 -> build_view Ctypes.uint64_t uint64_of_int64 int64_of_uint64
  | Ctypes_static.Float | Ctypes_static.Double ->
    Printf.ksprintf failwith
      "Enum type detected as floating type: %s" name

let use_value v = Ctypes_memory_stubs.use_value v
