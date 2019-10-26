(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let string_of_char_ptr (Ctypes_static.CPointer p) =
  Ctypes_std_view_stubs.string_of_cstring p

let char_ptr_of_string s =
  let managed = Ctypes_std_view_stubs.cstring_of_string s in
  Ctypes_static.CPointer (Ctypes_ptr.Fat.make ~managed ~reftyp:Ctypes_static.char
                     (Ctypes_memory_stubs.block_address managed))

let string = Ctypes_static.(view (ptr char))
  ~read:string_of_char_ptr ~write:char_ptr_of_string

let read_nullable t reftyp =
  let coerce = Ctypes_coerce.coerce Ctypes_static.(ptr reftyp) t in
  fun p -> if Ctypes_memory.is_null p then None else Some (coerce p)

let write_nullable t reftyp =
  let coerce = Ctypes_coerce.coerce t Ctypes_static.(ptr reftyp) in
  Ctypes_memory.(function None -> from_voidp reftyp null | Some f -> coerce f)

let nullable_view ?format_typ ?format t reftyp =
  let read = read_nullable t reftyp
  and write = write_nullable t reftyp
  in Ctypes_static.(view ~read ~write ?format_typ ?format (ptr reftyp))

let read_nullable_funptr t reftyp =
  let coerce = Ctypes_coerce.coerce (Ctypes_static.static_funptr reftyp) t in
  fun (Ctypes_static.Static_funptr p as ptr) ->
    if Ctypes_ptr.Fat.is_null p
    then None
    else Some (coerce ptr)

let write_nullable_funptr t reftyp =
  let coerce = Ctypes_coerce.coerce t Ctypes_static.(static_funptr reftyp) in
  function None -> Ctypes_static.Static_funptr
                     (Ctypes_ptr.Fat.make ~reftyp Ctypes_ptr.Raw.null)
         | Some f -> coerce f

let nullable_funptr_view ?format_typ ?format t reftyp =
  let read = read_nullable_funptr t reftyp
  and write = write_nullable_funptr t reftyp
  in Ctypes_static.(view ~read ~write ?format_typ ?format (static_funptr reftyp))

let ptr_opt t = nullable_view (Ctypes_static.ptr t) t

let string_opt = nullable_view string Ctypes_static.char

module type Signed_type =
sig
  include Signed.S
  val t : t Ctypes_static.typ
end

module type Unsigned_type =
sig
  include Unsigned.S
  val t : t Ctypes_static.typ
end

let signed_typedef name ~size : (module Signed_type) =
  match size with
    1 -> (module struct include Signed.Int
           let t = Ctypes_static.(typedef int8_t name) end)
  | 2 -> (module struct include Signed.Int
           let t = Ctypes_static.(typedef int16_t name) end)
  | 4 -> (module struct include Signed.Int32
           let t = Ctypes_static.(typedef int32_t name) end)
  | 8 -> (module struct include Signed.Int64
           let t = Ctypes_static.(typedef int64_t name) end)
  | n -> Printf.kprintf failwith "size %d not supported for %s\n" n name

let unsigned_typedef name ~size : (module Unsigned_type) =
  match size with
  | 1 -> (module struct include Unsigned.UInt8
           let t = Ctypes_static.(typedef uint8_t name) end)
  | 2 -> (module struct include Unsigned.UInt16
           let t = Ctypes_static.(typedef uint16_t name) end)
  | 4 -> (module struct include Unsigned.UInt32
           let t = Ctypes_static.(typedef uint32_t name) end)
  | 8 -> (module struct include Unsigned.UInt64
           let t = Ctypes_static.(typedef uint64_t name) end)
  | n -> Printf.kprintf failwith "size %d not supported for %s\n" n name

module Intptr = (val signed_typedef "intptr_t"
                    ~size:(Ctypes_std_view_stubs.intptr_t_size ()))
module Uintptr = (val unsigned_typedef "uintptr_t"
                    ~size:(Ctypes_std_view_stubs.uintptr_t_size ()))
let intptr_t = Intptr.t
let uintptr_t = Uintptr.t

module Ptrdiff = (val signed_typedef "ptrdiff_t"
                     ~size:(Ctypes_std_view_stubs.ptrdiff_t_size ()))
let ptrdiff_t = Ptrdiff.t
