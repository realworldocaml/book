[@@@warning "-32"]

let sign8 v =
  (v lsl ( Sys.int_size - 8 )) asr ( Sys.int_size - 8 )
  [@@ocaml.inline]

let sign16 v =
  (v lsl ( Sys.int_size - 16 )) asr ( Sys.int_size - 16 )
  [@@ocaml.inline]

let get_uint8 s off =
  Char.code (get_char s off)
  [@@ocaml.inline]
let get_int8 s off =
  ((get_uint8 s off) lsl ( Sys.int_size - 8 )) asr ( Sys.int_size - 8 )
  [@@ocaml.inline]
let set_int8 s off v =
  (* It is ok to cast using unsafe_chr because both String.set
     and Bigarray.Array1.set (on bigstrings) use the 'store unsigned int8'
     primitives that effectively extract the bits before writing *)
  set_char s off (Char.unsafe_chr v)
  [@@ocaml.inline]

let unsafe_get_uint8 s off =
  Char.code (unsafe_get_char s off)
  [@@ocaml.inline]
let unsafe_get_int8 s off =
  ((unsafe_get_uint8 s off) lsl ( Sys.int_size - 8 )) asr ( Sys.int_size - 8 )
  [@@ocaml.inline]
let unsafe_set_int8 s off v =
  unsafe_set_char s off (Char.unsafe_chr v)
  [@@ocaml.inline]
