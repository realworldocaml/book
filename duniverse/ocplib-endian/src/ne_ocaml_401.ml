  let get_uint16 s off =
    get_16 s off
  [@@ocaml.inline]

  let get_int16 s off =
   ((get_uint16 s off) lsl ( Sys.int_size - 16 )) asr ( Sys.int_size - 16 )
  [@@ocaml.inline]

  let get_int32 s off =
    get_32 s off
  [@@ocaml.inline]

  let get_int64 s off =
    get_64 s off
  [@@ocaml.inline]

  let set_int16 s off v =
    set_16 s off v
  [@@ocaml.inline]

  let set_int32 s off v =
    set_32 s off v
  [@@ocaml.inline]

  let set_int64 s off v =
    set_64 s off v
  [@@ocaml.inline]
