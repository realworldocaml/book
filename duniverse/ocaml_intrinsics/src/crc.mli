external int_crc : initial:int -> data:int -> int = "caml_int_crc" "caml_int_crc_untagged"
[@@noalloc] [@@untagged] [@@builtin] [@@no_effects] [@@no_coeffects]

external int64_crc
  :  initial:(int[@untagged])
  -> data:(int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_crc" "caml_int64_crc_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Accumulates [iterations] of [int_crc].
    If [iterations] < 0, raises Invalid_argument. *)
val iterated_crc_exn : initial:int -> iterations:int -> data:int -> int
