(** Generates the crc32q machine instruction, a 64bit accumulate CRC32 (polynomial
    0x11EDC6F41) value.

    requires sse4.2 support. (-msse4.2) *)
external int_crc : initial:int -> data:int -> int = "caml_int_crc" "caml_int_crc_untagged"
[@@noalloc] [@@untagged] [@@builtin] [@@no_effects] [@@no_coeffects]

external int64_crc
  :  initial:(int[@untagged])
  -> data:(int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_crc" "caml_int64_crc_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

let iterated_crc_exn ~initial ~iterations ~data =
  if iterations < 0
  then
    raise
      (Invalid_argument
         (Printf.sprintf
            "iterated_crc: iterations=%d is invalid, must be non-negative value"
            iterations));
  let rec loop i ~acc =
    if i < iterations then loop (i + 1) ~acc:(int_crc ~initial:0 ~data:acc) else acc
  in
  loop 1 ~acc:(int_crc ~initial ~data)
;;
