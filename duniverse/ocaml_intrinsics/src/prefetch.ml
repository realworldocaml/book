type temporal_locality =
  | None
  | Low
  | Moderate
  | High

type operation =
  | Read
  | Write

(* The compiler needs to know statically the operation and the temporal locality hints,
   because they correspond to different instructions on amd64 target and different GCC
   builtins in the C stubs.  We have one "external" declaration for each pair of
   operation, locality, and pointer type. Naming convention:
   caml_prefetch_<operation>_<temporal_locality>_<pointer_type>

   Prefetching hints are meant for highly-optimized code, the bytecode stubs do nothing,
   so they all call the same C stub [caml_prefetch_ignore].
*)

(* Prefetching primitives should not be annotated with [@@no_effects].
   Otherwise, the compiler can eliminate them, because they have no result.
*)

(* Native_pointer *)

external prefetch_write_high_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_high_native_pointer_unboxed"
[@@noalloc] [@@builtin]

external prefetch_write_moderate_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_moderate_native_pointer_unboxed"
[@@noalloc] [@@builtin]

external prefetch_write_low_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_low_native_pointer_unboxed"
[@@noalloc] [@@builtin]

external prefetch_write_none_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_none_native_pointer_unboxed"
[@@noalloc] [@@builtin]

external prefetch_read_none_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_none_native_pointer_unboxed"
[@@noalloc] [@@builtin]

external prefetch_read_low_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_low_native_pointer_unboxed"
[@@noalloc] [@@builtin]

external prefetch_read_moderate_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_moderate_native_pointer_unboxed"
[@@noalloc] [@@builtin]

external prefetch_read_high_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_high_native_pointer_unboxed"
[@@noalloc] [@@builtin]

(* Ext_pointer *)

external prefetch_write_high_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_high_ext_pointer"
[@@noalloc] [@@builtin]

external prefetch_write_moderate_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_moderate_ext_pointer"
[@@noalloc] [@@builtin]

external prefetch_write_low_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_low_ext_pointer"
[@@noalloc] [@@builtin]

external prefetch_write_none_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_none_ext_pointer"
[@@noalloc] [@@builtin]

external prefetch_read_none_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_none_ext_pointer"
[@@noalloc] [@@builtin]

external prefetch_read_low_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_low_ext_pointer"
[@@noalloc] [@@builtin]

external prefetch_read_moderate_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_moderate_ext_pointer"
[@@noalloc] [@@builtin]

external prefetch_read_high_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_high_ext_pointer"
[@@noalloc] [@@builtin]

(* Bigstring *)

type bigstring =
  ( char
  , Stdlib.Bigarray.int8_unsigned_elt
  , Stdlib.Bigarray.c_layout )
    Stdlib.Bigarray.Array1.t

external prefetch_write_high_bigstring
  :  bigstring
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_high_bigstring_untagged"
[@@noalloc] [@@builtin]

external prefetch_write_moderate_bigstring
  :  bigstring
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_moderate_bigstring_untagged"
[@@noalloc] [@@builtin]

external prefetch_write_low_bigstring
  :  bigstring
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_low_bigstring_untagged"
[@@noalloc] [@@builtin]

external prefetch_write_none_bigstring
  :  bigstring
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_none_bigstring_untagged"
[@@noalloc] [@@builtin]

external prefetch_read_none_bigstring
  :  bigstring
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_none_bigstring_untagged"
[@@noalloc] [@@builtin]

external prefetch_read_low_bigstring
  :  bigstring
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_low_bigstring_untagged"
[@@noalloc] [@@builtin]

external prefetch_read_moderate_bigstring
  :  bigstring
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_moderate_bigstring_untagged"
[@@noalloc] [@@builtin]

external prefetch_read_high_bigstring
  :  bigstring
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_high_bigstring_untagged"
[@@noalloc] [@@builtin]

let native_pointer p ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> prefetch_write_high_native_pointer p
  | Write, Moderate -> prefetch_write_moderate_native_pointer p
  | Write, Low -> prefetch_write_low_native_pointer p
  | Write, None -> prefetch_write_none_native_pointer p
  | Read, None -> prefetch_read_none_native_pointer p
  | Read, Low -> prefetch_read_low_native_pointer p
  | Read, Moderate -> prefetch_read_moderate_native_pointer p
  | Read, High -> prefetch_read_high_native_pointer p
;;

let ext_pointer p ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> prefetch_write_high_ext_pointer p
  | Write, Moderate -> prefetch_write_moderate_ext_pointer p
  | Write, Low -> prefetch_write_low_ext_pointer p
  | Write, None -> prefetch_write_none_ext_pointer p
  | Read, None -> prefetch_read_none_ext_pointer p
  | Read, Low -> prefetch_read_low_ext_pointer p
  | Read, Moderate -> prefetch_read_moderate_ext_pointer p
  | Read, High -> prefetch_read_high_ext_pointer p
;;

let bigstring bigstring ~pos ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> prefetch_write_high_bigstring bigstring pos
  | Write, Moderate -> prefetch_write_moderate_bigstring bigstring pos
  | Write, Low -> prefetch_write_low_bigstring bigstring pos
  | Write, None -> prefetch_write_none_bigstring bigstring pos
  | Read, None -> prefetch_read_none_bigstring bigstring pos
  | Read, Low -> prefetch_read_low_bigstring bigstring pos
  | Read, Moderate -> prefetch_read_moderate_bigstring bigstring pos
  | Read, High -> prefetch_read_high_bigstring bigstring pos
;;

external pause : unit -> unit = "caml_pause_hint" [@@noalloc] [@@builtin]
