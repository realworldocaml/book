(** [count_leading_zeros n] returns the number of most-significant
    zero bits before the most significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 32 or 64, depending on the target. *)
external count_leading_zeros
  :  (nativeint[@unboxed])
  -> (int[@untagged])
  = "caml_nativeint_clz" "caml_nativeint_clz_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Same as [count_leading_zeros] except if the argument is zero,
    then the result is undefined. Emits more efficient code. *)
external count_leading_zeros_nonzero_arg
  :  (nativeint[@unboxed])
  -> (int[@untagged])
  = "caml_nativeint_clz" "caml_nativeint_clz_nonzero_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [count_trailing_zeros n] returns the number of least-significant
    zero bits before the least significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 32 or 64, depending on the target. *)
external count_trailing_zeros
  :  (nativeint[@unboxed])
  -> (int[@untagged])
  = "caml_nativeint_ctz" "caml_nativeint_ctz_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Same as [count_trailing_zeros] except if the argument is zero,
    then the result is undefined. Emits more efficient code. *)
external count_trailing_zeros_nonzero_arg
  :  (nativeint[@unboxed])
  -> (int[@untagged])
  = "caml_nativeint_ctz" "caml_nativeint_ctz_nonzero_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
external count_set_bits
  :  (nativeint[@unboxed])
  -> (int[@untagged])
  = "caml_nativeint_popcnt" "caml_nativeint_popcnt_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
