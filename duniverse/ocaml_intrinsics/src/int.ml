(** The are two version of [count_leading_zeros], [count_set_bits] each, which differ in
    their native code implementation.  The first version takes as input a tagged integer
    and the second version takes as input an untagged integer.  Generally, the first
    version (that operates on a tagged integer) is faster, but if the integer is already
    untagged, it may be faster to use the second version.
*)

(** [count_leading_zeros n] returns the number of most-significant
    zero bits before the most significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 31 or 63, depending on the target. *)
external count_leading_zeros
  :  int
  -> (int[@untagged])
  = "caml_int_clz" "caml_int_clz_tagged_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external count_leading_zeros2
  :  int
  -> int
  = "caml_int_clz" "caml_int_clz_untagged_to_untagged"
[@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
external count_set_bits
  :  int
  -> (int[@untagged])
  = "caml_int_popcnt" "caml_int_popcnt_tagged_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external count_set_bits2
  :  int
  -> int
  = "caml_int_popcnt" "caml_int_popcnt_untagged_to_untagged"
[@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [count_trailing_zeros n] returns the number of least-significant
    zero bits before the least significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 31 or 63, depending on the target. *)
external count_trailing_zeros
  :  int
  -> int
  = "caml_int_ctz" "caml_int_ctz_untagged_to_untagged"
[@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
