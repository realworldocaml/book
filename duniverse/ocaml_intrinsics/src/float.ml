(** Rounds a [float] to an [int64] using the current rounding mode. The default
    rounding mode is "round half to even", and we expect that no program will
    change the rounding mode.

    If the argument is NaN or infinite or if the rounded value cannot be
    represented, then the result is unspecified.

    On an x86-64 machine, this compiles to [cvtsd2si rax, xmm0]. *)
external iround_half_to_even
  :  (float[@unboxed])
  -> (int64[@unboxed])
  = "caml_float_iround_half_to_even" "caml_float_iround_half_to_even_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Equivalent to [if x < y then x else y].

    On an x86-64 machine, this compiles to [minsd xmm0, xmm1]. *)
external min
  :  (float[@unboxed])
  -> (float[@unboxed])
  -> (float[@unboxed])
  = "caml_float_min" "caml_float_min_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Equivalent to [if x > y then x else y].

    On an x86-64 machine, this compiles to [maxsd xmm0, xmm1]. *)
external max
  :  (float[@unboxed])
  -> (float[@unboxed])
  -> (float[@unboxed])
  = "caml_float_max" "caml_float_max_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Rounds a [float] to an integer [float] using the current rounding
    mode.  The default rounding mode is "round half to even", and we
    expect that no program will change the rounding mode.

    On an x86-64 machine, this compiles to [roundsd xmm0, xmm1, $12].
    Requires SSE4.1. *)
external round_half_to_even
  :  (float[@unboxed])
  -> (float[@unboxed])
  = "caml_float_round_current" "caml_float_round_current_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Rounds a [float] to an integer [float] using the mode specified
    in the function name. *)
external round_down
  :  (float[@unboxed])
  -> (float[@unboxed])
  = "caml_float_round_down" "caml_float_round_down_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external round_up
  :  (float[@unboxed])
  -> (float[@unboxed])
  = "caml_float_round_up" "caml_float_round_up_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external round_towards_zero
  :  (float[@unboxed])
  -> (float[@unboxed])
  = "caml_float_round_towards_zero" "caml_float_round_towards_zero_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
