/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#if !__USE_MINGW_ANSI_STDIO && (defined(__MINGW32__) || defined(__MINGW64__))
#define __USE_MINGW_ANSI_STDIO 1
#endif

#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <caml/fail.h>

#include <inttypes.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>

#define OCAML_INTEGERS_INTERNAL 1
#include "ocaml_integers.h"

#define UINT_DECLS(BITS)                                                    \
  extern value integers_copy_uint ## BITS(uint ## BITS ## _t u);            \
  /* uintX_add : t -> t -> t */                                             \
  extern value integers_uint ## BITS ## _ ## add(value a, value b);         \
  /* uintX_sub : t -> t -> t */                                             \
  extern value integers_uint ## BITS ## _ ## sub(value a, value b);         \
  /* uintX_mul : t -> t -> t */                                             \
  extern value integers_uint ## BITS ## _ ## mul(value a, value b);         \
  /* uintX_div : t -> t -> t */                                             \
  extern value integers_uint ## BITS ## _ ## div(value a, value b);         \
  /* uintX_rem : t -> t -> t */                                             \
  extern value integers_uint ## BITS ## _ ## rem(value a, value b);         \
  /* uintX_logand : t -> t -> t */                                          \
  extern value integers_uint ## BITS ## _ ## logand(value a, value b);      \
  /* uintX_logor : t -> t -> t */                                           \
  extern value integers_uint ## BITS ## _ ## logor(value a, value b);       \
  /* uintX_logxor : t -> t -> t */                                          \
  extern value integers_uint ## BITS ## _ ## logxor(value a, value b);      \
  /* uintX_shift_left : t -> t -> t */                                      \
  extern value integers_uint ## BITS ## _ ## shift_left(value a, value b);  \
  /* uintX_shift_right : t -> t -> t */                                     \
  extern value integers_uint ## BITS ## _ ## shift_right(value a, value b); \
  /* of_int : int -> t */                                                   \
  extern value integers_uint ## BITS ## _of_int(value a);                   \
  /* to_int : t -> int */                                                   \
  extern value integers_uint ## BITS ## _to_int(value a);                   \
  /* of_string : string -> t */                                             \
  extern value integers_uint ## BITS ## _of_string(value a);                \
  /* to_string : t -> string */                                             \
  extern value integers_uint ## BITS ## _to_string(value a);                \
  /* max : unit -> t */                                                     \
  extern value integers_uint ## BITS ## _max(value a);

#define UINT_SMALL_DECLS(BITS)                                              \
  /* of_string : string -> t */                                             \
  extern value integers_uint ## BITS ## _of_string(value a);                \
  /* to_string : t -> string */                                             \
  extern value integers_uint ## BITS ## _to_string(value a);                \
  /* max : unit -> t */                                                     \
  extern value integers_uint ## BITS ## _max(value a);

UINT_SMALL_DECLS(8)
UINT_SMALL_DECLS(16)
UINT_DECLS(32)
UINT_DECLS(64)

/* X_size : unit -> int */
extern value integers_size_t_size (value _);
extern value integers_ushort_size (value _);
extern value integers_uint_size (value _);
extern value integers_ulong_size (value _);
extern value integers_ulonglong_size (value _);


static int parse_digit(char c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return -1;
}

#define Uint_custom_val(SIZE, V) Uint_custom_val_(SIZE, V)
#define Uint_custom_val_(SIZE, V) \
  (*(uint ## SIZE ## _t *)(Data_custom_val(V)))

#define TYPE(SIZE) uint ## SIZE ## _t
#define BUF_SIZE(TYPE) ((sizeof(TYPE) * CHAR_BIT + 2) / 3 + 1)

#define UINT_PRIMOP(NAME, SIZE, OP)                                          \
  /* OP : t -> t -> t */                                                     \
  value integers_uint ## SIZE ## _ ## NAME(value a, value b)                 \
  {                                                                          \
    return integers_copy_uint ## SIZE(Uint_custom_val(SIZE, a)               \
                                 OP Uint_custom_val(SIZE, b));             \
  }

#define UINT_OF_STRING(BITS, COPY)                                           \
  value integers_uint ## BITS ## _of_string(value a)                         \
  {                                                                          \
    TYPE(BITS) u, max_prefix;                                                \
    const char *pos = String_val(a);                                         \
    int base = 10, d;                                                        \
                                                                             \
    /* Strip a leading + sign, if given */                                   \
    if (*pos == '+') pos++;                                                  \
    if (*pos == '0') {                                                       \
      switch (pos[1]) {                                                      \
        case 'x': case 'X':                                                  \
          base = 16; pos += 2; break;                                        \
        case 'o': case 'O':                                                  \
          base = 8; pos += 2; break;                                         \
        case 'b': case 'B':                                                  \
          base = 2; pos += 2; break;                                         \
        case 'u': case 'U': /* Unsigned prefix. No-op for unsigned types */  \
          pos += 2; break;                                                   \
      }                                                                      \
    }                                                                        \
                                                                             \
    max_prefix = ((TYPE(BITS)) -1) / base;                                   \
                                                                             \
    d = parse_digit(*pos);                                                   \
    if (d < 0 || d >= base) {                                                \
      caml_failwith("UInt"#BITS".of_string");                                \
    }                                                                        \
    u = (TYPE(BITS)) d;                                                      \
    pos++;                                                                   \
                                                                             \
    for (;; pos++) {                                                         \
      if (*pos == '_') continue;                                             \
      d = parse_digit(*pos);                                                 \
      /* Halt if the digit isn't valid (or this is the string terminator) */ \
      if (d < 0 || d >= base) break;                                         \
      /* Check that we can add another digit */                              \
      if (u > max_prefix) break;                                             \
      u = d + u * base;                                                      \
      /* Check for overflow */                                               \
      if (u < (TYPE(BITS)) d) break;                                         \
    }                                                                        \
                                                                             \
    if (pos != String_val(a) + caml_string_length(a)){                       \
      caml_failwith("UInt"#BITS".of_string");                                \
    }                                                                        \
                                                                             \
    return COPY(u);                                                          \
  }                                                                          \

#define UINT_DEFS(BITS, BYTES)                                               \
  static int uint ## BITS ## _cmp(value v1, value v2)                        \
  {                                                                          \
    TYPE(BITS) u1 = Uint_custom_val(BITS, v1);                               \
    TYPE(BITS) u2 = Uint_custom_val(BITS, v2);                               \
    return (u1 > u2) - (u1 < u2);                                            \
  }                                                                          \
                                                                             \
  static intnat uint ## BITS ## _hash(value v)                               \
  {                                                                          \
    return Uint_custom_val(BITS, v);                                         \
  }                                                                          \
                                                                             \
  static void uint ## BITS ## _serialize(value v,                            \
                                         uintnat *wsize_32,                  \
                                         uintnat *wsize_64)                  \
  {                                                                          \
    caml_serialize_int_ ## BYTES(Uint_custom_val(BITS, v));                  \
    *wsize_32 = *wsize_64 = BYTES;                                           \
  }                                                                          \
                                                                             \
  static uintnat uint ## BITS ## _deserialize(void *dst)                     \
  {                                                                          \
    *(TYPE(BITS) *)dst = caml_deserialize_uint_ ## BYTES();                  \
    return BYTES;                                                            \
  }                                                                          \
                                                                             \
  static struct custom_operations caml_uint ## BITS ## _ops = {              \
    "integers:uint" #BITS,                                                   \
    custom_finalize_default,                                                 \
    uint ## BITS ## _cmp,                                                    \
    uint ## BITS ## _hash,                                                   \
    uint ## BITS ## _serialize,                                              \
    uint ## BITS ## _deserialize,                                            \
    custom_compare_ext_default                                               \
  };                                                                         \
                                                                             \
  value integers_copy_uint ## BITS(TYPE(BITS) u)                             \
  {                                                                          \
    value res = caml_alloc_custom(&caml_uint ## BITS ## _ops, BYTES, 0, 1);  \
    Uint_custom_val(BITS, res) = u;                                          \
    return res;                                                              \
  }                                                                          \
  UINT_PRIMOP(add, BITS,  +)                                                 \
  UINT_PRIMOP(sub, BITS,  -)                                                 \
  UINT_PRIMOP(mul, BITS,  *)                                                 \
  UINT_PRIMOP(logand, BITS,  &)                                              \
  UINT_PRIMOP(logor, BITS,  |)                                               \
  UINT_PRIMOP(logxor, BITS,  ^)                                              \
                                                                             \
  /* div : t -> t -> t */                                                    \
  value integers_uint ## BITS ## _div(value n_, value d_)                    \
  {                                                                          \
    TYPE(BITS) n = Uint_custom_val(BITS, n_);                                \
    TYPE(BITS) d = Uint_custom_val(BITS, d_);                                \
    if (d == (TYPE(BITS)) 0)                                                 \
        caml_raise_zero_divide();                                            \
    return integers_copy_uint ## BITS (n / d);                               \
  }                                                                          \
                                                                             \
  /* rem : t -> t -> t */                                                    \
  value integers_uint ## BITS ## _rem(value n_, value d_)                    \
  {                                                                          \
    TYPE(BITS) n = Uint_custom_val(BITS, n_);                                \
    TYPE(BITS) d = Uint_custom_val(BITS, d_);                                \
    if (d == (TYPE(BITS)) 0)                                                 \
        caml_raise_zero_divide();                                            \
    return integers_copy_uint ## BITS (n % d);                               \
  }                                                                          \
                                                                             \
  /* shift_left : t -> int -> t */                                           \
  value integers_uint ## BITS ## _shift_left(value a, value b)               \
  {                                                                          \
    return integers_copy_uint ## BITS(Uint_custom_val(BITS, a)               \
                                    << Long_val(b));                         \
  }                                                                          \
                                                                             \
  /* shift_right : t -> int -> t */                                          \
  value integers_uint ## BITS ## _shift_right(value a, value b)              \
  {                                                                          \
    return integers_copy_uint ## BITS(Uint_custom_val(BITS, a)               \
                                    >> Long_val(b));                         \
  }                                                                          \
                                                                             \
  /* of_int : int -> t */                                                    \
  value integers_uint ## BITS ## _of_int(value a)                            \
  {                                                                          \
    return integers_copy_uint ## BITS (Long_val(a));                         \
  }                                                                          \
                                                                             \
  /* to_int : t -> int */                                                    \
  value integers_uint ## BITS ## _to_int(value a)                            \
  {                                                                          \
    return Val_long(Uint_custom_val(BITS, a));                               \
  }                                                                          \
                                                                             \
  /* of_int64 : int64 -> t */                                                \
  value integers_uint ## BITS ## _of_int64(value a)                          \
  {                                                                          \
    return integers_copy_uint ## BITS(Int64_val(a));                         \
  }                                                                          \
                                                                             \
  /* to_int64 : t -> int64 */                                                \
  value integers_uint ## BITS ## _to_int64(value a)                          \
  {                                                                          \
    return caml_copy_int64(Uint_custom_val(BITS, a));                        \
  }                                                                          \
                                                                             \
  /* of_string : string -> t */                                              \
  UINT_OF_STRING(BITS, integers_copy_uint ## BITS)                           \
                                                                             \
  /* to_string : t -> string */                                              \
  value integers_uint ## BITS ## _to_string(value a)                         \
  {                                                                          \
    char buf[BUF_SIZE(TYPE(BITS))];                                          \
    if (sprintf(buf, "%" PRIu ## BITS , Uint_custom_val(BITS, a)) < 0)       \
      caml_failwith("UInt ## BITS ## .to_string");                           \
    else                                                                     \
      return caml_copy_string(buf);                                          \
  }                                                                          \
                                                                             \
  /* to_hexstring : t -> string */                                           \
  value integers_uint ## BITS ## _to_hexstring(value a)                      \
  {                                                                          \
    char buf[BUF_SIZE(TYPE(BITS))];                                          \
    if (sprintf(buf, "%" PRIx ## BITS , Uint_custom_val(BITS, a)) < 0)       \
      caml_failwith("UInt ## BITS ## .to_hexstring");                        \
    else                                                                     \
      return caml_copy_string(buf);                                          \
  }                                                                          \
                                                                             \
  /* max : unit -> t */                                                      \
  value integers_uint ## BITS ## _max(value a)                               \
  {                                                                          \
    return integers_copy_uint ## BITS ((TYPE(BITS))(-1));                    \
  }

#define UINT_SMALL_DEFS(BITS, BYTES)                                         \
  /* of_string : string -> t */                                              \
  UINT_OF_STRING(BITS, Integers_val_uint ## BITS)                            \
                                                                             \
  /* to_string : t -> string */                                              \
  value integers_uint ## BITS ## _to_string(value a)                         \
  {                                                                          \
    char buf[BUF_SIZE(TYPE(BITS))];                                          \
    if (sprintf(buf, "%" PRIu ## BITS , Uint ## BITS ##_val(a)) < 0)         \
      caml_failwith("UInt ## BITS ## .to_string");                           \
    else                                                                     \
      return caml_copy_string(buf);                                          \
  }                                                                          \
                                                                             \
  /* to_hexstring : t -> string */                                           \
  value integers_uint ## BITS ## _to_hexstring(value a)                      \
  {                                                                          \
    char buf[BUF_SIZE(TYPE(BITS))];                                          \
    if (sprintf(buf, "%" PRIx ## BITS , Uint ## BITS ##_val(a)) < 0)         \
      caml_failwith("UInt ## BITS ## .to_hexstring");                        \
    else                                                                     \
      return caml_copy_string(buf);                                          \
  }                                                                          \
                                                                             \
  /* max : unit -> t */                                                      \
  value integers_uint ## BITS ## _max(value unit)                            \
  {                                                                          \
     return Integers_val_uint ## BITS((TYPE(BITS))(-1));                     \
  }

UINT_SMALL_DEFS(8, 1)
UINT_SMALL_DEFS(16, 2)
UINT_DEFS(32, 4)
UINT_DEFS(64, 8)

value integers_size_t_size (value _) { return Val_long(sizeof (size_t)); }
value integers_ushort_size (value _) { return Val_long(sizeof (unsigned short)); }
value integers_uint_size (value _) { return Val_long(sizeof (unsigned int)); }
value integers_ulong_size (value _) { return Val_long(sizeof (unsigned long)); }
value integers_ulonglong_size (value _) { return Val_long(sizeof (unsigned long long)); }
value integers_uint32_of_int32 (value i) { return integers_copy_uint32(Int32_val(i)); }
value integers_int32_of_uint32 (value u) { return caml_copy_int32(Uint_custom_val(32, u)); }
value integers_uintptr_t_size (value _) { return Val_long(sizeof (uintptr_t)); }
value integers_intptr_t_size (value _) { return Val_long(sizeof (intptr_t)); }
value integers_ptrdiff_t_size (value _) { return Val_long(sizeof (ptrdiff_t)); }
value integers_uint32_of_uint64 (value u) { return integers_copy_uint32(Uint_custom_val(64,u)); }
value integers_uint64_of_uint32 (value u) { return integers_copy_uint64(Uint_custom_val(32,u)); }

value integers_unsigned_init(value unit)
{
  caml_register_custom_operations(&caml_uint32_ops);
  caml_register_custom_operations(&caml_uint64_ops);
  return Val_unit;
}
