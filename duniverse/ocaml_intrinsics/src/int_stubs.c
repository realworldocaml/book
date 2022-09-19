#include "caml/config.h"
#include "caml/alloc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"


#if defined(__GNUC__)
#if ARCH_INT32_TYPE == long
#define int32_clz __builtin_clzl
#define int32_ctz __builtin_ctzl
#define int32_popcnt __builtin_popcountl
#else /* ARCH_INT32_TYPE == long */
#define int32_clz __builtin_clz
#define int32_ctz __builtin_ctz
#define int32_popcnt __builtin_popcount
#endif /* ARCH_INT32_TYPE == long */
#define int64_clz __builtin_clzll
#define int64_ctz __builtin_ctzll
#define int64_popcnt __builtin_popcountll
#else /* defined(__GNUC__) */
#ifdef _MSC_VER
#error "Functionality on Windows has not been tested"
#include <intrin.h>
#pragma intrinsic(_BitScanReverse)

intnat naive_int64_clz(uint64_t v)
{
  unsigned long n;
#ifdef ARCH_SIXTYFOUR
  if (_BitScanReverse64(&n, v)) return 63-n;
  else return 64;
#else
  /* _BitScanReverse64 is not supported */
  if ((v >> 32) == 0)
    {
      if (_BitScanReverse(&n,v)) return 63-n;
      else return 64;
    }
  else
    {
      _BitScanReverse(&n,(v>>32));
      return 31-n;
    }
#endif
}

intnat naive_int32_clz(uint32_t v)
{
  unsigned long n;
  if (_BitScanReverse(&n, v))
#ifdef ARCH_SIXTYFOUR
    return 63 - n;
#else
    return 31 - n;
#endif
  else return 32;
}

#pragma intrinsic(_BitScanForward)

intnat naive_int64_ctz(uint64_t v)
{
  unsigned long n;
#ifdef ARCH_SIXTYFOUR
  if (_BitScanForward64(&n, v)) return n;
  else return 64;
#else
  /* _BitScanForward64 is not supported */
  if ((v << 32) == 0)
    {
      if (_BitScanForward(&n,(v>>32))) return n+32;
      else return 64;
    }
  else
    {
      _BitScanForward(&n,v);
      return n;
    }
#endif
}

intnat naive_int32_ctz(uint32_t v)
{
  unsigned long n;
  if (_BitScanForward(&n, v)) return n
  else return 32;
}

/* _MSVC_ intrinsic for popcnt is not supported on all targets.
   Use naive version of clz and popcnt from Hacker's Delight. */

intnat naive_int64_popcnt (uint64_t x)
{
   int n = 0;
   while (x != 0) {
      n = n + 1;
      x = x & (x - 1);
   }
   return n;
}

intnat naive_int32_popcnt (uint32_t x)
{
   int n = 0;
   while (x != 0) {
      n = n + 1;
      x = x & (x - 1);
   }
   return n;
}

#define int32_clz naive_int32_clz
#define int64_clz naive_int64_clz
#define int32_ctz naive_int32_ctz
#define int64_ctz naive_int64_ctz
#define int32_popcnt naive_int32_popcnt
#define int64_popcnt naive_int64_popcnt
#elseif /* _MSC_VER */
#error "Not __GNUC__ Not _MSC_VER"
#endif /* _MSC_VER */
#endif /* defined(__GNUC__) */


#ifdef ARCH_SIXTYFOUR
static inline intnat int32_clz_for_64bit(uint32_t v)
{
  return int32_clz(v) - 32;
}
#undef int32_clz
#define int32_clz int32_clz_for_64bit
#endif

intnat int32_clz_check_for_zero_arg(uint32_t x)
{
  /* builtin_clz on input 0 is undefined */
  if (x == 0) return 32;
  return int32_clz(x);
}

intnat int64_clz_check_for_zero_arg(uint64_t x)
{
  /* builtin_clz on input 0 is undefined */
  if (x == 0) return 64;
  else return int64_clz(x);
}

intnat int32_ctz_check_for_zero_arg(uint32_t x)
{
  /* builtin_ctz on input 0 is undefined */
  if (x == 0) return 32;
  else return int32_ctz(x);
}

intnat int64_ctz_check_for_zero_arg(uint64_t x)
{
  /* builtin_clz on input 0 is undefined */
  if (x == 0) return 64;
  else return int64_ctz(x);
}

/* Untagging of a negative value shifts in an extra bit.
   The following code clears the shifted sign bit of the argument.
   This straightline code is faster than conditional code
   for checking whether the argument is negative. */
#ifdef ARCH_SIXTYFOUR
static inline uint64_t clear_sign_bit(intnat v1)
{
  return ((uint64_t) v1) & ~(1ull << 63);
}
#else
static inline uint32_t clear_sign_bit(intnat v1)
{
  return ((uint32_t) v1) & ~(1ul << 31);

}
#endif


/* Takes an untagged input and returns untagged output. */
intnat caml_int_clz_untagged_to_untagged(intnat v1)
{
#ifdef ARCH_SIXTYFOUR
  /* -1 because size of int is 63 not 64 (31 not 32, resp.) */
  return int64_clz_check_for_zero_arg(clear_sign_bit(v1))-1;
#else
  return int32_clz_check_for_zero_arg(clear_sign_bit(v1))-1;
#endif
}

/* Takes a tagged input and returns untagged output. */
intnat caml_int_clz_tagged_to_untagged(value v1)
{
  /* Do not use Long_val(v1) conversion, instead preserving the tag.
     It guarantees that the input to builtin_clz is non-zero, to guard
     against versions of builtin_clz that are undefined for input 0.
     The tag does not change the number of leading zeros.
   */
#ifdef ARCH_SIXTYFOUR
  return int64_clz((uint64_t)v1);
#else
  return int32_clz((uint32_t)v1);
#endif
}

CAMLprim value caml_int_clz(value v1)
{
  return Val_long(caml_int_clz_tagged_to_untagged(v1));
}

/* Takes an untagged input and returns untagged output. */
intnat caml_int_ctz_untagged_to_untagged(intnat v1)
{
  /* 1 at the most-significant bit: it does not change the result,
     because size of OCaml [int] is 63 not 64 (31 not 32, resp.),
     and guarantees that the input to ctz is not zero. */
#ifdef ARCH_SIXTYFOUR
  return int64_ctz( ((uint64_t)v1) | (1ull << 63));
#else
  return int32_ctz(((uint32_t)v1) | (1ul << 31));
#endif
}

CAMLprim value caml_int_ctz(value v1)
{
  return Val_long(caml_int_ctz_untagged_to_untagged(Long_val(v1)));
}

/* Takes untagged int and returns untagged int. */
intnat caml_int_popcnt_untagged_to_untagged(intnat v1)
{
  /* Untagging brought in one more '1' for negative numbers.
     Clear the shifted sign bit.
     This implementation is expected to be faster than [popcnt(x) - 1]
     where x is tag(v1). */
#ifdef ARCH_SIXTYFOUR
  return int64_popcnt(clear_sign_bit(v1));
#else
  return int32_popcnt(clear_sign_bit(v1));
#endif
}

/* Takes tagged int and returns untagged int. */
intnat caml_int_popcnt_tagged_to_untagged(value v1)
{
   /* Need -1 to account for the tag. */
#ifdef ARCH_SIXTYFOUR
  return int64_popcnt((uint64_t)v1) - 1;
#else
  return int32_popcnt((uint32_t)v1) - 1;
#endif
}

CAMLprim value caml_int_popcnt(value v1)
{
  return Val_long(caml_int_popcnt_tagged_to_untagged(v1));
}


intnat caml_int32_clz_unboxed_to_untagged(int32_t v)
{
  return int32_clz_check_for_zero_arg((uint32_t) v);
}

intnat caml_int32_ctz_unboxed_to_untagged(int32_t v)
{
  return int32_ctz_check_for_zero_arg((uint32_t) v);
}

intnat caml_int32_clz_nonzero_unboxed_to_untagged(int32_t v)
{
  return int32_clz((uint32_t) v);
}

intnat caml_int32_ctz_nonzero_unboxed_to_untagged(int32_t v)
{
  return int32_ctz((uint32_t) v);
}

intnat caml_int32_popcnt_unboxed_to_untagged(int32_t v)
{
  return int32_popcnt((uint32_t) v);
}

CAMLprim value caml_int32_clz(value v1)
{
  return Val_long(caml_int32_clz_unboxed_to_untagged(Int32_val(v1)));
}

CAMLprim value caml_int32_ctz(value v1)
{
  return Val_long(caml_int32_ctz_unboxed_to_untagged(Int32_val(v1)));
}

CAMLprim value caml_int32_popcnt(value v1)
{
  return Val_long(caml_int32_popcnt_unboxed_to_untagged(Int32_val(v1)));
}

intnat caml_int64_clz_unboxed_to_untagged(int64_t v)
{
  return int64_clz_check_for_zero_arg((uint64_t) v);
}

intnat caml_int64_ctz_unboxed_to_untagged(int64_t v)
{
  return int64_ctz_check_for_zero_arg((uint64_t) v);
}

intnat caml_int64_clz_nonzero_unboxed_to_untagged(int64_t v)
{
  return int64_clz((uint64_t) v);
}

intnat caml_int64_ctz_nonzero_unboxed_to_untagged(int64_t v)
{
  return int64_ctz((uint64_t) v);
}

intnat caml_int64_popcnt_unboxed_to_untagged(int64_t v)
{
  return int64_popcnt((uint64_t) v);
}

CAMLprim value caml_int64_clz(value v1)
{
  return Val_long(caml_int64_clz_unboxed_to_untagged(Int64_val(v1)));
}

CAMLprim value caml_int64_ctz(value v1)
{
  return Val_long(caml_int64_ctz_unboxed_to_untagged(Int64_val(v1)));
}

CAMLprim value caml_int64_popcnt(value v1)
{
  return Val_long(int64_popcnt(Int64_val(v1)));
}

int caml_nativeint_clz_unboxed_to_untagged(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return int64_clz_check_for_zero_arg((uint64_t) v);
#else
  return int32_clz_check_for_zero_arg((uint32_t) v);
#endif
}

intnat caml_nativeint_ctz_unboxed_to_untagged(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return int64_ctz_check_for_zero_arg((uint64_t) v);
#else
  return int32_ctz_check_for_zero_arg((uint32_t) v);
#endif
}

int caml_nativeint_clz_nonzero_unboxed_to_untagged(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return int64_clz((uint64_t) v);
#else
  return int32_clz((uint32_t) v);
#endif
}

intnat caml_nativeint_ctz_nonzero_unboxed_to_untagged(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return int64_ctz((uint64_t) v);
#else
  return int32_ctz((uint32_t) v);
#endif
}

intnat caml_nativeint_popcnt_unboxed_to_untagged(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return int64_popcnt((uint64_t) v);
#else
  return int32_popcnt((uint32_t) v);
#endif
}

CAMLprim value caml_nativeint_clz(value v1)
{
  return Val_long(caml_nativeint_clz_unboxed_to_untagged(Int64_val(v1)));
}

CAMLprim value caml_nativeint_ctz(value v1)
{
  return Val_long(caml_nativeint_ctz_unboxed_to_untagged(Int64_val(v1)));
}

CAMLprim value caml_nativeint_popcnt(value v1)
{
  return Val_long(caml_nativeint_popcnt_unboxed_to_untagged(Int64_val(v1)));
}
