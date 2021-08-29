#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#ifdef _MSC_VER

#include <intrin.h>

#define __builtin_popcountll __popcnt64
#define __builtin_popcount   __popcnt

static uint32_t __inline __builtin_clz(uint32_t x)
{
  int r = 0;
  _BitScanForward(&r, x);
  return r;
}

static uint64_t __inline __builtin_clzll(uint64_t x)
{
  int r = 0;
  _BitScanForward64(&r, x);
  return r;
}

static uint32_t __inline __builtin_ctz(uint32_t x)
{
  int r = 0;
  _BitScanReverse(&r, x);
  return r;
}

static uint64_t __inline __builtin_ctzll(uint64_t x)
{
  int r = 0;
  _BitScanReverse64(&r, x);
  return r;
}

#endif

static int64_t int_pow(int64_t base, int64_t exponent) {
  int64_t ret = 1;
  int64_t mul[4];
  mul[0] = 1;
  mul[1] = base;
  mul[3] = 1;

  while(exponent != 0) {
    mul[1] *= mul[3];
    mul[2] = mul[1] * mul[1];
    mul[3] = mul[2] * mul[1];
    ret *= mul[exponent & 3];
    exponent >>= 2;
  }

  return ret;
}

CAMLprim value Base_int_math_int_pow_stub(value base, value exponent) {
  return (Val_long(int_pow(Long_val(base), Long_val(exponent))));
}

CAMLprim value Base_int_math_int64_pow_stub(value base, value exponent) {
  CAMLparam2(base, exponent);
  CAMLreturn(caml_copy_int64(int_pow(Int64_val(base), Int64_val(exponent))));
}

/* This implementation is faster than [__builtin_popcount(v) - 1], even though
 * it seems more complicated.  The [&] clears the shifted sign bit after
 * [Long_val] or [Int_val]. */
CAMLprim value Base_int_math_int_popcount(value v) {
#ifdef ARCH_SIXTYFOUR
  return Val_int (__builtin_popcountll (Long_val (v) & ~((uint64_t)1 << 63)));
#else
  return Val_int (__builtin_popcount   (Int_val  (v) & ~((uint32_t)1 << 31)));
#endif
}

/* The specification of all below [clz] and [ctz] functions are undefined for [v = 0]. */

/*
 * For an int [x] in the [2n + 1] representation:
 *
 *   clz(x) = __builtin_clz(x >> 1) - 1
 *
 * If [x] is negative, then the macro [Int_val] would perform a arithmetic
 * shift right, rather than a logical shift right, and sign extend the number.
 * Therefore
 *
 *   __builtin_clz(Int_val(x))
 *
 *  would always be zero, so
 *
 *    clz(x) = __builtin_clz(Int_val(x)) - 1
 *
 *  would always be -1. This is not what we want.
 *
 *  The logical shift right adds a leading zero to the argument of
 *  __builtin_clz, which the -1 accounts for. Rather than adding the leading
 *  zero and subtracting, we can just compute the clz of the tagged
 *  representation, and that should be equivalent, while also handing negative
 *  inputs correctly (the result will now be 0).
 */
intnat Base_int_math_int_clz_untagged(value v) {
#ifdef ARCH_SIXTYFOUR
  return __builtin_clzll (v);
#else
  return __builtin_clz   (v);
#endif
}

CAMLprim value Base_int_math_int_clz(value v) {
  return Val_int (Base_int_math_int_clz_untagged (v));
}

intnat Base_int_math_int32_clz_unboxed(int32_t v) {
  return __builtin_clz (v);
}

CAMLprim value Base_int_math_int32_clz(value v) {
  return Val_int (Base_int_math_int32_clz_unboxed (Int32_val(v)));
}

intnat Base_int_math_int64_clz_unboxed(int64_t v) {
  return __builtin_clzll (v);
}

CAMLprim value Base_int_math_int64_clz(value v) {
  return Val_int (Base_int_math_int64_clz_unboxed (Int64_val(v)));
}

intnat Base_int_math_nativeint_clz_unboxed(intnat v) {
#ifdef ARCH_SIXTYFOUR
  return __builtin_clzll (v);
#else
  return __builtin_clz   (v);
#endif
}

CAMLprim value Base_int_math_nativeint_clz(value v) {
  return Val_int (Base_int_math_nativeint_clz_unboxed (Nativeint_val(v)));
}

intnat Base_int_math_int_ctz_untagged(intnat v) {
#ifdef ARCH_SIXTYFOUR
  return __builtin_ctzll (v);
#else
  return __builtin_ctz   (v);
#endif
}

CAMLprim value Base_int_math_int_ctz(value v) {
  return Val_int (Base_int_math_int_ctz_untagged (Int_val(v)));
}

intnat Base_int_math_int32_ctz_unboxed(int32_t v) {
  return __builtin_ctz (v);
}

CAMLprim value Base_int_math_int32_ctz(value v) {
  return Val_int (Base_int_math_int32_ctz_unboxed (Int32_val(v)));
}

intnat Base_int_math_int64_ctz_unboxed(int64_t v) {
  return __builtin_ctzll (v);
}

CAMLprim value Base_int_math_int64_ctz(value v) {
  return Val_int (Base_int_math_int64_ctz_unboxed (Int64_val(v)));
}

intnat Base_int_math_nativeint_ctz_unboxed(intnat v) {
#ifdef ARCH_SIXTYFOUR
  return __builtin_ctzll (v);
#else
  return __builtin_ctz   (v);
#endif
}

CAMLprim value Base_int_math_nativeint_ctz(value v) {
  return Val_int (Base_int_math_nativeint_ctz_unboxed (Nativeint_val(v)));
}
