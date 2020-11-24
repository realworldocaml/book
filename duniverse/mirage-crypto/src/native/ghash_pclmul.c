/* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. */

/* GHASH using PCLMULQDQ and SSE3.
 *
 * References:
 * - Intel Carry-Less Multiplication Instruction and its Usage for Computing the
 *   GCM Mode. Shay Gueron and Michael E. Kounavis.
 *   https://software.intel.com/sites/default/files/managed/72/cc/clmul-wp-rev-2.02-2014-04-20.pdf
 * - AES-GCM for Efficient Authenticated Encryption - Ending the Reign of HMAC-SHA-1?
 *   Shay Gureon. https://crypto.stanford.edu/RealWorldCrypto/slides/gueron.pdf
 * - Optimized Galois-Counter-Mode Implementation on Intel. Vinodh Gopal et al.
 *   http://www.intel.com/content/dam/www/public/us/en/documents/white-papers/communications-ia-galois-counter-mode-paper.pdf
 * - https://blog.quarkslab.com/reversing-a-finite-field-multiplication-optimization.html
 * - https://github.com/openssl/openssl/blob/master/crypto/modes/asm/aesni-gcm-x86_64.pl
 */


/* #define __MC_GHASH_KARATSUBA */
#define __MC_GHASH_REFLECTED_REDUCE
#define __MC_GHASH_AGGREGATED_REDUCE

#include "mirage_crypto.h"
#ifdef __mc_ACCELERATE__

#include <string.h>

#define xor(a, b) _mm_xor_si128 (a, b)
#define xor4(a, b, c, d) xor (xor (a, b), xor (c, d))

#define __reduction_poly (_mm_set_epi64x (0, 0xc200000000000000))

static inline __m128i __shiftl1_si128 (__m128i w) {
  return _mm_or_si128 (_mm_slli_epi64 (w, 1), _mm_slli_si128(_mm_srli_epi64 (w, 63), 8));
}

static inline __m128i __carry_si128 (__m128i w) {
  return _mm_cmpgt_epi32 (_mm_setzero_si128 (), _mm_shuffle_epi32 (w, 0xff));
}

static inline void __shiftl1_256 (__m128i *r1, __m128i *r0, __m128i w1, __m128i w0) {
  __m128i c = _mm_and_si128 (__carry_si128 (w0), _mm_set_epi64x (0, 1));
  *r1 = _mm_or_si128 (__shiftl1_si128 (w1), c);
  *r0 = __shiftl1_si128 (w0);
}

static inline __m128i __swap_epi64 (__m128i x) {
  return _mm_shuffle_epi32 (x, 0x4e); // ...or vpalignr
}

static inline __m128i __reverse_si128 (__m128i x) {
  __m128i mask = _mm_set_epi64x (0x0001020304050607, 0x08090a0b0c0d0e0f);
  return _mm_shuffle_epi8 (x, mask);
}

#if !defined (__MC_GHASH_KARATSUBA)
static inline void __clmul_128 (__m128i *r1, __m128i *r0, __m128i a, __m128i b) {

  __m128i w0 = _mm_clmulepi64_si128 (a, b, 0x00),
          w1 = _mm_clmulepi64_si128 (a, b, 0x11),
          t  = xor (_mm_clmulepi64_si128 (a, b, 0x01),
                    _mm_clmulepi64_si128 (a, b, 0x10));

  *r0 = xor (w0, _mm_slli_si128 (t, 8));
  *r1 = xor (w1, _mm_srli_si128 (t, 8));
}
#else
static inline void __clmul_128 (__m128i *r1, __m128i *r0, __m128i a, __m128i b) {

  __m128i w0 = _mm_clmulepi64_si128 (a, b, 0x00),
          w1 = _mm_clmulepi64_si128 (a, b, 0x11),
          t  = _mm_clmulepi64_si128 (xor (a, __swap_epi64 (a)),
                                     xor (b, __swap_epi64 (b)), 0x11);

  t   = xor (t, xor (w0, w1));
  *r0 = xor (w0, _mm_slli_si128 (t, 8));
  *r1 = xor (w1, _mm_srli_si128 (t, 8));
}
#endif /* __MC_GHASH_KARATSUBA */

#if !defined (__MC_GHASH_REFLECTED_REDUCE)
static inline __m128i __slli_128 (__m128i a, uint8_t bits) {
  return _mm_or_si128 (
    _mm_slli_epi64 (a, bits),
    _mm_srli_epi64 (_mm_slli_si128 (a, 8), 64 - bits) );
}

static inline __m128i __reflect (__m128i x) {
  __m128i and_mask = _mm_set_epi32 (0x0f0f0f0f, 0x0f0f0f0f, 0x0f0f0f0f, 0x0f0f0f0f),
          lo_mask  = _mm_set_epi32 (0x0f070b03, 0x0d050901, 0x0e060a02, 0x0c040800),
          hi_mask  = _mm_set_epi32 (0xf070b030, 0xd0509010, 0xe060a020, 0xc0408000);

  return xor (
    _mm_shuffle_epi8 (hi_mask, _mm_and_si128 (x, and_mask)),
    _mm_shuffle_epi8 (lo_mask, _mm_and_si128 (_mm_srli_epi16 (x, 4), and_mask)));
}

static inline __m128i __reduce_g (__m128i w1, __m128i w0) {

  __m128i t = _mm_srli_si128 (w1, 8);

  t = xor4 (w1, _mm_srli_epi64 (t, 63), _mm_srli_epi64 (t, 62), _mm_srli_epi64 (t, 57));

  return xor (w0, xor4 (t, __slli_128 (t, 1), __slli_128 (t, 2), __slli_128 (t, 7)));
}
#define __repr_xform __reflect
#else
static inline __m128i __reduce_g (__m128i w1, __m128i w0) {

  __m128i p = __reduction_poly;

  // XXX Get rid of this (Gopal et al)? __derive becomes fiddly.
  __shiftl1_256 (&w1, &w0, w1, w0);

  w0 = xor (__swap_epi64 (w0), _mm_clmulepi64_si128 (w0, p, 0x00));
  w0 = xor (__swap_epi64 (w0), _mm_clmulepi64_si128 (w0, p, 0x00));

  return xor (w1, w0);
}
#define __repr_xform __reverse_si128
#endif

static inline __m128i __load_xform (const __m128i *p) {
  return __repr_xform (_mm_loadu_si128 (p));
}

static inline __m128i __loadu_si128_with_padding (const void *src, size_t n) {
  __m128i buf[1] = { 0 };
  memcpy (buf, src, n);
  return __repr_xform (_mm_loadu_si128 (buf));
}

static inline __m128i __gfmul (__m128i a, __m128i b) {
  __m128i w1, w0;
  __clmul_128 (&w1, &w0, a, b);
  return __reduce_g (w1, w0);
}

#if defined (__MC_GHASH_AGGREGATED_REDUCE)
#define __keys 8
#else
#define __keys 1
#endif

static inline void __derive (__m128i key[1], __m128i *m) {
  __m128i acc = __repr_xform (_mm_set_epi64x (0, 0x80)),
          k   = __load_xform (key);
  for (int i = 0; i < __keys; i ++)
    _mm_storeu_si128 (m + i, acc = __gfmul (acc, k));
}

static inline void __ghash (__m128i *m, __m128i hash[1], const __m128i *src, size_t n) {

  __m128i k[__keys], acc = __load_xform (hash);
  k[0] = _mm_loadu_si128 (m);

#if defined (__MC_GHASH_AGGREGATED_REDUCE)
  if (n >= 128) {
    __m128i a1, a0, b1, b0, c1, c0, d1, d0, e1, e0, f1, f0, g1, g0, h1, h0;
    k[1] = _mm_loadu_si128 (m + 1),
    k[2] = _mm_loadu_si128 (m + 2),
    k[3] = _mm_loadu_si128 (m + 3),
    k[4] = _mm_loadu_si128 (m + 4),
    k[5] = _mm_loadu_si128 (m + 5),
    k[6] = _mm_loadu_si128 (m + 6),
    k[7] = _mm_loadu_si128 (m + 7);
    for (; n >= 128; src += 8, n -= 128) {
      __clmul_128 (&a1, &a0, k[7], xor (acc, __load_xform (src)));
      __clmul_128 (&b1, &b0, k[6], __load_xform (src + 1));
      __clmul_128 (&c1, &c0, k[5], __load_xform (src + 2));
      __clmul_128 (&d1, &d0, k[4], __load_xform (src + 3));
      __clmul_128 (&e1, &e0, k[3], __load_xform (src + 4));
      __clmul_128 (&f1, &f0, k[2], __load_xform (src + 5));
      __clmul_128 (&g1, &g0, k[1], __load_xform (src + 6));
      __clmul_128 (&h1, &h0, k[0], __load_xform (src + 7));
      acc = __reduce_g (xor(xor4 (a1, b1, c1, d1), xor4 (e1, f1, g1, h1)),
                        xor(xor4 (a0, b0, c0, d0), xor4 (e0, f0, g0, h0)));
    }
  }
#endif

  for (; n >= 16; n -= 16)
    acc = __gfmul (k[0], xor (acc, __load_xform (src ++)));
  if (n > 0)
    acc = __gfmul (k[0], xor (acc, __loadu_si128_with_padding (src, n)));
  _mm_storeu_si128 (hash, __repr_xform (acc));
}

#endif /* __mc_ACCELERATE__ */

CAMLprim value mc_ghash_key_size (__unit ()) {
  value s;
  _mc_switch_accel(pclmul,
    s = mc_ghash_key_size_generic(Val_unit),
    s = Val_int (__keys * 16))
  return s;
}

CAMLprim value mc_ghash_init_key (value key, value off, value m) {
  _mc_switch_accel(pclmul,
    mc_ghash_init_key_generic(key, off, m),
    __derive ((__m128i *) _ba_uint8_off (key, off), (__m128i *) Bp_val (m)))
  return Val_unit;
}

CAMLprim value
mc_ghash (value k, value hash, value src, value off, value len) {
  _mc_switch_accel(pclmul,
    mc_ghash_generic(k, hash, src, off, len),
    __ghash ( (__m128i *) Bp_val (k), (__m128i *) Bp_val (hash),
      (__m128i *) _ba_uint8_off (src, off), Int_val (len) ))
  return Val_unit;
}

CAMLprim value mc_ghash_mode (__unit ()) {
  value enabled = 0;
  _mc_switch_accel(pclmul,
    enabled = 0,
    enabled = 1)
  return Val_int (enabled);
}
