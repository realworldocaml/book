/* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. */

#include "mirage_crypto.h"
#include <string.h>

/* Generic table-driven GHASH.
 *
 * References:
 * - The Galois/Counter Mode of Operation. David A. McGrew and John Viega.
 * - NIST SP 800-38D. Recommendation for Block Cipher Modes of Operation:
 *   Galois/Counter Mode (GCM) and GMAC.
 */

/*  LARGE_TABLES -> 65K per key
 * !LARGE_TABLES -> 8K per key, ~3x slower. */
#define __MC_GHASH_LARGE_TABLES

#if defined (__x86_64__) || defined (__aarch64__)

#define __set_uint128_t(w1, w0) (((__uint128_t) w1 << 64) | w0)

static const __uint128_t r = __set_uint128_t (0xe100000000000000, 0);

static inline __uint128_t __load_128_t (const uint64_t s[2]) {
  return __set_uint128_t (be64_to_cpu (s[0]), be64_to_cpu (s[1]));
}

static inline __uint128_t __load_128_t_with_padding (const uint8_t *src, size_t n) {
  uint64_t buf[2] = { 0 };
  memcpy (buf, src, n);
  return __load_128_t (buf);
}

static inline void __store_128_t (uint64_t s[2], __uint128_t x) {
  s[0] = cpu_to_be64 (x >> 64);
  s[1] = cpu_to_be64 (x);
}

#if defined (__MC_GHASH_LARGE_TABLES)
#define __t_width  8     // coefficient window
#define __t_tables 16    // 128 / t_width
#define __t_size   4096  // 2^t_width * t_tables
#else
#define __t_width  4
#define __t_tables 32
#define __t_size   512
#endif

static inline __uint128_t __gfmul (__uint128_t a, __uint128_t b) {
  __uint128_t z = 0,
              v = a;
  for (int i = 0; i < 128; i ++) {
    if ((uint64_t) (b >> (127 - i)) & 1)
      z = z ^ v;
    v = (uint64_t) v & 1 ? (v >> 1) ^ r : v >> 1;
  }
  return z;
}

// NB Exponents are reversed.
// TODO: Fast table derivation.
static inline void __derive (uint64_t key[2], __uint128_t m[__t_size]) {
  __uint128_t e = 1 << (__t_width - 1),
              h = __load_128_t (key);
  for (int i = 0; i < __t_tables; i ++, e <<= __t_width) {
    __uint128_t exph = __gfmul (h, e);
    for (int j = 0; j < (1 << __t_width); j ++)
      m[(i << __t_width) | j] = __gfmul (exph, (__uint128_t) j << (128 - __t_width));
  }
}

#define __t_mask ((1 << __t_width) - 1)
static inline __uint128_t __gfmul_tab (__uint128_t m[__t_size], __uint128_t x) {
  __uint128_t r = 0;
  for (int i = 0; i < __t_tables; i ++)
    r ^= m[(i << __t_width) | ((uint8_t) (x >> (i * __t_width)) & __t_mask)];
  return r;
}

static inline void __ghash (__uint128_t m[__t_size], uint64_t hash[2], const uint8_t *src, size_t n) {
  __uint128_t acc = __load_128_t (hash);
  for (; n >= 16; src += 16, n -= 16)
    acc = __gfmul_tab (m, acc ^ __load_128_t ((uint64_t *) src));
  if (n > 0)
    acc = __gfmul_tab (m, acc ^ __load_128_t_with_padding (src, n));
  __store_128_t (hash, acc);
}

CAMLprim value mc_ghash_key_size_generic (__unit ()) {
  return Val_int (sizeof (__uint128_t) * __t_size);
}

CAMLprim value mc_ghash_init_key_generic (value key, value off, value m) {
  __derive ((uint64_t *) _ba_uint8_off (key, off), (__uint128_t *) Bp_val (m));
  return Val_unit;
}

CAMLprim value
mc_ghash_generic (value m, value hash, value src, value off, value len) {
  __ghash ((__uint128_t *) Bp_val (m), (uint64_t *) Bp_val (hash),
           _ba_uint8_off (src, off), Int_val (len) );
  return Val_unit;
}

#endif /* x86_64 || aarch64 */
