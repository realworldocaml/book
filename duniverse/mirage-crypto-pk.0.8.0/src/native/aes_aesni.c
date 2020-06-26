/* Copyright (c) 2015 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. */

/* AES using AES-NI and SSE2.
 *
 * References:
 * - Intel Advanced Encryption Standard (AES) New Instructions Set. Shay Gueron.
 *   https://software.intel.com/sites/default/files/article/165683/aes-wp-2012-09-22-v01.pdf
 */

#include "mirage_crypto.h"
#if defined (__mc_ACCELERATE__)

/* xmm: [3, 2, 1, 0] */
#define _S_3333 0xff
#define _S_2222 0xaa
#define _S_1111 0x55
#define _S_0000 0x00

/*
 * RKs are currently aligned from the C side on access. Would be better to
 * allocate and pass them in pre-aligned.
 *
 * XXX Get rid of the correction here.
 */
static int _mc_aesni_rk_size (uint8_t rounds) {
  return (rounds + 1) * 16 + 15;
}

#if defined(__x86_64__)
static inline __m128i* __rk (const void *rk) {
  return (__m128i *) (((uint64_t)rk + 15) & -16);
}
#else
static inline __m128i* __rk (const void *rk) {
  return (__m128i *) (((uint32_t)rk + 15) & -16);
}
#endif

static inline __m128i __mix (__m128i r1, __m128i r2) {
  __m128i r = r1;
  r = _mm_xor_si128 (r, _mm_slli_si128 (r1, 0x4));
  r = _mm_xor_si128 (r, _mm_slli_si128 (r1, 0x8));
  r = _mm_xor_si128 (r, _mm_slli_si128 (r1, 0xc));
  r = _mm_xor_si128 (r, r2);
  return r;
}

#define __assist(r1, r2, mode) (__mix (r1, _mm_shuffle_epi32 (r2, mode)))

static inline void __pack (__m128i *o1, __m128i *o2, __m128i r1, __m128i r2, __m128i r3) {
  *o1 = (__m128i) _mm_shuffle_pd ((__m128d) r1, (__m128d) r2, 0);
  *o2 = (__m128i) _mm_shuffle_pd ((__m128d) r2, (__m128d) r3, 1);
}

static inline void _mc_aesni_derive_e_key (const uint8_t *key, uint8_t *rk0, uint8_t rounds) {

  __m128i *rk = __rk (rk0);
  __m128i temp1, temp2;

  switch (rounds) {
    case 10:

      rk[0]  = _mm_loadu_si128 ((__m128i*) key);
      rk[1]  = __assist (rk[0], _mm_aeskeygenassist_si128 (rk[0], 0x01), _S_3333);
      rk[2]  = __assist (rk[1], _mm_aeskeygenassist_si128 (rk[1], 0x02), _S_3333);
      rk[3]  = __assist (rk[2], _mm_aeskeygenassist_si128 (rk[2], 0x04), _S_3333);
      rk[4]  = __assist (rk[3], _mm_aeskeygenassist_si128 (rk[3], 0x08), _S_3333);
      rk[5]  = __assist (rk[4], _mm_aeskeygenassist_si128 (rk[4], 0x10), _S_3333);
      rk[6]  = __assist (rk[5], _mm_aeskeygenassist_si128 (rk[5], 0x20), _S_3333);
      rk[7]  = __assist (rk[6], _mm_aeskeygenassist_si128 (rk[6], 0x40), _S_3333);
      rk[8]  = __assist (rk[7], _mm_aeskeygenassist_si128 (rk[7], 0x80), _S_3333);
      rk[9]  = __assist (rk[8], _mm_aeskeygenassist_si128 (rk[8], 0x1b), _S_3333);
      rk[10] = __assist (rk[9], _mm_aeskeygenassist_si128 (rk[9], 0x36), _S_3333);
      break;

    case 12:
      /* XXX
       * Simplify this horror. */

      rk[0] = _mm_loadu_si128 ((__m128i*) key);
      rk[1] = _mm_loadu_si128 ((__m128i*) (key+8));
      rk[1] = _mm_shuffle_epi32 (rk[1], 0xee); /* XXX shift */

      temp1 = __assist (rk[0], _mm_aeskeygenassist_si128 (rk[1], 0x01), _S_1111);
      temp2 = __assist (rk[1], temp1, _S_3333);

      __pack (&rk[1], &rk[2], rk[1], temp1, temp2);

      rk[3] = __assist (temp1, _mm_aeskeygenassist_si128 (temp2, 0x02), _S_1111);
      rk[4] = __assist (temp2, rk[3], _S_3333);

      temp1 = __assist (rk[3], _mm_aeskeygenassist_si128 (rk[4], 0x04), _S_1111);
      temp2 = __assist (rk[4], temp1, _S_3333);

      __pack (&rk[4], &rk[5], rk[4], temp1, temp2);

      rk[6] = __assist (temp1, _mm_aeskeygenassist_si128 (temp2, 0x08), _S_1111);
      rk[7] = __assist (temp2, rk[6], _S_3333);

      temp1 = __assist (rk[6], _mm_aeskeygenassist_si128 (rk[7], 0x10), _S_1111);
      temp2 = __assist (rk[7], temp1, _S_3333);

      __pack (&rk[7], &rk[8], rk[7], temp1, temp2);

      rk[9] = __assist (temp1, _mm_aeskeygenassist_si128 (temp2, 0x20), _S_1111);
      rk[10] = __assist (temp2, rk[9], _S_3333);

      temp1 = __assist (rk[9], _mm_aeskeygenassist_si128 (rk[10], 0x40), _S_1111);
      temp2 = __assist (rk[10], temp1, _S_3333);

      __pack (&rk[10], &rk[11], rk[10], temp1, temp2);

      rk[12] = __assist (temp1, _mm_aeskeygenassist_si128 (temp2, 0x80), _S_1111);
      break;

    case 14:

      rk[0]  = _mm_loadu_si128((__m128i*) key);
      rk[1]  = _mm_loadu_si128((__m128i*) (key+16));
      rk[2]  = __assist (rk[0],  _mm_aeskeygenassist_si128 (rk[1],  0x01), _S_3333);
      rk[3]  = __assist (rk[1],  _mm_aeskeygenassist_si128 (rk[2],  0x00), _S_2222);
      rk[4]  = __assist (rk[2],  _mm_aeskeygenassist_si128 (rk[3],  0x02), _S_3333);
      rk[5]  = __assist (rk[3],  _mm_aeskeygenassist_si128 (rk[4],  0x00), _S_2222);
      rk[6]  = __assist (rk[4],  _mm_aeskeygenassist_si128 (rk[5],  0x04), _S_3333);
      rk[7]  = __assist (rk[5],  _mm_aeskeygenassist_si128 (rk[6],  0x00), _S_2222);
      rk[8]  = __assist (rk[6],  _mm_aeskeygenassist_si128 (rk[7],  0x08), _S_3333);
      rk[9]  = __assist (rk[7],  _mm_aeskeygenassist_si128 (rk[8],  0x00), _S_2222);
      rk[10] = __assist (rk[8],  _mm_aeskeygenassist_si128 (rk[9],  0x10), _S_3333);
      rk[11] = __assist (rk[9],  _mm_aeskeygenassist_si128 (rk[10], 0x00), _S_2222);
      rk[12] = __assist (rk[10], _mm_aeskeygenassist_si128 (rk[11], 0x20), _S_3333);
      rk[13] = __assist (rk[11], _mm_aeskeygenassist_si128 (rk[12], 0x00), _S_2222);
      rk[14] = __assist (rk[12], _mm_aeskeygenassist_si128 (rk[13], 0x40), _S_3333);
      break;

    default:
      ;
  }
}

static inline void _mc_aesni_invert_e_key (const uint8_t *rk0, uint8_t *kr0, uint8_t rounds) {

  __m128i *rk1 = __rk (rk0),
          *kr  = __rk (kr0),
          rk[15];

  for (uint8_t i = 0; i <= rounds; i++)
    rk[i] = rk1[i];

  kr[0] = rk[rounds];

  for (uint8_t i = 1; i < rounds; i++)
    kr[i] = _mm_aesimc_si128 (rk[rounds - i]);

  kr[rounds] = rk[0];
}

static void _mc_aesni_derive_d_key (const uint8_t *key, uint8_t *kr, uint8_t rounds, uint8_t *rk) {
  if (!rk) {
    _mc_aesni_derive_e_key (key, kr, rounds);
    rk = kr;
  }
  _mc_aesni_invert_e_key (rk, kr, rounds);
}


static inline void _mc_aesni_enc (const uint8_t src[16], uint8_t dst[16], const uint8_t *rk0, uint8_t rounds) {

  __m128i r   = _mm_loadu_si128 ((__m128i*) src),
          *rk = __rk (rk0);

  r = _mm_xor_si128 (r, rk[0]);

  for (uint8_t i = 1; i < rounds; i++)
    r = _mm_aesenc_si128 (r, rk[i]);

  r = _mm_aesenclast_si128 (r, rk[rounds]);
  _mm_storeu_si128 ((__m128i*) dst, r);
}

static inline void _mc_aesni_dec (const uint8_t src[16], uint8_t dst[16], const uint8_t *rk0, uint8_t rounds) {

  __m128i r   = _mm_loadu_si128 ((__m128i*) src),
          *rk = __rk (rk0);

  r = _mm_xor_si128 (r, rk[0]);

  for (uint8_t i = 1; i < rounds; i++)
    r = _mm_aesdec_si128 (r, rk[i]);

  r = _mm_aesdeclast_si128 (r, rk[rounds]);
  _mm_storeu_si128 ((__m128i*) dst, r);
}

static inline void _mc_aesni_enc8 (const uint8_t src[128], uint8_t dst[128], const uint8_t *rk0, uint8_t rounds) {

  __m128i *in  = (__m128i*) src,
          *out = (__m128i*) dst,
          *rk  = __rk (rk0);

  __m128i r0 = _mm_loadu_si128 (in    ),
          r1 = _mm_loadu_si128 (in + 1),
          r2 = _mm_loadu_si128 (in + 2),
          r3 = _mm_loadu_si128 (in + 3),
          r4 = _mm_loadu_si128 (in + 4),
          r5 = _mm_loadu_si128 (in + 5),
          r6 = _mm_loadu_si128 (in + 6),
          r7 = _mm_loadu_si128 (in + 7);

  r0 = _mm_xor_si128 (r0, rk[0]);
  r1 = _mm_xor_si128 (r1, rk[0]);
  r2 = _mm_xor_si128 (r2, rk[0]);
  r3 = _mm_xor_si128 (r3, rk[0]);
  r4 = _mm_xor_si128 (r4, rk[0]);
  r5 = _mm_xor_si128 (r5, rk[0]);
  r6 = _mm_xor_si128 (r6, rk[0]);
  r7 = _mm_xor_si128 (r7, rk[0]);

  for (uint8_t i = 1; i < rounds; i++) {
    r0 = _mm_aesenc_si128 (r0, rk[i]);
    r1 = _mm_aesenc_si128 (r1, rk[i]);
    r2 = _mm_aesenc_si128 (r2, rk[i]);
    r3 = _mm_aesenc_si128 (r3, rk[i]);
    r4 = _mm_aesenc_si128 (r4, rk[i]);
    r5 = _mm_aesenc_si128 (r5, rk[i]);
    r6 = _mm_aesenc_si128 (r6, rk[i]);
    r7 = _mm_aesenc_si128 (r7, rk[i]);
  }

  r0 = _mm_aesenclast_si128 (r0, rk[rounds]);
  r1 = _mm_aesenclast_si128 (r1, rk[rounds]);
  r2 = _mm_aesenclast_si128 (r2, rk[rounds]);
  r3 = _mm_aesenclast_si128 (r3, rk[rounds]);
  r4 = _mm_aesenclast_si128 (r4, rk[rounds]);
  r5 = _mm_aesenclast_si128 (r5, rk[rounds]);
  r6 = _mm_aesenclast_si128 (r6, rk[rounds]);
  r7 = _mm_aesenclast_si128 (r7, rk[rounds]);

  _mm_storeu_si128 (out    , r0);
  _mm_storeu_si128 (out + 1, r1);
  _mm_storeu_si128 (out + 2, r2);
  _mm_storeu_si128 (out + 3, r3);
  _mm_storeu_si128 (out + 4, r4);
  _mm_storeu_si128 (out + 5, r5);
  _mm_storeu_si128 (out + 6, r6);
  _mm_storeu_si128 (out + 7, r7);
}

static inline void _mc_aesni_dec8 (const uint8_t src[128], uint8_t dst[128], const uint8_t *rk0, uint8_t rounds) {

  __m128i *in  = (__m128i*) src,
          *out = (__m128i*) dst,
          *rk  = __rk (rk0);

  __m128i r0 = _mm_loadu_si128 (in    ),
          r1 = _mm_loadu_si128 (in + 1),
          r2 = _mm_loadu_si128 (in + 2),
          r3 = _mm_loadu_si128 (in + 3),
          r4 = _mm_loadu_si128 (in + 4),
          r5 = _mm_loadu_si128 (in + 5),
          r6 = _mm_loadu_si128 (in + 6),
          r7 = _mm_loadu_si128 (in + 7);

  r0 = _mm_xor_si128 (r0, rk[0]);
  r1 = _mm_xor_si128 (r1, rk[0]);
  r2 = _mm_xor_si128 (r2, rk[0]);
  r3 = _mm_xor_si128 (r3, rk[0]);
  r4 = _mm_xor_si128 (r4, rk[0]);
  r5 = _mm_xor_si128 (r5, rk[0]);
  r6 = _mm_xor_si128 (r6, rk[0]);
  r7 = _mm_xor_si128 (r7, rk[0]);

  for (uint8_t i = 1; i < rounds; i++) {
    r0 = _mm_aesdec_si128 (r0, rk[i]);
    r1 = _mm_aesdec_si128 (r1, rk[i]);
    r2 = _mm_aesdec_si128 (r2, rk[i]);
    r3 = _mm_aesdec_si128 (r3, rk[i]);
    r4 = _mm_aesdec_si128 (r4, rk[i]);
    r5 = _mm_aesdec_si128 (r5, rk[i]);
    r6 = _mm_aesdec_si128 (r6, rk[i]);
    r7 = _mm_aesdec_si128 (r7, rk[i]);
  }

  r0 = _mm_aesdeclast_si128 (r0, rk[rounds]);
  r1 = _mm_aesdeclast_si128 (r1, rk[rounds]);
  r2 = _mm_aesdeclast_si128 (r2, rk[rounds]);
  r3 = _mm_aesdeclast_si128 (r3, rk[rounds]);
  r4 = _mm_aesdeclast_si128 (r4, rk[rounds]);
  r5 = _mm_aesdeclast_si128 (r5, rk[rounds]);
  r6 = _mm_aesdeclast_si128 (r6, rk[rounds]);
  r7 = _mm_aesdeclast_si128 (r7, rk[rounds]);

  _mm_storeu_si128 (out    , r0);
  _mm_storeu_si128 (out + 1, r1);
  _mm_storeu_si128 (out + 2, r2);
  _mm_storeu_si128 (out + 3, r3);
  _mm_storeu_si128 (out + 4, r4);
  _mm_storeu_si128 (out + 5, r5);
  _mm_storeu_si128 (out + 6, r6);
  _mm_storeu_si128 (out + 7, r7);
}

#define __b(ptr, n) (ptr + n * 16)

#define __blocked_loop(f1, f8, src, dst, rk, rounds, blocks) \
  while (blocks) {                                           \
    switch (blocks) {                                        \
      case 7:                                                \
        f1 (__b (src, 6), __b (dst, 6), rk, rounds);         \
        /* fall through */                                   \
      case 6:                                                \
        f1 (__b (src, 5), __b (dst, 5), rk, rounds);         \
        /* fall through */                                   \
      case 5:                                                \
        f1 (__b (src, 4), __b (dst, 4), rk, rounds);         \
        /* fall through */                                   \
      case 4:                                                \
        f1 (__b (src, 3), __b (dst, 3), rk, rounds);         \
        /* fall through */                                   \
      case 3:                                                \
        f1 (__b (src, 2), __b (dst, 2), rk, rounds);         \
        /* fall through */                                   \
      case 2:                                                \
        f1 (__b (src, 1), __b (dst, 1), rk, rounds);         \
        /* fall through */                                   \
      case 1:                                                \
        f1 (__b (src, 0), __b (dst, 0), rk, rounds);         \
        /* fall through */                                   \
      case 0:                                                \
        return;                                              \
      default:                                               \
        f8 (src, dst, rk, rounds);                           \
        src += 128; dst += 128; blocks -= 8;                 \
    }                                                        \
  }                                                          \

static inline void _mc_aesni_enc_blocks (const uint8_t *src, uint8_t *dst, const uint8_t *rk, uint8_t rounds, size_t blocks) {
  __blocked_loop (_mc_aesni_enc, _mc_aesni_enc8, src, dst, rk, rounds, blocks);
}

static inline void _mc_aesni_dec_blocks (const uint8_t *src, uint8_t *dst, const uint8_t *rk, uint8_t rounds, size_t blocks) {
  __blocked_loop (_mc_aesni_dec, _mc_aesni_dec8, src, dst, rk, rounds, blocks);
}

#endif /* __mc_ACCELERATE__ */

CAMLprim value
mc_aes_rk_size (value rounds) {
  value s;
  _mc_switch_accel(aesni,
    s = mc_aes_rk_size_generic(rounds),
    s = Val_int (_mc_aesni_rk_size (Int_val (rounds))))
  return s;
}

CAMLprim value
mc_aes_derive_e_key (value key, value off1, value rk, value rounds) {
  _mc_switch_accel(aesni,
    mc_aes_derive_e_key_generic(key, off1, rk, rounds),
    _mc_aesni_derive_e_key (_ba_uint8_off (key, off1),
                            _ba_uint8 (rk),
                            Int_val (rounds)))
  return Val_unit;
}

CAMLprim value
mc_aes_derive_d_key (value key, value off1, value kr, value rounds, value rk) {
  _mc_switch_accel(aesni,
    mc_aes_derive_d_key_generic(key, off1, kr, rounds, rk),
    _mc_aesni_derive_d_key (_ba_uint8_off (key, off1),
                            _ba_uint8 (kr),
                            Int_val (rounds),
                            Is_block(rk) ? _ba_uint8(Field(rk, 0)) : 0))
  return Val_unit;
}

CAMLprim value
mc_aes_enc (value src, value off1, value dst, value off2, value rk, value rounds, value blocks) {
  _mc_switch_accel(aesni,
    mc_aes_enc_generic(src, off1, dst, off2, rk, rounds, blocks),
    _mc_aesni_enc_blocks ( _ba_uint8_off (src, off1),
                           _ba_uint8_off (dst, off2),
                           _ba_uint8 (rk),
                           Int_val (rounds),
                           Int_val (blocks) ))
  return Val_unit;
}

CAMLprim value
mc_aes_dec (value src, value off1, value dst, value off2, value rk, value rounds, value blocks) {
  _mc_switch_accel(aesni,
    mc_aes_dec_generic(src, off1, dst, off2, rk, rounds, blocks),
    _mc_aesni_dec_blocks ( _ba_uint8_off (src, off1),
                           _ba_uint8_off (dst, off2),
                           _ba_uint8 (rk),
                           Int_val (rounds),
                           Int_val (blocks) ))
  return Val_unit;
}

CAMLprim value mc_aes_mode (__unit ()) {
  value enabled = 0;
  _mc_switch_accel(aesni,
    enabled = 0,
    enabled = 1)
  return Val_int (enabled);
}

__define_bc_7 (mc_aes_enc)
__define_bc_7 (mc_aes_dec)
