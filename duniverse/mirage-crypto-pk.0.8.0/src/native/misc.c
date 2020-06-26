#include "mirage_crypto.h"

static inline void xor_into (uint8_t *src, uint8_t *dst, size_t n) {

  for (; n >= 8; n -= 8, src += 8, dst += 8)
    *(uint64_t*) dst ^= *(uint64_t*) src;

  for (; n --; ++ src, ++ dst) *dst = *src ^ *dst;
}

static inline void _mc_count_8_be (uint64_t *init, uint64_t *dst, size_t blocks) {
  uint64_t qw = be64_to_cpu (*init);
  while (blocks --) *(dst ++) = cpu_to_be64 (qw ++);
}

/* XXX
 *
 * Counters are garbage. ;_;
 * Calling this incurs about a 15% hit in AES-CTR.
 *
 * What slows things down:
 *   - Naive __uint128_t.
 *   - Loop unrolling.
 *   - SSE carry bit handling.
 */
static inline void _mc_count_16_be (uint64_t *init, uint64_t *dst, size_t blocks) {
  uint64_t qw1 = init[0],
           qw2 = be64_to_cpu (init[1]);
  for (; blocks --; dst += 2) {
    dst[0] = qw1;
    dst[1] = cpu_to_be64 (qw2);
    if ((++ qw2) == 0) qw1 = cpu_to_be64 (be64_to_cpu (qw1) + 1);
  }
}

/* The GCM counter. Counts on the last 32 bits, ignoring carry. */
static inline void _mc_count_16_be_4 (uint64_t *init, uint64_t *dst, size_t blocks) {

  uint64_t qw1 = init[0];
  uint32_t dw3 = ((uint32_t*) init)[2],
           dw4 = be32_to_cpu (((uint32_t*) init)[3]);
  for (; blocks --; dst += 2) {
    dst[0] = qw1;
    ((uint32_t*) dst)[2] = dw3;
    ((uint32_t*) dst)[3] = cpu_to_be32 (dw4 ++);
  }
}

CAMLprim value
mc_xor_into_generic (value b1, value off1, value b2, value off2, value n) {
  xor_into (_ba_uint8_off (b1, off1), _ba_uint8_off (b2, off2), Int_val (n));
  return Val_unit;
}

#define __export_counter(name, f)                                        \
  CAMLprim value name (value ctr, value dst, value off, value blocks) {  \
    f ( (uint64_t*) Bp_val (ctr),                                        \
        (uint64_t*) _ba_uint8_off (dst, off), Long_val (blocks) );       \
    return Val_unit;                                                     \
  }

__export_counter (mc_count_8_be, _mc_count_8_be)
__export_counter (mc_count_16_be, _mc_count_16_be)
__export_counter (mc_count_16_be_4_generic, _mc_count_16_be_4)
