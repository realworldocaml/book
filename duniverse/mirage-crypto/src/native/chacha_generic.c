/* Based on https://github.com/abeaumont/ocaml-chacha.git */

#include "mirage_crypto.h"

static inline void mc_chacha_quarterround(uint32_t *x, int a, int b, int c, int d) {
  x[a] += x[b]; x[d] = rol32(x[d] ^ x[a], 16);
  x[c] += x[d]; x[b] = rol32(x[b] ^ x[c], 12);
  x[a] += x[b]; x[d] = rol32(x[d] ^ x[a], 8);
  x[c] += x[d]; x[b] = rol32(x[b] ^ x[c], 7);
}

static inline uint32_t mc_get_u32_le(uint8_t *input, int offset) {
  return input[offset]
    | (input[offset + 1] << 8)
    | (input[offset + 2] << 16)
    | (input[offset + 3] << 24);
}

static inline void mc_set_u32_le(uint8_t *input, int offset, uint32_t value) {
  input[offset] = (uint8_t) value;
  input[offset + 1] = (uint8_t) (value >> 8);
  input[offset + 2] = (uint8_t) (value >> 16);
  input[offset + 3] = (uint8_t) (value >> 24);
}

void mc_chacha_core_generic(int count, uint8_t *src, uint8_t *dst) {
  uint32_t x[16];
  for (int i = 0; i < 16; i++) {
    x[i] = mc_get_u32_le(src, i * 4);
  }
  for (int i = 0; i < count; i++) {
    mc_chacha_quarterround(x, 0, 4, 8, 12);
    mc_chacha_quarterround(x, 1, 5, 9, 13);
    mc_chacha_quarterround(x, 2, 6, 10, 14);
    mc_chacha_quarterround(x, 3, 7, 11, 15);

    mc_chacha_quarterround(x, 0, 5, 10, 15);
    mc_chacha_quarterround(x, 1, 6, 11, 12);
    mc_chacha_quarterround(x, 2, 7, 8, 13);
    mc_chacha_quarterround(x, 3, 4, 9, 14);
  }
  for (int i = 0; i < 16; i++) {
    uint32_t xi = x[i];
    uint32_t hj = mc_get_u32_le(src, i * 4);
    mc_set_u32_le(dst, i * 4, xi + hj);
  }
}

