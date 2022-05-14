/* This file has been modified to be used as one of our hash-folding algorithm.
   The reference implementation of siphash is kept in siphash.c.txt */
/*
   SipHash reference C implementation

   Copyright (c) 2012-2014 Jean-Philippe Aumasson
   <jeanphilippe.aumasson@gmail.com>
   Copyright (c) 2012-2014 Daniel J. Bernstein <djb@cr.yp.to>

   To the extent possible under law, the author(s) have dedicated all copyright
   and related and neighboring rights to this software to the public domain
   worldwide. This software is distributed without any warranty.

   You should have received a copy of the CC0 Public Domain Dedication along
   with
   this software. If not, see
   <http://creativecommons.org/publicdomain/zero/1.0/>.

*/

#include <stdint.h>
#include <string.h>
#include <caml/alloc.h>

/* default: SipHash-2-4 */
#define cROUNDS 1
#define dROUNDS 3

#define ROTL(x, b) (uint64_t)(((x) << (b)) | ((x) >> (64 - (b))))

#define U32TO8_LE(p, v)                                                        \
  (p)[0] = (uint8_t)((v));                                                     \
  (p)[1] = (uint8_t)((v) >> 8);                                                \
  (p)[2] = (uint8_t)((v) >> 16);                                               \
  (p)[3] = (uint8_t)((v) >> 24);

#define U64TO8_LE(p, v)                                                        \
  U32TO8_LE((p), (uint32_t)((v)));                                             \
  U32TO8_LE((p) + 4, (uint32_t)((v) >> 32));

#define U8TO64_LE(p)                                                           \
  (((uint64_t)((p)[0])) | ((uint64_t)((p)[1]) << 8) |                          \
   ((uint64_t)((p)[2]) << 16) | ((uint64_t)((p)[3]) << 24) |                   \
   ((uint64_t)((p)[4]) << 32) | ((uint64_t)((p)[5]) << 40) |                   \
   ((uint64_t)((p)[6]) << 48) | ((uint64_t)((p)[7]) << 56))

#define SIPROUND(h)                                                      \
  do {                                                                         \
    h->v0 += h->v1;                                                                  \
    h->v1 = ROTL(h->v1, 13);                                                         \
    h->v1 ^= h->v0;                                                                  \
    h->v0 = ROTL(h->v0, 32);                                                         \
    h->v2 += h->v3;                                                                  \
    h->v3 = ROTL(h->v3, 16);                                                         \
    h->v3 ^= h->v2;                                                                  \
    h->v0 += h->v3;                                                                  \
    h->v3 = ROTL(h->v3, 21);                                                         \
    h->v3 ^= h->v0;                                                                  \
    h->v2 += h->v1;                                                                  \
    h->v1 = ROTL(h->v1, 17);                                                         \
    h->v1 ^= h->v2;                                                                  \
    h->v2 = ROTL(h->v2, 32);                                                         \
  } while (0)

#define TRACE(h)                                                         \
  do {                                                                         \
    printf(" h->v0 %08x %08x\n",  (uint32_t)(h->v0 >> 32),           \
           (uint32_t)h->v0);                                                      \
    printf(" h->v1 %08x %08x\n",  (uint32_t)(h->v1 >> 32),           \
           (uint32_t)h->v1);                                                      \
    printf(" h->v2 %08x %08x\n",  (uint32_t)(h->v2 >> 32),           \
           (uint32_t)h->v2);                                                      \
    printf(" h->v3 %08x %08x\n",  (uint32_t)(h->v3 >> 32),           \
           (uint32_t)h->v3);                                                      \
  } while (0)

/* The code above this line is mostly a copy and paste from siphash reference
   implementation. The code below has been substantially modified. */

struct hash_state
{
  uint64_t v0, v1, v2, v3;
};

/* internal */
void siphash_fold_uint64(value state, uint64_t i)
{
  struct hash_state * h = (struct hash_state *) state;
  unsigned round;
  h->v3 ^= i;
  for (round = 0; round < cROUNDS; ++round)
    SIPROUND(h);
  h->v0 ^= i;
}

CAMLprim value siphash_fold_int64(value st, value i)
{
  siphash_fold_uint64(st, Int64_val(i));
  return st;
}

CAMLprim value siphash_fold_int(value st, value i)
{
  siphash_fold_uint64(st, Long_val(i));
  return st;
}

/* The code has been 'borrowed' from byterun/hash.c in ocaml */
CAMLexport uint64_t caml_hash_normalize_double_to_int64(double d)
{
  union {
    double d;
    uint64_t i64;
#if defined(ARCH_BIG_ENDIAN) || (defined(__arm__) && !defined(__ARM_EABI__))
    struct { uint32_t h; uint32_t l; } i;
#else
    struct { uint32_t l; uint32_t h; } i;
#endif
  } u;
  uint32_t h, l;
  /* Convert to two 32-bit halves */
  u.d = d;
  h = u.i.h; l = u.i.l;
  /* Normalize NaNs */
  if ((h & 0x7FF00000) == 0x7FF00000 && (l | (h & 0xFFFFF)) != 0) {
    h = 0x7FF00000;
    l = 0x00000001;
  }
  /* Normalize -0 into +0 */
  else if (h == 0x80000000 && l == 0) {
    h = 0;
  }
  u.i.h = h;
  u.i.l = l;
  return u.i64;
}

CAMLprim value siphash_fold_float(value st, value i)
{
  uint64_t x = caml_hash_normalize_double_to_int64(Double_val(i));
  siphash_fold_uint64(st, x);
  return st;
}

CAMLprim value siphash_fold_string(value st, value s)
{
  const mlsize_t len = caml_string_length(s);
  const int left = len & 7;
  unsigned char * in;
  mlsize_t i;
  uint64_t w;
  /* The length must be mixed in before the elements to avoid a violation of the rule described by Perfect_hash. */
  siphash_fold_uint64(st, ((uint64_t)len));
  /* Mix by 64-bit blocks (little-endian) */
  for (i = 0; i + 8 <= len; i += 8) {
    w = U8TO64_LE(Bp_val(s)+i);
    siphash_fold_uint64(st, w);
  }
  in = (uint8_t*)Bp_val(s) + i;
  w = ((uint64_t)len) << 56;
  switch (left) {
  case 7:
    w |= ((uint64_t)in[6]) << 48;
    /* fall through */
  case 6:
    w |= ((uint64_t)in[5]) << 40;
    /* fall through */
  case 5:
    w |= ((uint64_t)in[4]) << 32;
    /* fall through */
  case 4:
    w |= ((uint64_t)in[3]) << 24;
    /* fall through */
  case 3:
    w |= ((uint64_t)in[2]) << 16;
    /* fall through */
  case 2:
    w |= ((uint64_t)in[1]) << 8;
    /* fall through */
  case 1:
    w |= ((uint64_t)in[0]);
    break;
  case 0:
    break;
  }
  siphash_fold_uint64(st,w);
  return st;
}

CAMLprim value siphash_alloc ()
{
  return caml_alloc_small(sizeof(struct hash_state) / sizeof(value), Abstract_tag);
}

CAMLprim value siphash_reset (value st, value key)
{
  struct hash_state * h = (struct hash_state *)  (Op_val(st));
  char buffer[16];
  uint64_t k0, k1;
  unsigned i;
  unsigned key_len = caml_string_length(key);
  /* initialize the buffer */
  memset(buffer, 0, 16);
  /* copy the first 16 chars of the key to the buffer */
  for (i = 0; i < (key_len > 16 ? 16 : key_len); i++)
    buffer[i] = Byte_u(key,i);
  /* initialize k0 and k1 */
  k0 = U8TO64_LE(buffer);
  k1 = U8TO64_LE(buffer + 8);
  /* Those are verbatim from siphash reference implementation.*/
  /* "somepseudorandomlygeneratedbytes" */
  h->v0 = 0x736f6d6570736575ULL ^ k0;
  h->v1 = 0x646f72616e646f6dULL ^ k1;
  h->v2 = 0x6c7967656e657261ULL ^ k0;
  h->v3 = 0x7465646279746573ULL ^ k1;
  /* If we switch to DOUBLE, we need to bring back the rest of siphash init
     code. */
  return st;
}

/* This function destroy (that is, mixes) the content of the hash state. That
   means that it is not possible to stop in the middle of a hash, and ask for
   the current hash value, and continue to hash the rest of a structure. */
CAMLprim value siphash_get_hash_value(value st)
{
  struct hash_state * h = (struct hash_state *) st;
  uint64_t b;
  unsigned i;
  /* This is the final mix step of the reference implementation of siphash, in
     the "non-DOUBLE" case (that is, where we get a single int64 out of the hash
     state). */
  h->v2 ^= 0xff;
  for (i = 0; i < dROUNDS; ++i)
    SIPROUND(h);
  b = h->v0 ^ h->v1 ^ h->v2 ^ h->v3;
  /* We lose one bit of precision here. */
  return Val_long(b);
}

CAMLprim value siphash_blit_hash_to_bytes(value st, value bytes)
{
  struct hash_state * h = (struct hash_state *) st;
  uint8_t *str = (uint8_t *) (Bp_val(bytes));
  U64TO8_LE(str +  0, h -> v0);
  U64TO8_LE(str +  8, h -> v1);
  U64TO8_LE(str + 16, h -> v2);
  U64TO8_LE(str + 24, h -> v3);
  return Val_long(0);
}
