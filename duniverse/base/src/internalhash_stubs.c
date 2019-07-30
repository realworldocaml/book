#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/hash.h>
#include "internalhash.h"

/* This pretends that the state of the OCaml internal hash function, which is an
   int32, is actually stored in an OCaml int. */

CAMLprim value Base_internalhash_fold_int32(value st, value i)
{
  return Val_long(caml_hash_mix_uint32(Long_val(st), Int32_val(i)));
}

CAMLprim value Base_internalhash_fold_nativeint(value st, value i)
{
  return Val_long(caml_hash_mix_intnat(Long_val(st), Nativeint_val(i)));
}

CAMLprim value Base_internalhash_fold_int64(value st, value i)
{
  return Val_long(caml_hash_mix_int64(Long_val(st), Int64_val(i)));
}

CAMLprim value Base_internalhash_fold_int(value st, value i)
{
  return Val_long(caml_hash_mix_intnat(Long_val(st), Long_val(i)));
}

CAMLprim value Base_internalhash_fold_float(value st, value i)
{
  return Val_long(caml_hash_mix_double(Long_val(st), Double_val(i)));
}

/* This code mimics what hashtbl.hash does in OCaml's hash.c */
#define FINAL_MIX(h)                            \
  h ^= h >> 16; \
  h *= 0x85ebca6b; \
  h ^= h >> 13; \
  h *= 0xc2b2ae35; \
  h ^= h >> 16;

CAMLprim value Base_internalhash_get_hash_value(value st)
{
  uint32_t h = Int_val(st);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU); /*30 bits*/
}

/* Macros copied from hash.c in ocaml distribution */
#define ROTL32(x,n) ((x) << n | (x) >> (32-n))

#define MIX(h,d)   \
  d *= 0xcc9e2d51; \
  d = ROTL32(d, 15); \
  d *= 0x1b873593; \
  h ^= d; \
  h = ROTL32(h, 13); \
  h = h * 5 + 0xe6546b64;

/* Version of [caml_hash_mix_string] from hash.c - adapted for arbitrary char arrays */
CAMLexport uint32_t Base_internalhash_fold_blob(uint32_t h, mlsize_t len, uint8_t *s)
{
  mlsize_t i;
  uint32_t w;

  /* Mix by 32-bit blocks (little-endian) */
  for (i = 0; i + 4 <= len; i += 4) {
#ifdef ARCH_BIG_ENDIAN
    w = s[i]
      | (s[i+1] << 8)
      | (s[i+2] << 16)
      | (s[i+3] << 24);
#else
    w = *((uint32_t *) &(s[i]));
#endif
    MIX(h, w);
  }
  /* Finish with up to 3 bytes */
  w = 0;
  switch (len & 3) {
  case 3: w  = s[i+2] << 16;   /* fallthrough */
  case 2: w |= s[i+1] << 8;    /* fallthrough */
  case 1: w |= s[i];
          MIX(h, w);
  default: /*skip*/;     /* len & 3 == 0, no extra bytes, do nothing */
  }
  /* Finally, mix in the length. Ignore the upper 32 bits, generally 0. */
  h ^= (uint32_t) len;
  return h;
}

CAMLprim value Base_internalhash_fold_string(value st, value v_str)
{
  uint32_t h = Long_val(st);
  mlsize_t len = caml_string_length(v_str);
  uint8_t *s = (uint8_t *) String_val(v_str);

  h = Base_internalhash_fold_blob(h, len, s);

  return Val_long(h);
}
