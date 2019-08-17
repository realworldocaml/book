#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/hash.h>

/* Final mix and return from the hash.c implementation from INRIA */
#define FINAL_MIX_AND_RETURN(h) \
  h ^= h >> 16; \
  h *= 0x85ebca6b; \
  h ^= h >> 13; \
  h *= 0xc2b2ae35; \
  h ^= h >> 16; \
  return Val_int(h & 0x3FFFFFFFU);

CAMLprim value Base_hash_string (value string)
{
  uint32_t h;
  h = caml_hash_mix_string (0, string);
  FINAL_MIX_AND_RETURN(h)
}

CAMLprim value Base_hash_double (value d)
{
  uint32_t h;
  h = caml_hash_mix_double (0, Double_val(d));
  FINAL_MIX_AND_RETURN (h);
}
