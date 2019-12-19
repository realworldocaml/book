#include "../nocrypto.h"

#include "md5.h"
#include "sha1.h"
#include "sha256.h"
#include "sha512.h"

#define __define_hash(name, upper)                                           \
                                                                             \
  CAMLprim value                                                             \
  caml_nc_ ## name ## _init (value ctx) {                                    \
    nc_ ## name ## _init ((struct name ## _ctx *) String_val (ctx));         \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_nc_ ## name ## _update (value ctx, value src, value off, value len) { \
    nc_ ## name ## _update (                                                 \
      (struct name ## _ctx *) String_val (ctx),                              \
      _ba_uint8_off (src, off), Int_val (len));                              \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_nc_ ## name ## _finalize (value ctx, value dst, value off) {          \
    nc_ ## name ## _finalize (                                               \
      (struct name ## _ctx *) String_val (ctx),                              \
      _ba_uint8_off (dst, off));                                             \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_nc_ ## name ## _ctx_size (__unit ()) {                                \
    return Val_int (upper ## _CTX_SIZE);                                     \
  }

__define_hash (md5, MD5)
__define_hash (sha1, SHA1)
__define_hash (sha224, SHA224)
__define_hash (sha256, SHA256)
__define_hash (sha384, SHA384)
__define_hash (sha512, SHA512)
