/*
 * Copyright (c) 2016 Andy Ray.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#if !__USE_MINGW_ANSI_STDIO && (defined(__MINGW32__) || defined(__MINGW64__))
#define __USE_MINGW_ANSI_STDIO 1
#endif

#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include <caml/hash.h>
#include <caml/memory.h>

#include <stdio.h>
#include <stdint.h>
#include <float.h>
#include <math.h>
#include <string.h>

#include "ctypes_ldouble_stubs.h"
#include "ctypes_complex_compatibility.h"

/*********************** long double *************************/

/*
 * long double comes in various different flavours on different
 * platforms/architectures.
 *
 * 8 byte double - arm, msvc
 * 10 byte extended - intel gcc.  can be packed into 12 or 16 bytes.
 * 16 byte - powerpc, either IEEE quad float or __ibm128 double double
 *
 * We make a best guess as to the format based on LDBL_MANT_DIG.
 * This only affects the operation of hashing and serialization.
 *
 * For deserialization we consider it an error if the stored
 * value is a different format.  Doing such conversions would
 * get very complicated.
 *
 * Regarding endianness - the 8 and 16 byte formats should
 * interwork between big and little endian systems.  The
 * intel extended 10 byte format only seems to occurs on
 * x86 so we dont need to consider endianness.
 *
 * In case a format is encountered that we do not understand,
 * then we fall back to casting the value to a double.
 *
 */

#define LDOUBLE_STORAGE_BYTES sizeof(long double)
#if (LDBL_MANT_DIG == 53)      // 64 bit - same as double
#define LDOUBLE_VALUE_BYTES 8
#elif (LDBL_MANT_DIG == 64)    // intel 80 bit extended
#define LDOUBLE_VALUE_BYTES 10
#elif (LDBL_MANT_DIG == 106)   // __ibm128 (pair of doubles)
#define LDOUBLE_VALUE_BYTES 16
#elif (LDBL_MANT_DIG == 113)   // ieee __float128
#define LDOUBLE_VALUE_BYTES 16
#else
#define LDOUBLE_VALUE_BYTES LDOUBLE_STORAGE_BYTES
#endif

static inline long double ldouble_custom_val(value v) {
  long double r;
  memcpy(&r, Data_custom_val(v), sizeof(r));
  return r;
}

// initialized in ldouble_init
static long double nan_;

static long double norm(long double x) {
  switch (fpclassify(x)){
  case FP_ZERO      : return 0.0L; // if -0 force to +0.
  case FP_NAN       : return nan_;  // cannonical nan
  default           : return x;
  }
}

static int ldouble_cmp(long double u1, long double u2) {
  if (u1 < u2) return -1;
  if (u1 > u2) return 1;
  if (u1 != u2) {
    caml_compare_unordered = 1;
    if (u1 == u1) return 1;  // u2 is nan
    if (u2 == u2) return -1; // u1 is nan
    // both nan ==> equal
  }
  return 0;
}

static int ldouble_cmp_val(value v1, value v2)
{
  long double u1 = ldouble_custom_val(v1);
  long double u2 = ldouble_custom_val(v2);
  return ldouble_cmp(u1, u2);
}

static uint32_t ldouble_mix_hash(uint32_t hash, long double d) {
  union {
    long double d;
    uint32_t a[(LDOUBLE_STORAGE_BYTES+3)/4];
  } u;
  u.d = norm(d);

  if (LDOUBLE_VALUE_BYTES == 16) {
    // ieee quad or __ibm128
#ifdef ARCH_BIG_ENDIAN
    hash = caml_hash_mix_uint32(hash, u.a[0]);
    hash = caml_hash_mix_uint32(hash, u.a[1]);
    hash = caml_hash_mix_uint32(hash, u.a[2]);
    hash = caml_hash_mix_uint32(hash, u.a[3]);
#else
    hash = caml_hash_mix_uint32(hash, u.a[1]);
    hash = caml_hash_mix_uint32(hash, u.a[0]);
    hash = caml_hash_mix_uint32(hash, u.a[3]);
    hash = caml_hash_mix_uint32(hash, u.a[2]);
#endif
  } else if (LDOUBLE_VALUE_BYTES == 10) {
    // intel extended
    hash = caml_hash_mix_uint32(hash, u.a[0]);
    hash = caml_hash_mix_uint32(hash, u.a[1]);
    hash = caml_hash_mix_uint32(hash, u.a[2] & 0xFFFF);
  } else {
    // either LDOUBLE_VALUE_BYTES == 8, or we dont know what else to do.
    hash = caml_hash_mix_double(hash,  (double) d);
  }
  return hash;

}

static intnat ldouble_hash(value v) {
  return ldouble_mix_hash(0, ldouble_custom_val(v));
}

static void ldouble_serialize_data(long double *q) {
  unsigned char *p = (unsigned char *)q;
  if (LDOUBLE_VALUE_BYTES == 16) {
    caml_serialize_block_8(p, 2);
  } else if (LDOUBLE_VALUE_BYTES == 10) {
    caml_serialize_block_8(p, 1);
    caml_serialize_block_2(p+8, 1);
  } else {
    double d = (double) *q;
    if (sizeof(double) == 4) caml_serialize_float_4(d);
    else caml_serialize_float_8(d);
  }
}

static void ldouble_serialize(value v, uintnat *wsize_32, uintnat *wsize_64) {
  long double p = norm(ldouble_custom_val(v));
  caml_serialize_int_1(LDBL_MANT_DIG);
  ldouble_serialize_data(&p);
  *wsize_32 = *wsize_64 = sizeof(long double);
}

static void ldouble_deserialize_data(long double *q) {
  unsigned char *p = (unsigned char *)q;
  if (LDOUBLE_VALUE_BYTES == 16) {
    caml_deserialize_block_8(p, 2);
  } else if (LDOUBLE_VALUE_BYTES == 10) {
    caml_deserialize_block_8(p, 1);
    caml_deserialize_block_2(p+8, 1);
  } else {
    double d;
    if (sizeof(double) == 4) d = caml_deserialize_float_4();
    else d = caml_deserialize_float_8();
    *q = (long double) d;
  }
}

static uintnat ldouble_deserialize(void *d) {
  if (caml_deserialize_uint_1() != LDBL_MANT_DIG)
    caml_deserialize_error("invalid long double size");
  ldouble_deserialize_data((long double *) d);
  return (sizeof(long double));
}

static struct custom_operations caml_ldouble_ops = {
  "ctypes:ldouble",
  custom_finalize_default,
  ldouble_cmp_val,
  ldouble_hash,
  ldouble_serialize,
  ldouble_deserialize,
  custom_compare_ext_default
};

value ctypes_copy_ldouble(long double u)
{
  value res = caml_alloc_custom(&caml_ldouble_ops, sizeof(long double), 0, 1);
  memcpy(Data_custom_val(res), &u, sizeof(u));
  return res;
}

long double ctypes_ldouble_val(value v) { return ldouble_custom_val(v); }

CAMLprim value ctypes_ldouble_of_float(value a) {
  CAMLparam1(a);
  CAMLreturn(ctypes_copy_ldouble(Double_val(a)));
}
CAMLprim value ctypes_ldouble_to_float(value a) {
  CAMLparam1(a);
  CAMLreturn(caml_copy_double(ldouble_custom_val(a)));
}
CAMLprim value ctypes_ldouble_of_int(value a) {
  CAMLparam1(a);
  CAMLreturn(ctypes_copy_ldouble(Long_val(a)));
}
CAMLprim value ctypes_ldouble_to_int(value a) {
  CAMLparam1(a);
  long double b = ldouble_custom_val(a);
  intnat c = b;
  CAMLreturn(Val_long(c));
}

#define OP2(OPNAME, OP)                                                               \
  CAMLprim value ctypes_ldouble_ ## OPNAME(value a, value b) {                        \
    CAMLparam2(a, b);                                                                 \
    CAMLreturn(ctypes_copy_ldouble( ldouble_custom_val(a) OP ldouble_custom_val(b))); \
  }

OP2(add, +)
OP2(sub, -)
OP2(mul, *)
OP2(div, /)

CAMLprim value ctypes_ldouble_neg(value a) {
  CAMLparam1(a);
  CAMLreturn(ctypes_copy_ldouble( - ldouble_custom_val(a)));
}

#define FN1(OP)                                                   \
  CAMLprim value ctypes_ldouble_ ## OP (value a) {                \
    CAMLparam1(a);                                                \
    CAMLreturn(ctypes_copy_ldouble( OP (ldouble_custom_val(a)))); \
  }

#define FN2(OP)                                                                          \
  CAMLprim value ctypes_ldouble_ ## OP (value a, value b) {                              \
    CAMLparam2(a, b);                                                                    \
    CAMLreturn(ctypes_copy_ldouble( OP (ldouble_custom_val(a), ldouble_custom_val(b)))); \
  }

#define FN1FAIL(OP)                                                        \
  CAMLprim value ctypes_ldouble_ ## OP (value a) {                         \
    CAMLparam1(a);                                                         \
    caml_failwith("ctypes: " #OP " does not exist on current platform");   \
  }

#define FN2FAIL(OP)                                                        \
  CAMLprim value ctypes_ldouble_ ## OP (value a, value b) {                \
    CAMLparam2(a, b);                                                      \
    caml_failwith("ctypes: " #OP " does not exist on current platform");   \
  }

FN2(powl)
FN1(sqrtl)
FN1(expl)
FN1(logl)
FN1(log10l)
#ifdef __NetBSD__
FN1FAIL(expm1l)
FN1FAIL(log1pl)
#else
FN1(expm1l)
FN1(log1pl)
#endif
FN1(cosl)
FN1(sinl)
FN1(tanl)
FN1(acosl)
FN1(asinl)
FN1(atanl)
FN2(atan2l)
FN2(hypotl)
FN1(coshl)
FN1(sinhl)
FN1(tanhl)
FN1(acoshl)
FN1(asinhl)
FN1(atanhl)
FN1(ceill)
FN1(floorl)
FN1(fabsl)
#ifdef __NetBSD__
FN2FAIL(remainderl)
#else
FN2(remainderl)
#endif
FN2(copysignl)

#undef OP2
#undef FN1
#undef FN2
#undef FN1FAIL
#undef FN2FAIL

CAMLprim value ctypes_ldouble_frexp(value v) {
  CAMLparam1(v);
  CAMLlocal2(r, rfv);
  long double f = ldouble_custom_val(v);
  int ri;
  long double rf;
  r = caml_alloc_tuple(2);
  rf = frexpl(f, &ri);
  rfv = ctypes_copy_ldouble(rf);
  Store_field(r,0, rfv);
  Store_field(r,1, Val_int(ri));
  CAMLreturn(r);
}

CAMLprim value ctypes_ldouble_ldexp(value vf, value vi) {
  CAMLparam2(vf, vi);
  CAMLlocal1(r);
  long double f = ldouble_custom_val(vf);
  int i = Int_val(vi);
  long double rf = ldexpl(f, i);
  r = ctypes_copy_ldouble(rf);
  CAMLreturn(r);
}

CAMLprim value ctypes_ldouble_modf(value v) {
  CAMLparam1(v);
  CAMLlocal1(r);
  long double f = ldouble_custom_val(v);
  long double rf2;
  long double rf1 = modfl(f, &rf2);
  r = caml_alloc_tuple(2);
  Store_field(r, 0, ctypes_copy_ldouble(rf1));
  Store_field(r, 1, ctypes_copy_ldouble(rf2));
  CAMLreturn(r);
}

enum {
  ml_FP_NORMAL = 0,
  ml_FP_SUBNORMAL,
  ml_FP_ZERO,
  ml_FP_INFINITE,
  ml_FP_NAN,
};

CAMLprim value ctypes_ldouble_classify(value v){
  CAMLparam1(v);
  CAMLlocal1(r);
  long double f = ldouble_custom_val(v);
  switch (fpclassify(f)){
  case FP_NORMAL    : r = Val_int(ml_FP_NORMAL); break;
  case FP_SUBNORMAL : r = Val_int(ml_FP_SUBNORMAL); break;
  case FP_ZERO      : r = Val_int(ml_FP_ZERO); break;
  case FP_INFINITE  : r = Val_int(ml_FP_INFINITE); break;
  case FP_NAN       :
  default           : r = Val_int(ml_FP_NAN); break;
  }
  CAMLreturn(r);
}

static char *format_ldouble(int width, int prec, long double d) {
  size_t print_len;
  char *buf = NULL;

  // find length
  print_len = snprintf(NULL, 0, "%*.*Lf", width, prec, d);
  if (0 == print_len) // this shouldn't happen
    caml_invalid_argument("bad ldouble format");

  // allocate buffer
  buf = malloc(print_len+1);
  if (NULL == buf) caml_raise_out_of_memory();

  // format string
  buf[0] = '\0';
  snprintf(buf, print_len+1, "%*.*Lf", width, prec, d);
  return buf;
}

CAMLprim value ctypes_ldouble_format(value width, value prec, value d) {
  CAMLparam3(width, prec, d);
  CAMLlocal1(s);
  char *str = format_ldouble(Int_val(width), Int_val(prec),
                             ldouble_custom_val(d));
  s = caml_copy_string(str);
  free(str);
  CAMLreturn(s);
}

CAMLprim value ctypes_ldouble_of_string(value v) {
  CAMLparam1(v);
  char *str = String_val(v);
  int len = caml_string_length(v);
  char *end;
  long double r;
  if (0 == len) caml_invalid_argument("LDouble.of_string");
  r = strtold(str, &end);
  if (*end != '\0') caml_invalid_argument("LDouble.of_string");
  CAMLreturn(ctypes_copy_ldouble(r));
}

/* debug code */
/*static char hex_char(char x) {
  if (x < 10) return '0' + x;
  return 'a' + x - 10;
}

CAMLprim value ctypes_ldouble_to_hex(value v) {
  CAMLparam1(v);
  static char x[LDOUBLE_STORAGE_BYTES*2 + 1];
  char *p = (char *) Data_custom_val(v);
  int i;
  for (i=0; i<LDOUBLE_STORAGE_BYTES; i++) {
    x[i*2+0] = hex_char(((*(p+i)) >> 0) & 0xf);
    x[i*2+1] = hex_char(((*(p+i)) >> 4) & 0xf);
  }
  x[LDOUBLE_STORAGE_BYTES*2] = 0;
  CAMLreturn(caml_copy_string(x));
}*/

value ctypes_ldouble_min(value unit) { return ctypes_copy_ldouble(-LDBL_MAX); }
value ctypes_ldouble_max(value unit) { return ctypes_copy_ldouble(LDBL_MAX); }
value ctypes_ldouble_epsilon(value unit) { return ctypes_copy_ldouble(LDBL_EPSILON); }
value ctypes_ldouble_nan(value unit) { return ctypes_copy_ldouble(nan_); }
// XXX note; -(log 0) gives +ve inf (and vice versa).  Is this consistent? *)
value ctypes_ldouble_inf(value unit) { return ctypes_copy_ldouble(-log(0)); }
value ctypes_ldouble_ninf(value unit) { return ctypes_copy_ldouble(log(0)); }

value ctypes_ldouble_size(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(r);
  r = caml_alloc_tuple(2);
  Store_field(r,0, Val_int(LDOUBLE_STORAGE_BYTES));
  Store_field(r,1, Val_int(LDOUBLE_VALUE_BYTES));
  CAMLreturn(r);
}

/*********************** complex *************************/

static inline long double _Complex ldouble_complex_custom_val(value v)
{
  long double _Complex r;
  memcpy(&r, Data_custom_val(v), sizeof(r));
  return r;
}

static int ldouble_complex_cmp_val(value v1, value v2)
{
  long double _Complex u1 = ldouble_complex_custom_val(v1);
  long double _Complex u2 = ldouble_complex_custom_val(v2);
  int cmp_real = ldouble_cmp(ctypes_compat_creall(u1), ctypes_compat_creall(u2));
  return cmp_real == 0 ? ldouble_cmp(ctypes_compat_cimagl(u1), ctypes_compat_cimagl(u2)) : cmp_real;
}

static intnat ldouble_complex_hash(value v) {
  long double _Complex c = ldouble_complex_custom_val(v);
  return ldouble_mix_hash(ldouble_mix_hash(0, ctypes_compat_creall(c)), ctypes_compat_cimagl(c));
}

static void ldouble_complex_serialize(value v, uintnat *wsize_32, uintnat *wsize_64) {
  long double re,im;
  long double _Complex c;
  void * p = Data_custom_val(v);
#if defined(__GNUC__) && __GNUC__  == 6 && __GNUC_MINOR__ == 4
  /* workaround gcc bug. gcc tries to inline the memcpy calls, but
   * fails with an internal compiler error. I've observed this error
   * only under Alpine Linux, other distros have already imported a
   * patch from upstream.
   */
  void *(*volatile mymemcpy)(void*,const void*,size_t) = memcpy;
  mymemcpy(&c, p, sizeof(c));
#else
  memcpy(&c, p, sizeof(c));
#endif
  caml_serialize_int_1(LDBL_MANT_DIG);
  re = ctypes_compat_creall(c);
  ldouble_serialize_data(&re);
  im = ctypes_compat_cimagl(c);
  ldouble_serialize_data(&im);
  *wsize_32 = *wsize_64 = sizeof(long double _Complex);
}

static uintnat ldouble_complex_deserialize(void *d) {
  long double re, im;
  long double _Complex c;
  if (caml_deserialize_uint_1() != LDBL_MANT_DIG)
    caml_deserialize_error("invalid long double size");
  ldouble_deserialize_data(&re);
  ldouble_deserialize_data(&im);
  c = ctypes_compat_make_complexl(re, im);
  memcpy(d, &c, sizeof(c));
  return (sizeof(long double _Complex));
}

static struct custom_operations caml_ldouble_complex_ops = {
  "ctypes:ldouble_complex",
  custom_finalize_default,
  ldouble_complex_cmp_val,
  ldouble_complex_hash,
  ldouble_complex_serialize,
  ldouble_complex_deserialize,
  custom_compare_ext_default
};

value ctypes_copy_ldouble_complex(long double _Complex u)
{
  value res = caml_alloc_custom(&caml_ldouble_complex_ops, sizeof(long double _Complex), 0, 1);
  memcpy(Data_custom_val(res), &u, sizeof(u));
  return res;
}

long double _Complex ctypes_ldouble_complex_val(value v) {
  return ldouble_complex_custom_val(v);
}

/* make : t -> t -> complex */
CAMLprim value ctypes_ldouble_complex_make(value r, value i) {
  CAMLparam2(r, i);
  long double re = ldouble_custom_val(r);
  long double im = ldouble_custom_val(i);
  CAMLreturn(ctypes_copy_ldouble_complex(ctypes_compat_make_complexl(re, im)));
}

/* real : complex -> t */
CAMLprim value ctypes_ldouble_complex_real(value v) {
  CAMLparam1(v);
  CAMLreturn(ctypes_copy_ldouble(ctypes_compat_creall(ldouble_complex_custom_val(v))));
}

/* imag : complex -> t */
CAMLprim value ctypes_ldouble_complex_imag(value v) {
  CAMLparam1(v);
  CAMLreturn(ctypes_copy_ldouble(ctypes_compat_cimagl(ldouble_complex_custom_val(v))));
}

#define OP2(OPNAME, OP)                                                    \
  CAMLprim value ctypes_ldouble_complex_ ## OPNAME(value a, value b) {     \
    CAMLparam2(a, b);                                                      \
    CAMLreturn(ctypes_copy_ldouble_complex(                                \
        ldouble_complex_custom_val(a) OP ldouble_complex_custom_val(b) )); \
  }

OP2(add, +)
OP2(sub, -)
OP2(mul, *)
OP2(div, /)

CAMLprim value ctypes_ldouble_complex_neg(value a) {
  CAMLparam1(a);
  CAMLreturn(ctypes_copy_ldouble_complex( - ldouble_complex_custom_val(a) ));
}

#define FN1(OP)                                                                   \
  CAMLprim value ctypes_ldouble_complex_ ## OP (value a) {                        \
    CAMLparam1(a);                                                                \
    CAMLreturn(ctypes_copy_ldouble_complex( ctypes_compat_ ## OP (ldouble_complex_custom_val(a)))); \
  }

#define FN2(OP)                                                            \
  CAMLprim value ctypes_ldouble_complex_ ## OP (value a, value b) {        \
    CAMLparam2(a, b);                                                      \
    CAMLreturn(ctypes_copy_ldouble_complex(                                \
      ctypes_compat_ ## OP (ldouble_complex_custom_val(a), ldouble_complex_custom_val(b)))); \
  }

FN1(conjl)
FN1(csqrtl)

FN1(cexpl)
FN1(clogl)
FN2(cpowl)

CAMLprim value ctypes_ldouble_complex_cargl(value a) {
  CAMLparam1(a);
  CAMLreturn(ctypes_copy_ldouble( ctypes_compat_cargl(ldouble_complex_custom_val(a))));
}

value ldouble_init(value unit) {
  nan_ = nanl(""); // platform dependant argument - use as cannonical nan
  caml_register_custom_operations(&caml_ldouble_ops);
  caml_register_custom_operations(&caml_ldouble_complex_ops);
  return Val_unit;
}

CAMLprim value ctypes_ldouble_mant_dig(value unit) {
  intnat r = LDBL_MANT_DIG;
  return Val_long(r);
}
