/** 
  Implementation of Z module.


  This file is part of the Zarith library 
  http://forge.ocamlcore.org/projects/zarith .
  It is distributed under LGPL 2 licensing, with static linking exception.
  See the LICENSE file included in the distribution.

  Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
  Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
  a joint laboratory by:
  CNRS (Centre national de la recherche scientifique, France),
  ENS (École normale supérieure, Paris, France),
  INRIA Rocquencourt (Institut national de recherche en informatique, France).

*/


/*---------------------------------------------------
  INCLUDES 
  ---------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include <limits.h>

#ifdef HAS_GMP
#include <gmp.h>
#endif
#ifdef HAS_MPIR
#include <mpir.h>
#endif

#include "z_features.h"
#include "zarith.h"

#ifdef __cplusplus
extern "C" {
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/callback.h>
#include <caml/intext.h>
#ifdef Z_OCAML_HASH
#include <caml/hash.h>
#endif

#define inline __inline

#ifdef _MSC_VER
#include <float.h>
#endif

/*---------------------------------------------------
  CONFIGURATION
  ---------------------------------------------------*/

/* Whether to enable native (i.e. non-mpn_) operations and output
   ocaml integers when possible.
   Highly recommended.
 */
#define Z_FAST_PATH  1
#define Z_USE_NATINT 1

/* Sanity checks. */
#define Z_PERFORM_CHECK 0

/* Enable performance counters. 
   Prints some info on stdout at exit. 
*/
/*
  #define Z_PERF_COUNTER 0
  now set by configure
*/

/* whether to use custom blocks (supporting serialization, comparison &
   hashing) instead of abstract tags
*/
#define Z_CUSTOM_BLOCK 1

/* whether the "compare_ext" operation over custom blocks is supported.
   This operation is required for OCaml's generic comparisons to
   operate properly over values of type Z.t.
   The compare_ext operation is supported in OCaml since version 3.12.1. 
*/
/*
  #define Z_OCAML_COMPARE_EXT 0
  now set by configure
*/

/*---------------------------------------------------
  DATA STRUCTURES
  ---------------------------------------------------*/

/*
   we assume that:
   - intnat is a signed integer type
   - mp_limb_t is an unsigned integer type
   - sizeof(intnat) == sizeof(mp_limb_t) == either 4 or 8
*/

#ifdef _WIN64
#define PRINTF_LIMB "I64"
#else
#define PRINTF_LIMB "l"
#endif

/*
  A z object x can be:
  - either an ocaml int
  - or a block with abstract or custom tag and containing:
    . a 1 value header containing the sign Z_SIGN(x) and the size Z_SIZE(x) 
    . Z_SIZE(x) mp_limb_t

  Invariant:
  - if the number fits in an int, it is stored in an int, not a block
  - if the number is stored in a block, then Z_SIZE(x) >= 1 and
  the most significant limb Z_LIMB(x)[Z_SIZE(x)] is not 0 
 */


/* a sign is always denoted as 0 (+) or Z_SIGN_MASK (-) */
#ifdef ARCH_SIXTYFOUR
#define Z_SIGN_MASK 0x8000000000000000
#define Z_SIZE_MASK 0x7fffffffffffffff
#else
#define Z_SIGN_MASK 0x80000000
#define Z_SIZE_MASK 0x7fffffff
#endif

#if Z_CUSTOM_BLOCK
#define Z_HEAD(x)   (*((value*)Data_custom_val((x))))
#define Z_LIMB(x)   ((mp_limb_t*)Data_custom_val((x)) + 1)
#else
#define Z_HEAD(x)   (Field((x),0))
#define Z_LIMB(x)   ((mp_limb_t*)&(Field((x),1)))
#endif
#define Z_SIGN(x)   (Z_HEAD((x)) & Z_SIGN_MASK)
#define Z_SIZE(x)   (Z_HEAD((x)) & Z_SIZE_MASK)

/* bounds of an Ocaml int */
#ifdef ARCH_SIXTYFOUR
#define Z_MAX_INT       0x3fffffffffffffff
#define Z_MIN_INT     (-0x4000000000000000)
#else
#define Z_MAX_INT       0x3fffffff
#define Z_MIN_INT     (-0x40000000)
#endif
#define Z_FITS_INT(v)  ((v) >= Z_MIN_INT && (v) <= Z_MAX_INT)

/* Z_MAX_INT may not be representable exactly as a double => we use a
   lower approximation to be safe
 */
#ifdef ARCH_SIXTYFOUR
#define Z_MAX_INT_FL    0x3ffffffffffff000
#define Z_MIN_INT_FL    (-Z_MAX_INT_FL)
#else
#define Z_MAX_INT_FL    Z_MAX_INT
#define Z_MIN_INT_FL    Z_MIN_INT
#endif

/* safe bounds to avoid overflow in multiplication */
#ifdef ARCH_SIXTYFOUR
#define Z_MAX_HINT  0x3fffffff
#else
#define Z_MAX_HINT  0x3fff
#endif
#define Z_MIN_HINT (-Z_MAX_HINT)
#define Z_FITS_HINT(v) ((v) >= Z_MIN_HINT && (v) <= Z_MAX_HINT)

/* hi bit of OCaml int32, int64 & nativeint */
#define Z_HI_INT32   0x80000000
#define Z_HI_INT64   0x8000000000000000LL
#ifdef ARCH_SIXTYFOUR
#define Z_HI_INTNAT  Z_HI_INT64
#define Z_HI_INT     0x4000000000000000
#else
#define Z_HI_INTNAT  Z_HI_INT32
#define Z_HI_INT     0x40000000
#endif

/* safe bounds for the length of a base n string fitting in a native
   int. Defined as the result of (n - 2) log_base(2) with n = 64 or
   32. 
*/
#ifdef ARCH_SIXTYFOUR
#define Z_BASE16_LENGTH_OP 15
#define Z_BASE10_LENGTH_OP 18
#define Z_BASE8_LENGTH_OP 20
#define Z_BASE2_LENGTH_OP 62
#else
#define Z_BASE16_LENGTH_OP 7
#define Z_BASE10_LENGTH_OP 9
#define Z_BASE8_LENGTH_OP 10
#define Z_BASE2_LENGTH_OP 30
#endif

#define Z_LIMB_BITS  (8 * sizeof(mp_limb_t))


/* performance counters */
unsigned long ml_z_ops = 0;
unsigned long ml_z_slow = 0;
unsigned long ml_z_ops_as = 0;

#if Z_PERF_COUNTER
#define Z_MARK_OP    ml_z_ops++
#define Z_MARK_SLOW  ml_z_slow++
#else
#define Z_MARK_OP
#define Z_MARK_SLOW 
#endif


/*---------------------------------------------------
  UTILITIES
  ---------------------------------------------------*/

extern struct custom_operations ml_z_custom_ops;

static double ml_z_2p32; /* 2 ^ 32 in double */

#if  Z_PERFORM_CHECK
/* for debugging: dump a mp_limb_t array */
static void ml_z_dump(const char* msg, mp_limb_t* p, mp_size_t sz)
{
  mp_size_t i;
  printf("%s %i: ",msg,(int)sz);
  for (i = 0; i < sz; i++)
#ifdef ARCH_SIXTYFOUR
    printf("%08" PRINTF_LIMB "x ",p[i]);
#else
    printf("%04" PRINTF_LIMB "x ",p[i]);
#endif
  printf("\n");
  fflush(stdout);
}
#endif

#if Z_PERFORM_CHECK
/* for debugging: check invariant */
void ml_z_check(const char* fn, int line, const char* arg, value v)
{
  mp_size_t sz;

  if (Is_block(v)) {
#if Z_CUSTOM_BLOCK
    if (Custom_ops_val(v) != &ml_z_custom_ops) {
      printf("ml_z_check: wrong custom block for %s at %s:%i.\n", 
             arg, fn, line);
      exit(1);
    }
    sz = Wosize_val(v) - 1;
#else
    sz = Wosize_val(v);
#endif
    if (Z_SIZE(v) + 2 > sz) {
      printf("ml_z_check: invalid block size (%i / %i) for %s at %s:%i.\n", 
             (int)Z_SIZE(v), (int)sz,
             arg, fn, line);
      exit(1);
    }
    if ((mp_size_t) Z_LIMB(v)[sz - 2] != (mp_size_t)(0xDEADBEEF ^ (sz - 2))) {
      printf("ml_z_check: corrupted block for %s at %s:%i.\n", 
             arg, fn, line);
      exit(1);
    }
    if (Z_SIZE(v) && Z_LIMB(v)[Z_SIZE(v)-1]) return;
#if !Z_USE_NATINT
    if (!Z_SIZE(v)) {
      if (Z_SIGN(v)) {
        printf("ml_z_check: invalid sign of 0 for %s at %s:%i.\n", 
               arg, fn, line);
        exit(1);
      }
      return;
    }
    if (Z_SIZE(v) <= 1 && Z_LIMB(v)[0] <= Z_MAX_INT) {
      printf("ml_z_check: unreduced argument for %s at %s:%i.\n", arg, fn, line);
      ml_z_dump("offending argument: ", Z_LIMB(v), Z_SIZE(v));
      exit(1);
    }
#endif
    printf("ml_z_check failed for %s at %s:%i.\n", arg, fn, line);
    ml_z_dump("offending argument: ", Z_LIMB(v), Z_SIZE(v));
    exit(1);
  }
}
#endif
  
/* for debugging */
#if Z_PERFORM_CHECK
#define Z_CHECK(v) ml_z_check(__FUNCTION__, __LINE__, #v, v)
#else
#define Z_CHECK(v)
#endif

/* allocates z object block with space for sz mp_limb_t;
   does not set the header
 */

#if !Z_PERFORM_CHECK
/* inlined allocation */
#if Z_CUSTOM_BLOCK
#define ml_z_alloc(sz) \
  caml_alloc_custom(&ml_z_custom_ops, (1 + (sz)) * sizeof(value), 0, 1)
#else
#define ml_z_alloc(sz) \
  caml_alloc(1 + (sz), Abstract_tag);
#endif

#else
/* out-of-line allocation, inserting a canary after the last limb */
static value ml_z_alloc(mp_size_t sz)
{
  value v;
#if Z_CUSTOM_BLOCK
  v = caml_alloc_custom(&ml_z_custom_ops, (1 + sz + 1) * sizeof(value), 0, 1);
#else
  v = caml_alloc(1 + sz + 1, Abstract_tag);
#endif
  Z_LIMB(v)[sz] = 0xDEADBEEF ^ sz;
  return v;
}
#endif

/* duplicates the caml block src */
static inline void ml_z_cpy_limb(mp_limb_t* dst, mp_limb_t* src, mp_size_t sz)
{
  memcpy(dst, src, sz * sizeof(mp_limb_t));
}

/* duplicates the mp_limb_t array src */
static inline mp_limb_t* ml_z_dup_limb(mp_limb_t* src, mp_size_t sz)
{
  mp_limb_t* r = (mp_limb_t*) malloc(sz * sizeof(mp_limb_t));
  memcpy(r, src, sz * sizeof(mp_limb_t));
  return r;
}


#ifdef _MSC_VER
#define MAYBE_UNUSED
#else
#define MAYBE_UNUSED (void)
#endif

/* given a z object, define:
   - ptr_arg: a pointer to the first mp_limb_t
   - size_arg: the number of mp-limb_t
   - sign_arg: the sign of the number 
   if arg is an int, it is converted to a 1-limb number
*/
#define Z_DECL(arg)                                                     \
  mp_limb_t loc_##arg, *ptr_##arg;                                      \
  mp_size_t size_##arg;                                                 \
  intnat sign_##arg;                                                    \
  MAYBE_UNUSED loc_##arg;                                                      \
  MAYBE_UNUSED ptr_##arg;                                                      \
  MAYBE_UNUSED size_##arg;                                                     \
  MAYBE_UNUSED sign_##arg;

#define Z_ARG(arg)                                                      \
  if (Is_long(arg)) {                                                   \
    intnat n = Long_val(arg);                                           \
    loc_##arg = n < 0 ? -n : n;                                         \
    sign_##arg = n & Z_SIGN_MASK;                                       \
    size_##arg = n != 0;                                                \
    ptr_##arg = &loc_##arg;                                             \
  }                                                                     \
  else {                                                                \
    size_##arg = Z_SIZE(arg);                                           \
    sign_##arg = Z_SIGN(arg);                                           \
    ptr_##arg = Z_LIMB(arg);                                            \
  }

/* After an allocation, a heap-allocated Z argument may have moved and
  its ptr_arg pointer can be invalid.  Reset the ptr_arg pointer to
  its correct value. */

#define Z_REFRESH(arg) \
  if (! Is_long(arg)) ptr_##arg = Z_LIMB(arg);

/* computes the actual size of the z object r and updates its header,
   either returns r or, if the number is small enough, an int
 */
static value ml_z_reduce(value r, mp_size_t sz, intnat sign)
{
  while (sz > 0 && !Z_LIMB(r)[sz-1]) sz--;
#if Z_USE_NATINT
  if (!sz) return Val_long(0);
  if (sz <= 1 && Z_LIMB(r)[0] <= Z_MAX_INT) {
    if (sign) return Val_long(-Z_LIMB(r)[0]); 
    else return Val_long(Z_LIMB(r)[0]);
  }
#else
  if (!sz) sign = 0;
#endif
  Z_HEAD(r) = sz | sign;
  return r;
}

static void ml_z_raise_overflow()
{
  caml_raise_constant(*caml_named_value("ml_z_overflow"));
}

#define ml_z_raise_divide_by_zero() \
  caml_raise_zero_divide()



/*---------------------------------------------------
  CONVERSION FUNCTIONS
  ---------------------------------------------------*/

CAMLprim value ml_z_of_int(value v)
{
#if Z_USE_NATINT  
  Z_MARK_OP;
  return v;
#else
  intnat x;
  value r;
  Z_MARK_OP;
  Z_MARK_SLOW;
  x = Long_val(v);
  r =  ml_z_alloc(1);
  if (x > 0) { Z_HEAD(r) = 1; Z_LIMB(r)[0] = x; }
  else if (x < 0) { Z_HEAD(r) = 1 | Z_SIGN_MASK; Z_LIMB(r)[0] = -x; }
  else Z_HEAD(r) = 0;
  Z_CHECK(r);
  return r;
#endif
}

CAMLprim value ml_z_of_nativeint(value v)
{
  intnat x;
  value r;
  Z_MARK_OP;
  x = Nativeint_val(v);
#if Z_USE_NATINT  
  if (Z_FITS_INT(x)) return Val_long(x);
#endif
  Z_MARK_SLOW;
  r = ml_z_alloc(1);
  if (x > 0) { Z_HEAD(r) = 1; Z_LIMB(r)[0] = x; }
  else if (x < 0) { Z_HEAD(r) = 1 | Z_SIGN_MASK; Z_LIMB(r)[0] = -x; }
  else Z_HEAD(r) = 0;
  Z_CHECK(r);
  return r;
}

CAMLprim value ml_z_of_int32(value v)
{
  int32_t x;
  Z_MARK_OP;
  x = Int32_val(v);
#if Z_USE_NATINT && defined(ARCH_SIXTYFOUR)
  return Val_long(x);
#else
#if Z_USE_NATINT
  if (Z_FITS_INT(x)) return Val_long(x);
#endif
  {
    value r;
    Z_MARK_SLOW;
    r = ml_z_alloc(1);
    if (x > 0) { Z_HEAD(r) = 1; Z_LIMB(r)[0] = x; }
    else if (x < 0) { Z_HEAD(r) = 1 | Z_SIGN_MASK; Z_LIMB(r)[0] = -(mp_limb_t)x; }
    else Z_HEAD(r) = 0;
    Z_CHECK(r);
    return r;
  }
#endif
}

CAMLprim value ml_z_of_int64(value v)
{
  int64_t x;
  value r;
  Z_MARK_OP;
  x = Int64_val(v);
#if Z_USE_NATINT
  if (Z_FITS_INT(x)) return Val_long(x);
#endif
  Z_MARK_SLOW;
#ifdef ARCH_SIXTYFOUR
  r = ml_z_alloc(1);
  if (x > 0) { Z_HEAD(r) = 1; Z_LIMB(r)[0] = x; }
  else if (x < 0) { Z_HEAD(r) = 1 | Z_SIGN_MASK; Z_LIMB(r)[0] = -x; }
  else Z_HEAD(r) = 0;
#else
  {
  mp_limb_t sign;
  r = ml_z_alloc(2);
  if (x >= 0) { sign = 0; }
  else { sign = Z_SIGN_MASK; x = -x; }
  Z_LIMB(r)[0] = x;
  Z_LIMB(r)[1] = x >> 32;
  r = ml_z_reduce(r, 2, sign);
  }
#endif
  Z_CHECK(r);
  return r;
}

CAMLprim value ml_z_of_float(value v)
{
  double x;
  int exp;
  int64_t y, m;
  value r;
  Z_MARK_OP;
  x = Double_val(v);
#if Z_USE_NATINT
  if (x >= Z_MIN_INT_FL && x <= Z_MAX_INT_FL) return Val_long((intnat) x);
#endif
  Z_MARK_SLOW;
#ifdef ARCH_ALIGN_INT64
  memcpy(&y, (void *) v, 8);
#else
  y = *((int64_t*)v);
#endif
  exp = ((y >> 52) & 0x7ff) - 1023; /* exponent */
  if (exp < 0) return(Val_long(0));
  if (exp == 1024) ml_z_raise_overflow();  /* NaN or infinity */
  m = (y & 0x000fffffffffffffLL) | 0x0010000000000000LL; /* mantissa */
  if (exp <= 52) {
    m >>= 52-exp;
#ifdef ARCH_SIXTYFOUR
    r = Val_long((x >= 0.) ? m : -m);
#else
    r = ml_z_alloc(2);
    Z_LIMB(r)[0] = m;
    Z_LIMB(r)[1] = m >> 32;
    r = ml_z_reduce(r, 2, (x >= 0.) ? 0 : Z_SIGN_MASK);
#endif
  }
  else {
    int c1 = (exp-52) / Z_LIMB_BITS;
    int c2 = (exp-52) % Z_LIMB_BITS;
    mp_size_t i;
#ifdef ARCH_SIXTYFOUR
    r = ml_z_alloc(c1 + 2);
    for (i = 0; i < c1; i++) Z_LIMB(r)[i] = 0;
    Z_LIMB(r)[c1] = m << c2;
    Z_LIMB(r)[c1+1] = c2 ? (m >> (64-c2)) : 0;
    r = ml_z_reduce(r, c1 + 2, (x >= 0.) ? 0 : Z_SIGN_MASK);
#else
    r = ml_z_alloc(c1 + 3);
    for (i = 0; i < c1; i++) Z_LIMB(r)[i] = 0;
    Z_LIMB(r)[c1] = m << c2;
    Z_LIMB(r)[c1+1] = m >> (32-c2);
    Z_LIMB(r)[c1+2] = c2 ? (m >> (64-c2)) : 0;
    r = ml_z_reduce(r, c1 + 3, (x >= 0.) ? 0 : Z_SIGN_MASK);
#endif
  }
  Z_CHECK(r);
  return r;
}

CAMLprim value ml_z_of_substring_base(value b, value v, value offset, value length)
{
  CAMLparam1(v);
  CAMLlocal1(r);
  intnat ofs = Long_val(offset);
  intnat len = Long_val(length);
  /* make sure the ofs/length make sense */
  if (ofs < 0
      || len < 0
      || (intnat)caml_string_length(v) < ofs + len)
    caml_invalid_argument("Z.of_substring_base: invalid offset or length");
  /* process the string */
  char *d = String_val(v) + ofs;
  char *end = d + len;  
  mp_size_t i, sz, sz2;
  mp_limb_t sign = 0;
  intnat base = Long_val(b);
  /* We allow [d] to advance beyond [end] while parsing the prefix: 
     sign, base, and/or leading zeros.  
     This simplifies the code, and reading these locations is safe since
     we don't progress beyond a terminating null character. 
     At the end of the prefix, if we ran past the end, we return 0. 
  */
  /* get optional sign */
  if (*d == '-') { sign ^= Z_SIGN_MASK; d++; }
  if (*d == '+') d++;
  /* get optional base */
  if (!base) {
    base = 10;
    if (*d == '0') {
      d++;
      if (*d == 'o' || *d == 'O') { base = 8; d++; }
      else if (*d == 'x' || *d == 'X') { base = 16; d++; }
      else if (*d == 'b' || *d == 'B') { base = 2; d++; }
    }
  }
  if (base < 2 || base > 16) 
    caml_invalid_argument("Z.of_substring_base: base must be between 2 and 16");
  while (*d == '0') d++;
  /* sz is the length of the substring that has not been consumed above. */
  sz = end - d;
#if Z_USE_NATINT
  if (sz <= 0) {
    /* "+", "-", "0x" are parsed as 0. */
    r = Val_long(0);
  }
  /* Process common case (fits into a native integer) */
  else if ((base == 10 && sz <= Z_BASE10_LENGTH_OP)
        || (base == 16 && sz <= Z_BASE16_LENGTH_OP)
        || (base == 8  && sz <= Z_BASE8_LENGTH_OP)
        || (base == 2  && sz <= Z_BASE2_LENGTH_OP)) {
      Z_MARK_OP;
      intnat ret = 0;
      for (i = 0; i < sz; i++) {
        int digit = 0;
        if (d[i] >= '0' && d[i] <= '9') digit = d[i] - '0';
        else if (d[i] >= 'a' && d[i] <= 'f') digit = d[i] - 'a' + 10;
        else if (d[i] >= 'A' && d[i] <= 'F') digit = d[i] - 'A' + 10;
        else caml_invalid_argument("Z.of_substring_base: invalid digit");
        if (digit >= base) 
          caml_invalid_argument("Z.of_substring_base: invalid digit");
        ret = ret * base + digit;
      }
      r = Val_long(ret * (sign ? -1 : 1));
  } else
#endif
  {
     /* converts to sequence of digits */
    char* dd = (char*)malloc(sz+1);
    strncpy(dd,d,sz);
    /* make sure that dd is nul terminated */
    dd[sz] = 0;
    for (i = 0; i < sz; i++) {
      if (dd[i] >= '0' && dd[i] <= '9') dd[i] -= '0';
      else if (dd[i] >= 'a' && dd[i] <= 'f') dd[i] -= 'a' - 10;
      else if (dd[i] >= 'A' && dd[i] <= 'F') dd[i] -= 'A' - 10;
      else caml_invalid_argument("Z.of_substring_base: invalid digit");
      if (dd[i] >= base) 
        caml_invalid_argument("Z.of_substring_base: invalid digit");
    }
    r = ml_z_alloc(1 + sz / (2 * sizeof(mp_limb_t)));
    sz2 = mpn_set_str(Z_LIMB(r), (unsigned char*)dd, sz, base);
    r = ml_z_reduce(r, sz2, sign);
    free(dd);
  }
  Z_CHECK(r);
  CAMLreturn(r);
}

CAMLprim value ml_z_to_int(value v)
{
  intnat x;
  Z_DECL(v);
  Z_MARK_OP;
  Z_CHECK(v);
  if (Is_long(v)) return v;
  Z_MARK_SLOW;
  Z_ARG(v);
  if (size_v > 1) ml_z_raise_overflow();
  if (!size_v) return Val_long(0);
  x = *ptr_v;
  if (sign_v) {
    if ((uintnat)x > Z_HI_INT) ml_z_raise_overflow();
    x = -x;
  }
  else {
    if ((uintnat)x >= Z_HI_INT) ml_z_raise_overflow();
  }
  return Val_long(x);
}

CAMLprim value ml_z_to_nativeint(value v)
{
  intnat x;
  Z_DECL(v);
  Z_MARK_OP;
  Z_CHECK(v);
  if (Is_long(v)) return caml_copy_nativeint(Long_val(v));
  Z_MARK_SLOW;
  Z_ARG(v);
  if (size_v > 1) ml_z_raise_overflow();
  if (!size_v) x = 0; 
  else {
    x = *ptr_v;
    if (sign_v) {
      if ((uintnat)x > Z_HI_INTNAT) ml_z_raise_overflow();
      x = -x;
    }
    else {
      if ((uintnat)x >= Z_HI_INTNAT) ml_z_raise_overflow();
    }
  }
  return caml_copy_nativeint(x);
}

CAMLprim value ml_z_to_int32(value v)
{
  intnat x;
  Z_DECL(v);
  Z_MARK_OP;
  Z_CHECK(v);
  if (Is_long(v)) {
    x = Long_val(v);
#ifdef ARCH_SIXTYFOUR
    if (x >= (intnat)Z_HI_INT32 || x < -(intnat)Z_HI_INT32) 
      ml_z_raise_overflow();
#endif
    return caml_copy_int32(x);
  }
  else {
    Z_ARG(v);
    Z_MARK_SLOW;
    if (size_v > 1) ml_z_raise_overflow();
    if (!size_v) x = 0; 
    else {
      x = *ptr_v;
      if (sign_v) {
        if ((uintnat)x > Z_HI_INT32) ml_z_raise_overflow();
        x = -x;
      }
      else {
        if ((uintnat)x >= Z_HI_INT32) ml_z_raise_overflow();
      }
    }
    return caml_copy_int32(x);
  }
}

CAMLprim value ml_z_to_int64(value v)
{
  int64_t x = 0;
  Z_DECL(v);
  Z_MARK_OP;
  Z_CHECK(v);  
  if (Is_long(v)) return caml_copy_int64(Long_val(v));
  Z_MARK_SLOW;
  Z_ARG(v);
  switch (size_v) {
  case 0: x = 0; break;
  case 1: x = ptr_v[0]; break;
#ifndef ARCH_SIXTYFOUR
  case 2: x = ptr_v[0] | ((uint64_t)ptr_v[1] << 32); break;
#endif
  default: ml_z_raise_overflow(); break;
  }
  if (sign_v) {
    if ((uint64_t)x > Z_HI_INT64) ml_z_raise_overflow();
    x = -x;
  }
  else {
    if ((uint64_t)x >= Z_HI_INT64) ml_z_raise_overflow();
  }
  return caml_copy_int64(x);
}

/* XXX: characters that do not belong to the format are ignored, this departs
   from the classic printf behavior (it copies them in the output)
 */
CAMLprim value ml_z_format(value f, value v)
{
  CAMLparam2(f,v);
  Z_DECL(v);
  const char tab[2][16] = 
    { { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' },
      { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' } };
  char* buf, *dst;
  mp_size_t i, size_dst, max_size;
  value r;
  char* fmt = String_val(f);
  int base = 10;     /* base */
  int cas = 0;       /* uppercase X / lowercase x */
  int width = 0;
  int alt = 0;       /* alternate # */
  int dir = 0;       /* right / left adjusted */
  char sign = 0;     /* sign char */
  char pad = ' ';    /* padding char */
  char *prefix = "";
  Z_MARK_OP;
  Z_CHECK(v);
  Z_ARG(v);
  Z_MARK_SLOW;

  /* parse format */
  while (*fmt == '%') fmt++;
  for (; ; fmt++) {
    if (*fmt == '#') alt = 1;
    else if (*fmt == '0') pad = '0';
    else if (*fmt == '-') dir = 1;
    else if (*fmt == ' ' || *fmt == '+') sign = *fmt;
    else break;
  }
  if (sign_v) sign = '-';
  for (;*fmt>='0' && *fmt<='9';fmt++) 
    width = 10*width + *fmt-'0';
  switch (*fmt) {
  case 'i': case 'd': case 'u': break;
  case 'b': base = 2; if (alt) prefix = "0b"; break;
  case 'o': base = 8; if (alt) prefix = "0o"; break;
  case 'x': base = 16; if (alt) prefix = "0x"; cas = 1; break;
  case 'X': base = 16; if (alt) prefix = "0X"; break;
  default: caml_invalid_argument("Z.format: invalid format");
  }
  if (dir) pad = ' ';
  /* get digits */
  /*  we need space for sign + prefix + digits + 1 + padding + terminal 0 */
  max_size = 1 + 2 + Z_LIMB_BITS * size_v + 1 + 2 * width + 1;
  buf = (char*) malloc(max_size);
  dst = buf + 1 + 2 + width;
  if (!size_v) {
    size_dst = 1;
    *dst = '0';
  }
  else {
    mp_limb_t* copy_v = ml_z_dup_limb(ptr_v, size_v);
    size_dst = mpn_get_str((unsigned char*)dst, base, copy_v, size_v);
    if (dst + size_dst >= buf + max_size)
      caml_failwith("Z.format: internal error");
    free(copy_v);
    while (size_dst && !*dst) { dst++; size_dst--; }
    for (i = 0; i < size_dst; i++)
      dst[i] = tab[cas][ (int) dst[i] ];
  }
  /* add prefix, sign & padding */
  if (pad == ' ') {
    if (dir) {
      /* left alignment */
      for (i = strlen(prefix); i > 0; i--, size_dst++) 
        *(--dst) = prefix[i-1];
      if (sign) { *(--dst) = sign; size_dst++; }
      for (; size_dst < width; size_dst++) 
        dst[size_dst] = pad;
    }
    else {
      /* right alignment, space padding */
      for (i = strlen(prefix); i > 0; i--, size_dst++) 
        *(--dst) = prefix[i-1];
      if (sign) { *(--dst) = sign; size_dst++; }
      for (; size_dst < width; size_dst++) *(--dst) = pad;
    }
  }
  else {
    /* right alignment, non-space padding */
    width -= strlen(prefix) + (sign ? 1 : 0);
    for (; size_dst < width; size_dst++) *(--dst) = pad;
    for (i = strlen(prefix); i > 0; i--, size_dst++) 
      *(--dst) = prefix[i-1];
    if (sign) { *(--dst) = sign; size_dst++; }
  }
  dst[size_dst] = 0;
  if (dst < buf || dst + size_dst >= buf + max_size)
    caml_failwith("Z.format: internal error");
  r = caml_copy_string(dst);
  free(buf);
  CAMLreturn(r);
}

#ifdef ARCH_SIXTYFOUR
#define BITS_PER_WORD 64
#else
#define BITS_PER_WORD 32
#endif

CAMLprim value ml_z_extract(value arg, value off, value len)
{
  intnat o, l, x;
  mp_size_t sz, c1, c2, csz, i;
  mp_limb_t cr;
  value r;
  Z_DECL(arg);
  Z_MARK_OP;
  MAYBE_UNUSED x;
  o = Long_val(off);
  l = Long_val(len);
  if (o < 0) caml_invalid_argument("Z.extract: negative bit offset");
  if (l <= 0) caml_invalid_argument("Z.extract: non-positive bit length");
#if Z_USE_NATINT
  /* Fast path */
  if (Is_long(arg)) {
    x = Long_val(arg);
    /* Shift away low "o" bits.  If "o" too big, just replicate sign bit. */
    if (o >= BITS_PER_WORD) o = BITS_PER_WORD - 1;
    x = x >> o;
    /* Extract "l" low bits, if "l" is small enough */
    if (l < BITS_PER_WORD - 1) {
      x = x & (((intnat)1 << l) - 1);
      return Val_long(x);
    } else {
      /* If x >= 0, the extraction of "l" low bits keeps x unchanged. */
      if (x >= 0) return Val_long(x);
      /* If x < 0, fall through slow path */
    }
  }
#endif
  Z_MARK_SLOW;
  {
  CAMLparam1(arg);
  Z_ARG(arg);
  sz = (l + Z_LIMB_BITS - 1) / Z_LIMB_BITS;
  r = ml_z_alloc(sz + 1);
  Z_REFRESH(arg);
  c1 = o / Z_LIMB_BITS;
  c2 = o % Z_LIMB_BITS;
  /* shift or copy */
  csz = size_arg - c1;
  if (csz > sz + 1) csz = sz + 1;
  cr = 0;
  if (csz > 0) {
    if (c2) cr = mpn_rshift(Z_LIMB(r), ptr_arg + c1, csz, c2);
    else ml_z_cpy_limb(Z_LIMB(r), ptr_arg + c1, csz);
  }
  else csz = 0;
  /* 0-pad */
  for (i = csz; i < sz; i++)
    Z_LIMB(r)[i] = 0;
  /* 2's complement */
  if (sign_arg) {
    for (i = 0; i < sz; i++) 
      Z_LIMB(r)[i] = ~Z_LIMB(r)[i];
    /* carry (cr=0 if all shifted-out bits are 0) */
    for (i = 0; !cr && i < c1 && i < size_arg; i++)
      cr = ptr_arg[i];
    if (!cr) mpn_add_1(Z_LIMB(r), Z_LIMB(r), sz, 1);
  }
  /* mask out high bits */
  l %= Z_LIMB_BITS;
  if (l) Z_LIMB(r)[sz-1] &= ((uintnat)(intnat)-1) >> (Z_LIMB_BITS - l);
  r = ml_z_reduce(r, sz, 0);
  CAMLreturn(r);
  }
}

/* NOTE: the sign is not stored */
CAMLprim value ml_z_to_bits(value arg)
{
  CAMLparam1(arg);
  CAMLlocal1(r);
  Z_DECL(arg);
  mp_size_t i;
  unsigned char* p;
  Z_MARK_OP;
  Z_MARK_SLOW;
  Z_ARG(arg);
  r = caml_alloc_string(size_arg * sizeof(mp_limb_t));
  Z_REFRESH(arg);
  p = (unsigned char*) String_val(r);
  memset(p, 0, size_arg * sizeof(mp_limb_t));
  for (i = 0; i < size_arg; i++) {
    mp_limb_t x = ptr_arg[i];
    *(p++) = x;
    *(p++) = x >> 8;
    *(p++) = x >> 16;
    *(p++) = x >> 24;
#ifdef ARCH_SIXTYFOUR
    *(p++) = x >> 32;
    *(p++) = x >> 40;
    *(p++) = x >> 48;
    *(p++) = x >> 56;
#endif
  }
  CAMLreturn(r);
}

CAMLprim value ml_z_of_bits(value arg)
{
  CAMLparam1(arg);
  CAMLlocal1(r);
  mp_size_t sz, szw;
  mp_size_t i = 0;
  mp_limb_t x;
  unsigned char* p;
  Z_MARK_OP;
  Z_MARK_SLOW;
  sz = caml_string_length(arg);
  szw = (sz + sizeof(mp_limb_t) - 1) / sizeof(mp_limb_t);
  r = ml_z_alloc(szw);
  p = (unsigned char*) String_val(arg);
  /* all limbs but last */
  if (szw > 1) {
    for (; i < szw - 1; i++) {
      x = *(p++);
      x |= ((mp_limb_t) *(p++)) << 8;
      x |= ((mp_limb_t) *(p++)) << 16;
      x |= ((mp_limb_t) *(p++)) << 24;
#ifdef ARCH_SIXTYFOUR
      x |= ((mp_limb_t) *(p++)) << 32;
      x |= ((mp_limb_t) *(p++)) << 40;
      x |= ((mp_limb_t) *(p++)) << 48;
      x |= ((mp_limb_t) *(p++)) << 56;
#endif
      Z_LIMB(r)[i] = x;
    }
    sz -= i * sizeof(mp_limb_t);
  }
  /* last limb */
  if (sz > 0) {
    x = *(p++);
    if (sz > 1) x |= ((mp_limb_t) *(p++)) << 8;
    if (sz > 2) x |= ((mp_limb_t) *(p++)) << 16;
    if (sz > 3) x |= ((mp_limb_t) *(p++)) << 24;
#ifdef ARCH_SIXTYFOUR
    if (sz > 4) x |= ((mp_limb_t) *(p++)) << 32;
    if (sz > 5) x |= ((mp_limb_t) *(p++)) << 40;
    if (sz > 6) x |= ((mp_limb_t) *(p++)) << 48;
    if (sz > 7) x |= ((mp_limb_t) *(p++)) << 56;
#endif
    Z_LIMB(r)[i] = x;
  }
  r = ml_z_reduce(r, szw, 0);
  Z_CHECK(r);
  CAMLreturn(r);
}

/*---------------------------------------------------
  TESTS AND COMPARISONS
  ---------------------------------------------------*/

CAMLprim value ml_z_compare(value arg1, value arg2)
{
  int r;
  Z_DECL(arg1); Z_DECL(arg2);
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    if (arg1 > arg2) return Val_long(1);
    else if (arg1 < arg2) return Val_long(-1);
    else return Val_long(0);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  Z_ARG(arg1);
  Z_ARG(arg2);
  r = 0;
  if (sign_arg1 != sign_arg2) r = 1;
  else if (size_arg1 > size_arg2) r = 1;
  else if (size_arg1 < size_arg2) r = -1;
  else {
    mp_size_t i;
    for (i = size_arg1 - 1; i >= 0; i--) {
      if (ptr_arg1[i] > ptr_arg2[i]) { r = 1; break; }
      if (ptr_arg1[i] < ptr_arg2[i]) { r = -1; break; }
    }
  }
  if (sign_arg1) r = -r;
  return Val_long(r);
}

CAMLprim value ml_z_equal(value arg1, value arg2)
{
  mp_size_t i;
  Z_DECL(arg1); Z_DECL(arg2);
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    return (arg1 == arg2) ? Val_true : Val_false;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  Z_ARG(arg1);
  Z_ARG(arg2);
  if (sign_arg1 != sign_arg2 || size_arg1 != size_arg2) return Val_false;
  for (i = 0; i < size_arg1; i++)
    if (ptr_arg1[i] != ptr_arg2[i]) return Val_false;
  return Val_true;
}

int ml_z_sgn(value arg)
{
  if (Is_long(arg)) {
    if (arg > Val_long(0)) return 1;
    else if (arg < Val_long(0)) return -1;
    else return 0;
  }
  else {
    Z_MARK_SLOW;
    if (!Z_SIZE(arg)) return 0;
    else if (Z_SIGN(arg)) return -1;
    else return 1;
  }
}
CAMLprim value ml_z_sign(value arg)
{
  Z_MARK_OP;
  Z_CHECK(arg);
  return Val_long(ml_z_sgn(arg));
}

CAMLprim value ml_z_fits_int(value v)
{
  intnat x;
  Z_DECL(v);
  Z_MARK_OP;
  Z_CHECK(v);
  if (Is_long(v)) return Val_true;
  Z_MARK_SLOW;
  Z_ARG(v);
  if (size_v > 1) return Val_false;
  if (!size_v) return Val_true;
  x = *ptr_v;
  if (sign_v) {
    if ((uintnat)x > Z_HI_INT) return Val_false;
  }
  else {
    if ((uintnat)x >= Z_HI_INT) return Val_false;
  }
  return Val_true;
}

CAMLprim value ml_z_fits_nativeint(value v)
{
  intnat x;
  Z_DECL(v);
  Z_MARK_OP;
  Z_CHECK(v);
  if (Is_long(v)) return Val_true;
  Z_MARK_SLOW;
  Z_ARG(v);
  if (size_v > 1) return Val_false;
  if (!size_v) return Val_true;
  x = *ptr_v;
  if (sign_v) {
    if ((uintnat)x > Z_HI_INTNAT) return Val_false;
  }
  else {
    if ((uintnat)x >= Z_HI_INTNAT) return Val_false;
  }
  return Val_true;
}

CAMLprim value ml_z_fits_int32(value v)
{
  intnat x;
  Z_MARK_OP;
  Z_CHECK(v);
  if (Is_long(v)) {
#ifdef ARCH_SIXTYFOUR
    x = Long_val(v);
    if (x >= (intnat)Z_HI_INT32 || x < -(intnat)Z_HI_INT32) 
      return Val_false;
#endif
    return Val_true;
  }
  else {
    Z_DECL(v);
    Z_MARK_SLOW;
    Z_ARG(v);
    if (size_v > 1) return Val_false;
    if (!size_v) return Val_true;
    x = *ptr_v;
    if (sign_v) {
      if ((uintnat)x > Z_HI_INT32) return Val_false;
    }
    else {
      if ((uintnat)x >= Z_HI_INT32) return Val_false;
    }
    return Val_true;
  }
}

CAMLprim value ml_z_fits_int64(value v)
{
  int64_t x;
  Z_DECL(v);
  Z_MARK_OP;
  Z_CHECK(v);  
  if (Is_long(v)) return Val_true;
  Z_MARK_SLOW;
  Z_ARG(v);
  switch (size_v) {
  case 0: return Val_true;
  case 1: x = ptr_v[0]; break;
#ifndef ARCH_SIXTYFOUR
  case 2: x = ptr_v[0] | ((uint64_t)ptr_v[1] << 32); break;
#endif
  default: return Val_false;
  }
  if (sign_v) {
    if ((uint64_t)x > Z_HI_INT64) return Val_false;
  }
  else {
    if ((uint64_t)x >= Z_HI_INT64) return Val_false;
  }
  return Val_true;
}

CAMLprim value ml_z_size(value v)
{
  Z_MARK_OP;
  if (Is_long(v)) return Val_long(1);
  else return Val_long(Z_SIZE(v));
}


/*---------------------------------------------------
  ARITHMETIC OPERATORS
  ---------------------------------------------------*/

CAMLprim value ml_z_neg(value arg)
{
  Z_MARK_OP;
  Z_CHECK(arg);
#if Z_FAST_PATH && !defined(Z_ASM_neg)
  if (Is_long(arg)) {
    /* fast path */
    if (arg > Val_long(Z_MIN_INT)) return 2 - arg;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam1(arg);
    value r;
    Z_DECL(arg);
    Z_ARG(arg);
    r = ml_z_alloc(size_arg);
    Z_REFRESH(arg);
    ml_z_cpy_limb(Z_LIMB(r), ptr_arg, size_arg);
    r = ml_z_reduce(r, size_arg, sign_arg ^ Z_SIGN_MASK);
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_abs(value arg)
{
  Z_MARK_OP;
  Z_CHECK(arg);
#if Z_FAST_PATH && !defined(Z_ASM_abs)
  if (Is_long(arg)) {
    /* fast path */
    if (arg >= Val_long(0)) return arg;
    if (arg > Val_long(Z_MIN_INT)) return 2 - arg;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam1(arg);
    Z_DECL(arg);
    value r;
    Z_ARG(arg);
    r = ml_z_alloc(size_arg);
    Z_REFRESH(arg);
    ml_z_cpy_limb(Z_LIMB(r), ptr_arg, size_arg);
    r = ml_z_reduce(r, size_arg, 0);
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

/* helper function for add/sub */
static value ml_z_addsub(value arg1, value arg2, intnat sign)
{
  CAMLparam2(arg1,arg2);
  Z_DECL(arg1); Z_DECL(arg2);
  value r;
  mp_limb_t c;
  Z_ARG(arg1);
  Z_ARG(arg2);
  sign_arg2 ^= sign;
  if (!size_arg2) r = arg1;
  else if (!size_arg1) {
    if (sign) {
      /* negation */
      r = ml_z_alloc(size_arg2);
      Z_REFRESH(arg2);
      ml_z_cpy_limb(Z_LIMB(r), ptr_arg2, size_arg2);
      r = ml_z_reduce(r, size_arg2, sign_arg2);
    }
    else r = arg2;
  }
  else if (sign_arg1 == sign_arg2) {
    /* addition */
    if (size_arg1 >= size_arg2) {
      r = ml_z_alloc(size_arg1 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      c = mpn_add(Z_LIMB(r), ptr_arg1, size_arg1, ptr_arg2, size_arg2);
      Z_LIMB(r)[size_arg1] = c;
      r = ml_z_reduce(r, size_arg1+1, sign_arg1);
    }
    else {
      r = ml_z_alloc(size_arg2 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      c = mpn_add(Z_LIMB(r), ptr_arg2, size_arg2, ptr_arg1, size_arg1);
      Z_LIMB(r)[size_arg2] = c;
      r = ml_z_reduce(r, size_arg2+1, sign_arg1);
    }  
  }
  else {
    /* subtraction */
    if (size_arg1 > size_arg2) {
      r = ml_z_alloc(size_arg1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub(Z_LIMB(r), ptr_arg1, size_arg1, ptr_arg2, size_arg2);
      r = ml_z_reduce(r, size_arg1, sign_arg1);
   }
    else if (size_arg1 < size_arg2) {
      r = ml_z_alloc(size_arg2);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub(Z_LIMB(r), ptr_arg2, size_arg2, ptr_arg1, size_arg1);
      r = ml_z_reduce(r, size_arg2, sign_arg2);
    }
    else {
      int cmp = mpn_cmp(ptr_arg1, ptr_arg2, size_arg1);
      if (cmp > 0) {
        r = ml_z_alloc(size_arg1+1);
        Z_REFRESH(arg1);
        Z_REFRESH(arg2);
        mpn_sub_n(Z_LIMB(r), ptr_arg1, ptr_arg2, size_arg1);
        r = ml_z_reduce(r, size_arg1, sign_arg1);
     }
      else if (cmp < 0) {
        r = ml_z_alloc(size_arg1);
        Z_REFRESH(arg1);
        Z_REFRESH(arg2);
        mpn_sub_n(Z_LIMB(r), ptr_arg2, ptr_arg1, size_arg1);
        r = ml_z_reduce(r, size_arg1, sign_arg2);
      }
      else r = Val_long(0);
    }
  }
  Z_CHECK(r);
  CAMLreturn(r);
}

CAMLprim value ml_z_add(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH && !defined(Z_ASM_add)
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat v = a1 + a2;
    if (Z_FITS_INT(v)) return Val_long(v);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return ml_z_addsub(arg1, arg2, 0);
}

CAMLprim value ml_z_sub(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH && !defined(Z_ASM_sub)
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat v = a1 - a2;
    if (Z_FITS_INT(v)) return Val_long(v);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return ml_z_addsub(arg1, arg2, Z_SIGN_MASK);
}

CAMLprim value ml_z_mul(value arg1, value arg2)
{
  Z_DECL(arg1); Z_DECL(arg2);
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH && !defined(Z_ASM_mul)
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    if (!a1 || !a2) return Val_long(0);
    /* small argument case */
    if (Z_FITS_HINT(arg1) && Z_FITS_HINT(arg2)) return Val_long(a1 * a2);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  Z_ARG(arg1);
  Z_ARG(arg2);
  if (!size_arg1 || !size_arg2) return Val_long(0);
  {
    CAMLparam2(arg1,arg2);
    value r = ml_z_alloc(size_arg1 + size_arg2);
    mp_limb_t c;
    Z_REFRESH(arg1);
    Z_REFRESH(arg2);
    if (size_arg2 == 1) {
      c = mpn_mul_1(Z_LIMB(r), ptr_arg1, size_arg1, *ptr_arg2);
      Z_LIMB(r)[size_arg1] = c;
    }
    else if (size_arg1 == 1) {
      c = mpn_mul_1(Z_LIMB(r), ptr_arg2, size_arg2, *ptr_arg1);
      Z_LIMB(r)[size_arg2] = c;
    }
#if HAVE_NATIVE_mpn_mul_2 /* untested */
    else if (size_arg2 == 2) {
      c = mpn_mul_2(Z_LIMB(r), ptr_arg1, size_arg1, ptr_arg2);
      Z_LIMB(r)[size_arg1 + 1] = c;
    }
    else if (size_arg1 == 2) {
      c = mpn_mul_2(Z_LIMB(r), ptr_arg2, size_arg2, ptr_arg1);
      Z_LIMB(r)[size_arg2 + 1] = c;
    }
#endif
    else if (size_arg1 > size_arg2)
      mpn_mul(Z_LIMB(r), ptr_arg1, size_arg1, ptr_arg2, size_arg2);
    else if (size_arg1 < size_arg2)
      mpn_mul(Z_LIMB(r), ptr_arg2, size_arg2, ptr_arg1, size_arg1);
/* older GMP don't have mpn_sqr, so we make the optimisation optional */
#ifdef mpn_sqr
    else if (ptr_arg1 == ptr_arg2)
      mpn_sqr(Z_LIMB(r), ptr_arg1, size_arg1);
#endif
    else
      mpn_mul_n(Z_LIMB(r), ptr_arg1, ptr_arg2, size_arg1);
    r = ml_z_reduce(r, size_arg1 + size_arg2, sign_arg1^sign_arg2);
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

/* helper function for division: returns truncated quotient and remainder */
static value ml_z_tdiv_qr(value arg1, value arg2)
{
  CAMLparam2(arg1, arg2);
  CAMLlocal3(q, r, p);
  Z_DECL(arg1); Z_DECL(arg2);
  Z_ARG(arg1);  Z_ARG(arg2);
  if (!size_arg2) ml_z_raise_divide_by_zero();
  if (size_arg1 >= size_arg2) {
    q = ml_z_alloc(size_arg1 - size_arg2 + 1);
    r = ml_z_alloc(size_arg2);
    Z_REFRESH(arg1); Z_REFRESH(arg2);
    mpn_tdiv_qr(Z_LIMB(q), Z_LIMB(r), 0, 
                ptr_arg1, size_arg1, ptr_arg2, size_arg2);
    q = ml_z_reduce(q, size_arg1 - size_arg2 + 1, sign_arg1 ^ sign_arg2);
    r = ml_z_reduce(r, size_arg2, sign_arg1);
  }
  else {
    q = Val_long(0);
    r = arg1;
  }
  Z_CHECK(q);
  Z_CHECK(r);
  p = caml_alloc_small(2, 0);
  Field(p,0) = q;
  Field(p,1) = r;
  CAMLreturn(p);
}

CAMLprim value ml_z_div_rem(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q, r;
    if (!a2) ml_z_raise_divide_by_zero();
    q = a1 / a2;
    r = a1 % a2;
    if (Z_FITS_INT(q) && Z_FITS_INT(r)) {
      value p = caml_alloc_small(2, 0);
      Field(p,0) = Val_long(q);
      Field(p,1) = Val_long(r);
      return p;
    }
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return ml_z_tdiv_qr(arg1, arg2);
}

CAMLprim value ml_z_div(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH && !defined(Z_ASM_div)
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q;
    if (!a2) ml_z_raise_divide_by_zero();
    q = a1 / a2;
    if (Z_FITS_INT(q)) return Val_long(q);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return Field(ml_z_tdiv_qr(arg1, arg2), 0);
}

CAMLprim value ml_z_rem(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH && !defined(Z_ASM_rem)
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat r;
    if (!a2) ml_z_raise_divide_by_zero();
    r = a1 % a2;
    if (Z_FITS_INT(r)) return Val_long(r);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return Field(ml_z_tdiv_qr(arg1, arg2), 1);
}

/* helper function for division with rounding towards +oo / -oo */
static value ml_z_rdiv(value arg1, value arg2, intnat dir)
{
  CAMLparam2(arg1, arg2);
  CAMLlocal2(q, r);
  Z_DECL(arg1); Z_DECL(arg2);
  Z_ARG(arg1);  Z_ARG(arg2);
  if (!size_arg2) ml_z_raise_divide_by_zero();
  if (size_arg1 >= size_arg2) {
    mp_limb_t c = 0;
    q = ml_z_alloc(size_arg1 - size_arg2 + 2);
    r = ml_z_alloc(size_arg2);
    Z_REFRESH(arg1); Z_REFRESH(arg2);
    mpn_tdiv_qr(Z_LIMB(q), Z_LIMB(r), 0, 
                ptr_arg1, size_arg1, ptr_arg2, size_arg2);
    if ((sign_arg1 ^ sign_arg2) == dir) {
      /* outward rounding */
      mp_size_t sz;
      for (sz = size_arg2; sz > 0 && !Z_LIMB(r)[sz-1]; sz--);
      if (sz) {
        /* r != 0: needs adjustment */
        c = mpn_add_1(Z_LIMB(q), Z_LIMB(q), size_arg1 - size_arg2 + 1, 1);
      }
    }
    Z_LIMB(q)[size_arg1 - size_arg2 + 1] = c;
    q = ml_z_reduce(q, size_arg1 - size_arg2 + 2, sign_arg1 ^ sign_arg2);
  }
  else {
    if (size_arg1 && (sign_arg1 ^ sign_arg2) == dir) {
      if (dir) q = Val_long(-1);
      else q = Val_long(1);
    }
    else q = Val_long(0);
  }
  Z_CHECK(q);
  CAMLreturn(q);
}

CAMLprim value ml_z_cdiv(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q;
    if (!a2) ml_z_raise_divide_by_zero();
    /* adjust to round towards +oo */
    if (a1 > 0 && a2 > 0) a1 += a2-1;
    else if (a1 < 0 && a2 < 0) a1 += a2+1;
    q = a1 / a2;
    if (Z_FITS_INT(q)) return Val_long(q);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return ml_z_rdiv(arg1, arg2, 0);
}

CAMLprim value ml_z_fdiv(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q;
    if (!a2) ml_z_raise_divide_by_zero();
    /* adjust to round towards -oo */
    if (a1 < 0 && a2 > 0) a1 -= a2-1;
    else if (a1 > 0 && a2 < 0) a1 -= a2+1;
    q = a1 / a2;
    if (Z_FITS_INT(q)) return Val_long(q);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return ml_z_rdiv(arg1, arg2, Z_SIGN_MASK);
}

/* helper function for succ / pred */
static value ml_z_succpred(value arg, intnat sign)
{
  CAMLparam1(arg);
  Z_DECL(arg);
  value r;
  Z_ARG(arg);
  r = ml_z_alloc(size_arg + 1);
  Z_REFRESH(arg);
  if (!size_arg) {
    Z_LIMB(r)[0] = 1;
    r = ml_z_reduce(r, 1, sign);
  }
  else if (sign_arg == sign) {
    /* add 1 */
    mp_limb_t c = mpn_add_1(Z_LIMB(r), ptr_arg, size_arg, 1);
    Z_LIMB(r)[size_arg] = c;
    r = ml_z_reduce(r, size_arg + 1, sign_arg);
  }
  else {
    /* subtract 1 */
    mpn_sub_1(Z_LIMB(r), ptr_arg, size_arg, 1);
    r = ml_z_reduce(r, size_arg, sign_arg);
  }
  Z_CHECK(r);
  CAMLreturn(r);
}

CAMLprim value ml_z_succ(value arg)
{
  Z_MARK_OP;
  Z_CHECK(arg);
#if Z_FAST_PATH && !defined(Z_ASM_succ)
  if (Is_long(arg)) {
    /* fast path */
    if (arg < Val_long(Z_MAX_INT)) return arg + 2;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return ml_z_succpred(arg, 0);
}

CAMLprim value ml_z_pred(value arg)
{
  Z_MARK_OP;
  Z_CHECK(arg);
#if Z_FAST_PATH && !defined(Z_ASM_pred)
  if (Is_long(arg)) {
    /* fast path */
     if (arg > Val_long(Z_MIN_INT)) return arg - 2;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  return ml_z_succpred(arg, Z_SIGN_MASK);
}

CAMLprim value ml_z_sqrt(value arg)
{
  /* XXX TODO: fast path */
  CAMLparam1(arg);
  Z_DECL(arg);
  value r;
  Z_MARK_OP;
  Z_MARK_SLOW;
  Z_CHECK(arg);
  Z_ARG(arg);
  if (sign_arg) 
    caml_invalid_argument("Z.sqrt: square root of a negative number");
  if (size_arg) {
    mp_size_t sz = (size_arg + 1) / 2;
    r = ml_z_alloc(sz);
    Z_REFRESH(arg);
    mpn_sqrtrem(Z_LIMB(r), NULL, ptr_arg, size_arg);
    r = ml_z_reduce(r, sz, 0);
  }
  else r = Val_long(0);
  Z_CHECK(r);
  CAMLreturn(r);
}

CAMLprim value ml_z_sqrt_rem(value arg)
{
  CAMLparam1(arg);
  CAMLlocal3(r, s, p);
  Z_DECL(arg);
  /* XXX TODO: fast path */
  Z_MARK_OP;
  Z_MARK_SLOW;
  Z_CHECK(arg);
  Z_ARG(arg);
  if (sign_arg) 
    caml_invalid_argument("Z.sqrt_rem: square root of a negative number");
  if (size_arg) {
    mp_size_t sz = (size_arg + 1) / 2, sz2;
    r = ml_z_alloc(sz);
    s = ml_z_alloc(size_arg);
    Z_REFRESH(arg);
    sz2 = mpn_sqrtrem(Z_LIMB(r), Z_LIMB(s), ptr_arg, size_arg);
    r = ml_z_reduce(r, sz, 0);
    s = ml_z_reduce(s, sz2, 0);
  } 
  else r = s = Val_long(0);
  Z_CHECK(r);
  Z_CHECK(s);
  p = caml_alloc_small(2, 0);
  Field(p,0) = r;
  Field(p,1) = s;
  CAMLreturn(p);
}

CAMLprim value ml_z_gcd(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    if (a1 < 0) a1 = -a1;
    if (a2 < 0) a2 = -a2;
    if (a1 < a2) { intnat t = a1; a1 = a2; a2 = t; }
    while (a2)  {
      intnat r = a1 % a2;
      a1 = a2; a2 = r;
    }
    return Val_long(a1);
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam2(arg1, arg2);
    CAMLlocal3(r, tmp1, tmp2);
    mp_size_t sz, pos1, pos2, limb1, limb2, bit1, bit2, pos, limb, bit, i;
    Z_DECL(arg1); Z_DECL(arg2);
    Z_ARG(arg1);  Z_ARG(arg2);
    if (!size_arg1) r = arg2;
    else if (!size_arg2) r = arg1;
    else {
      /* copy args to tmp storage & remove lower 0 bits */
      pos1 = mpn_scan1(ptr_arg1, 0);
      pos2 = mpn_scan1(ptr_arg2, 0);
      limb1 = pos1 / Z_LIMB_BITS;
      limb2 = pos2 / Z_LIMB_BITS;
      bit1 = pos1 % Z_LIMB_BITS;
      bit2 = pos2 % Z_LIMB_BITS;
      size_arg1 -= limb1;
      size_arg2 -= limb2;
      tmp1 = ml_z_alloc(size_arg1 + 1);
      tmp2 = ml_z_alloc(size_arg2 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      if (bit1) {
        mpn_rshift(Z_LIMB(tmp1), ptr_arg1 + limb1, size_arg1, bit1);
        if (!Z_LIMB(tmp1)[size_arg1-1]) size_arg1--;
      }
      else ml_z_cpy_limb(Z_LIMB(tmp1), ptr_arg1 + limb1, size_arg1);
      if (bit2) { 
        mpn_rshift(Z_LIMB(tmp2), ptr_arg2 + limb2, size_arg2, bit2);
        if (!Z_LIMB(tmp2)[size_arg2-1]) size_arg2--;
      }
      else ml_z_cpy_limb(Z_LIMB(tmp2), ptr_arg2 + limb2, size_arg2);
      /* compute gcd of 2^pos1 & 2^pos2 */
      pos = (pos1 <= pos2) ? pos1 : pos2;
      limb = pos / Z_LIMB_BITS;
      bit = pos % Z_LIMB_BITS;
      /* compute gcd of arg1 & arg2 without lower 0 bits */
      /* second argument must have less bits than first  */
      if ((size_arg1 > size_arg2) ||
          ((size_arg1 == size_arg2) && 
           (Z_LIMB(tmp1)[size_arg1 - 1] >= Z_LIMB(tmp2)[size_arg1 - 1]))) {
        r = ml_z_alloc(size_arg2 + limb + 1);
        sz = mpn_gcd(Z_LIMB(r) + limb, Z_LIMB(tmp1), size_arg1, Z_LIMB(tmp2), size_arg2);
      }
      else {
        r = ml_z_alloc(size_arg1 + limb + 1);
        sz = mpn_gcd(Z_LIMB(r) + limb, Z_LIMB(tmp2), size_arg2, Z_LIMB(tmp1), size_arg1);
      } 
      /* glue the two results */
      for (i = 0; i < limb; i++) 
        Z_LIMB(r)[i] = 0;
      Z_LIMB(r)[sz + limb] = 0;
      if (bit) mpn_lshift(Z_LIMB(r) + limb, Z_LIMB(r) + limb, sz + 1, bit);
      r = ml_z_reduce(r, limb + sz + 1, 0);
    }
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

/* only computes one cofactor */
CAMLprim value ml_z_gcdext_intern(value arg1, value arg2)
{
  /* XXX TODO: fast path */
  CAMLparam2(arg1, arg2);
  CAMLlocal5(r, res_arg1, res_arg2, s, p);
  Z_DECL(arg1); Z_DECL(arg2);
  mp_size_t sz, sn;
  Z_MARK_OP;
  Z_MARK_SLOW;
  Z_CHECK(arg1);  Z_CHECK(arg2);
  Z_ARG(arg1);    Z_ARG(arg2);
  if (!size_arg1 || !size_arg2) ml_z_raise_divide_by_zero();
  /* copy args to tmp storage */
  res_arg1 = ml_z_alloc(size_arg1 + 1);
  res_arg2 = ml_z_alloc(size_arg2 + 1);
  Z_REFRESH(arg1);
  Z_REFRESH(arg2);
  ml_z_cpy_limb(Z_LIMB(res_arg1), ptr_arg1, size_arg1);
  ml_z_cpy_limb(Z_LIMB(res_arg2), ptr_arg2, size_arg2);
  /* must have arg1 >= arg2 */
  if ((size_arg1 > size_arg2) ||
      ((size_arg1 == size_arg2) && 
       (mpn_cmp(Z_LIMB(res_arg1), Z_LIMB(res_arg2), size_arg1)  >= 0))) {
    r = ml_z_alloc(size_arg1 + 1);
    s = ml_z_alloc(size_arg1 + 1);
    sz = mpn_gcdext(Z_LIMB(r), Z_LIMB(s), &sn, 
                    Z_LIMB(res_arg1), size_arg1, Z_LIMB(res_arg2), size_arg2);
    p = caml_alloc_small(3, 0);
    Field(p,2) = Val_true;
  }
  else {
    r = ml_z_alloc(size_arg2 + 1);
    s = ml_z_alloc(size_arg2 + 1);
    sz = mpn_gcdext(Z_LIMB(r), Z_LIMB(s), &sn, 
                    Z_LIMB(res_arg2), size_arg2, Z_LIMB(res_arg1), size_arg1);
    p = caml_alloc_small(3, 0);
    Field(p,2) = Val_false;
    sign_arg1 = sign_arg2;
  } 
  /* pack result */
  r = ml_z_reduce(r, sz, 0);
  if ((int)sn >= 0) s = ml_z_reduce(s, sn, sign_arg1);
  else s = ml_z_reduce(s, -sn, sign_arg1 ^ Z_SIGN_MASK);
  Z_CHECK(r);
  Z_CHECK(s);
  Field(p,0) = r;
  Field(p,1) = s;
  CAMLreturn(p);
}


/*---------------------------------------------------
  BITWISE OPERATORS
  ---------------------------------------------------*/

CAMLprim value ml_z_logand(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH && !defined(Z_ASM_logand)
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    return arg1 & arg2;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam2(arg1,arg2);
    value r;
    mp_size_t i;
    mp_limb_t c;
    Z_DECL(arg1); Z_DECL(arg2);
    Z_ARG(arg1);  Z_ARG(arg2);
    /* ensure size_arg1 >= size_arg2 */
    if (size_arg1 < size_arg2) {
      mp_size_t sz;
      mp_limb_t *p, s;
      value a;
      sz = size_arg1; size_arg1 = size_arg2; size_arg2 = sz;
      p = ptr_arg1; ptr_arg1 = ptr_arg2; ptr_arg2 = p;
      s = sign_arg1; sign_arg1 = sign_arg2; sign_arg2 = s;
      a = arg1; arg1 = arg2; arg2 = a;
    }
    if (!size_arg2) r = arg2;
    else if (sign_arg1 && sign_arg2) {
      /* arg1 < 0, arg2 < 0 => r < 0 */
      r = ml_z_alloc(size_arg1 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg1, size_arg1, 1);
      c = 1; /* carry when decrementing arg2 */
      for (i = 0; i < size_arg2; i++) {
        mp_limb_t v = ptr_arg2[i];
        Z_LIMB(r)[i] = Z_LIMB(r)[i] | (v - c);
        c = c && !v;
      }
      c = mpn_add_1(Z_LIMB(r), Z_LIMB(r), size_arg1, 1);
      Z_LIMB(r)[size_arg1] = c;
      r = ml_z_reduce(r, size_arg1 + 1, Z_SIGN_MASK);
    }
    else if (sign_arg1) {
      /* arg1 < 0, arg2 > 0 => r >= 0 */
      r = ml_z_alloc(size_arg2);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg1, size_arg2, 1);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = (~Z_LIMB(r)[i]) & ptr_arg2[i];
      r = ml_z_reduce(r, size_arg2, 0);
    }
    else if (sign_arg2) {
      /* arg1 > 0, arg2 < 0 => r >= 0 */
      r = ml_z_alloc(size_arg1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg2, size_arg2, 1);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i] & (~Z_LIMB(r)[i]);
      for (; i < size_arg1; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i];
      r = ml_z_reduce(r, size_arg1, 0);
    }
    else {
      /* arg1, arg2 > 0 => r >= 0 */
      r = ml_z_alloc(size_arg2);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i] & ptr_arg2[i];
      r = ml_z_reduce(r, size_arg2, 0);
    }
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_logor(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH && !defined(Z_ASM_logor)
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    return arg1 | arg2;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam2(arg1,arg2);
    Z_DECL(arg1); Z_DECL(arg2);
    mp_size_t i;
    mp_limb_t c;
    value r;
    Z_ARG(arg1);  Z_ARG(arg2);
    /* ensure size_arg1 >= size_arg2 */
    if (size_arg1 < size_arg2) {
      mp_size_t sz;
      mp_limb_t *p, s;
      value a;
      sz = size_arg1; size_arg1 = size_arg2; size_arg2 = sz;
      p = ptr_arg1; ptr_arg1 = ptr_arg2; ptr_arg2 = p;
      s = sign_arg1; sign_arg1 = sign_arg2; sign_arg2 = s;
      a = arg1; arg1 = arg2; arg2 = a;
    }
    if (!size_arg2) r = arg1;
    else if (sign_arg1 && sign_arg2) {
      /* arg1 < 0, arg2 < 0 => r < 0 */
      r = ml_z_alloc(size_arg2 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg1, size_arg2, 1);
      c = 1; /* carry when decrementing arg2 */
      for (i = 0; i < size_arg2; i++) {
        mp_limb_t v = ptr_arg2[i];
        Z_LIMB(r)[i] = Z_LIMB(r)[i] & (v - c);
        c = c && !v;
      }
      c = mpn_add_1(Z_LIMB(r), Z_LIMB(r), size_arg2, 1);
      Z_LIMB(r)[size_arg2] = c;
      r = ml_z_reduce(r, size_arg2 + 1, Z_SIGN_MASK);
    }
    else if (sign_arg1) {
      /* arg1 < 0, arg2 > 0 => r < 0 */
      r = ml_z_alloc(size_arg1 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg1, size_arg1, 1);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = Z_LIMB(r)[i] & (~ptr_arg2[i]);
      c = mpn_add_1(Z_LIMB(r), Z_LIMB(r), size_arg1, 1);
      Z_LIMB(r)[size_arg1] = c;
      r = ml_z_reduce(r, size_arg1 + 1, Z_SIGN_MASK);
    }
    else if (sign_arg2) {
      /* arg1 > 0, arg2 < 0 => r < 0*/
      r = ml_z_alloc(size_arg2 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg2, size_arg2, 1);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = (~ptr_arg1[i]) & Z_LIMB(r)[i];
      c = mpn_add_1(Z_LIMB(r), Z_LIMB(r), size_arg2, 1);
      Z_LIMB(r)[size_arg2] = c;
      r = ml_z_reduce(r, size_arg2 + 1, Z_SIGN_MASK);
    }
    else {
      /* arg1, arg2 > 0 => r > 0 */
      r = ml_z_alloc(size_arg1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i] | ptr_arg2[i];
      for (; i < size_arg1; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i];
      r = ml_z_reduce(r, size_arg1, 0);
    }
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_logxor(value arg1, value arg2)
{
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH && !defined(Z_ASM_logxor)
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    return (arg1 ^ arg2) | 1;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam2(arg1,arg2);
    Z_DECL(arg1); Z_DECL(arg2);
    value r;
    mp_size_t i;
    mp_limb_t c;
    Z_ARG(arg1);  Z_ARG(arg2);
    /* ensure size_arg1 >= size_arg2 */
    if (size_arg1 < size_arg2) {
      mp_size_t sz;
      mp_limb_t *p, s;
      value a;
      sz = size_arg1; size_arg1 = size_arg2; size_arg2 = sz;
      p = ptr_arg1; ptr_arg1 = ptr_arg2; ptr_arg2 = p;
      s = sign_arg1; sign_arg1 = sign_arg2; sign_arg2 = s;
      a = arg1; arg1 = arg2; arg2 = a;
    }
    if (!size_arg2) r = arg1;
    else if (sign_arg1 && sign_arg2) {
      /* arg1 < 0, arg2 < 0 => r >=0 */
      r = ml_z_alloc(size_arg1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg1, size_arg1, 1);
      c = 1; /* carry when decrementing arg2 */
      for (i = 0; i < size_arg2; i++) {
        mp_limb_t v = ptr_arg2[i];
        Z_LIMB(r)[i] = Z_LIMB(r)[i] ^ (v - c);
        c = c && !v;
      }
      r = ml_z_reduce(r, size_arg1, 0);
    }
    else if (sign_arg1) {
      /* arg1 < 0, arg2 > 0 => r < 0 */
      r = ml_z_alloc(size_arg1 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg1, size_arg1, 1);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = Z_LIMB(r)[i] ^ ptr_arg2[i];
      c = mpn_add_1(Z_LIMB(r), Z_LIMB(r), size_arg1, 1);
      Z_LIMB(r)[size_arg1] = c;
      r = ml_z_reduce(r, size_arg1 + 1, Z_SIGN_MASK);
    }
    else if (sign_arg2) {
      /* arg1 > 0, arg2 < 0 => r < 0 */
      r = ml_z_alloc(size_arg1 + 1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      mpn_sub_1(Z_LIMB(r), ptr_arg2, size_arg2, 1);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i] ^ Z_LIMB(r)[i];
      for (; i < size_arg1; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i];
      c = mpn_add_1(Z_LIMB(r), Z_LIMB(r), size_arg1, 1);
      Z_LIMB(r)[size_arg1] = c;
      r = ml_z_reduce(r, size_arg1 + 1, Z_SIGN_MASK);
    }
    else {
      /* arg1, arg2 > 0 => r >= 0 */
      r = ml_z_alloc(size_arg1);
      Z_REFRESH(arg1);
      Z_REFRESH(arg2);
      for (i = 0; i < size_arg2; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i] ^ ptr_arg2[i];
      for (; i < size_arg1; i++) 
        Z_LIMB(r)[i] = ptr_arg1[i];
      r = ml_z_reduce(r, size_arg1, 0);
    }
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_lognot(value arg)
{
  Z_MARK_OP;
  Z_CHECK(arg);
#if Z_FAST_PATH && !defined(Z_ASM_lognot)
  if (Is_long(arg)) {
    /* fast path */
    return (~arg) | 1;
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam1(arg);
    Z_DECL(arg);
    value r;
    Z_ARG(arg);
    r = ml_z_alloc(size_arg + 1);
    Z_REFRESH(arg);
    /* compute r = -arg - 1 */
    if (!size_arg) {
      /* arg = 0 => r = -1 */
      Z_LIMB(r)[0] = 1;
      r = ml_z_reduce(r, 1, Z_SIGN_MASK);
    }
    else if (sign_arg)  {
      /* arg < 0, r > 0, |r| = |arg| - 1 */
      mpn_sub_1(Z_LIMB(r), ptr_arg, size_arg, 1);
      r = ml_z_reduce(r, size_arg, 0);
    }
    else {
      /* arg > 0, r < 0, |r| = |arg| + 1 */
      mp_limb_t c = mpn_add_1(Z_LIMB(r), ptr_arg, size_arg, 1);
      Z_LIMB(r)[size_arg] = c;
      r = ml_z_reduce(r, size_arg + 1, Z_SIGN_MASK);
    }
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_shift_left(value arg, value count)
{
  Z_DECL(arg);
  intnat c = Long_val(count);
  intnat c1, c2;
  Z_MARK_OP;
  Z_CHECK(arg);
  if (c < 0)
    caml_invalid_argument("Z.shift_left: count argument must be positive");
  if (!c) return arg;
  c1 = c / Z_LIMB_BITS;
  c2 = c % Z_LIMB_BITS;
#if Z_FAST_PATH && !defined(Z_ASM_shift_left)
  if (Is_long(arg) && !c1) {
    /* fast path */
    value a = arg - 1;
    value r = arg << c2;
    if (a == (r >> c2)) return r | 1;
  }
#endif
  Z_ARG(arg);
  if (!size_arg) return Val_long(0);
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam1(arg);
    value r;
    mp_size_t i;
    r = ml_z_alloc(size_arg + c1 + 1);
    Z_REFRESH(arg);
    /* 0-filled limbs */
    for (i = 0; i < c1; i++) Z_LIMB(r)[i] = 0;
    if (c2) {
      /* shifted bits */
      mp_limb_t x = mpn_lshift(Z_LIMB(r) + c1, ptr_arg, size_arg, c2);
      Z_LIMB(r)[size_arg + c1] = x;
    }
    else {
      /* unshifted copy */
      ml_z_cpy_limb(Z_LIMB(r) + c1, ptr_arg, size_arg);
      Z_LIMB(r)[size_arg + c1] = 0;
    }
    r = ml_z_reduce(r, size_arg + c1 + 1, sign_arg);
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_shift_right(value arg, value count)
{
  Z_DECL(arg);
  intnat c = Long_val(count);
  intnat c1, c2;
  value r;
  Z_MARK_OP;
  Z_CHECK(arg);
  if (c < 0)
    caml_invalid_argument("Z.shift_right: count argument must be positive");
  if (!c) return arg;
  c1 = c / Z_LIMB_BITS;
  c2 = c % Z_LIMB_BITS;
#if Z_FAST_PATH && !defined(Z_ASM_shift_right)
  if (Is_long(arg)) {
    /* fast path */
    if (c1) {
      if (arg < 0) return Val_long(-1);
      else return Val_long(0);
    }
    return (arg >> c2) | 1;
  }
#endif
  Z_ARG(arg);
  if (c1 >= size_arg) {
    if (sign_arg) return Val_long(-1);
    else return Val_long(0);
  }
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam1(arg);
    mp_limb_t cr;
    r = ml_z_alloc(size_arg - c1 + 1);
    Z_REFRESH(arg);
    if (c2)
      /* shifted bits */
      cr = mpn_rshift(Z_LIMB(r), ptr_arg + c1, size_arg - c1, c2);
    else {
      /* unshifted copy */
      ml_z_cpy_limb(Z_LIMB(r), ptr_arg + c1, size_arg - c1);
      cr = 0;
    }
    if (sign_arg) {
      /* round |arg| to +oo */
      mp_size_t i;
      if (!cr) {
      for (i = 0; i < c1; i++)
        if (ptr_arg[i]) { cr = 1; break; }
      }
      if (cr) 
        cr = mpn_add_1(Z_LIMB(r), Z_LIMB(r), size_arg - c1, 1);
    }
    else cr = 0;
    Z_LIMB(r)[size_arg - c1] = cr;
    r = ml_z_reduce(r, size_arg - c1 + 1, sign_arg);
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_shift_right_trunc(value arg, value count)
{
  Z_DECL(arg);
  intnat c = Long_val(count);
  intnat c1, c2;
  value r;
  Z_MARK_OP;
  Z_CHECK(arg);
  if (c < 0)
    caml_invalid_argument("Z.shift_right_trunc: count argument must be positive");
  if (!c) return arg;
  c1 = c / Z_LIMB_BITS;
  c2 = c % Z_LIMB_BITS;
#if Z_FAST_PATH
  if (Is_long(arg)) {
    /* fast path */
    if (c1) return Val_long(0);
    if (arg >= 1) return (arg >> c2) | 1;
    else return 2 - (((2 - arg) >> c2) | 1);
  }
#endif
  Z_ARG(arg);
  if (c1 >= size_arg) return Val_long(0);
  /* mpn_ version */
  Z_MARK_SLOW;
  {
    CAMLparam1(arg);
    r = ml_z_alloc(size_arg - c1);
    Z_REFRESH(arg);
    if (c2)
      /* shifted bits */
      mpn_rshift(Z_LIMB(r), ptr_arg + c1, size_arg - c1, c2);
    else
      /* unshifted copy */
    ml_z_cpy_limb(Z_LIMB(r), ptr_arg + c1, size_arg - c1);
    r = ml_z_reduce(r, size_arg - c1, sign_arg);
    Z_CHECK(r);
    CAMLreturn(r);
  }
}

/* Helper function for numbits: number of leading 0 bits in x */

#ifdef _LONG_LONG_LIMB
#define BUILTIN_CLZ __builtin_clzll
#else
#define BUILTIN_CLZ __builtin_clzl
#endif

/* Use GCC or Clang built-in if available.  The argument must be != 0. */
#if defined(__clang__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#define ml_z_clz BUILTIN_CLZ
#else
/* Portable C implementation - Hacker's Delight fig 5.12 */
int ml_z_clz(mp_limb_t x)
{
  int n;
  mp_limb_t y;
#ifdef ARCH_SIXTYFOUR
  n = 64;
  y = x >> 32;  if (y != 0) { n = n - 32; x = y; }
#else
  n = 32;
#endif
  y = x >> 16;  if (y != 0) { n = n - 16; x = y; }
  y = x >>  8;  if (y != 0) { n = n -  8; x = y; }
  y = x >>  4;  if (y != 0) { n = n -  4; x = y; }
  y = x >>  2;  if (y != 0) { n = n -  2; x = y; }
  y = x >>  1;  if (y != 0) return n - 2;
  return n - x;
}
#endif

CAMLprim value ml_z_numbits(value arg)
{
  Z_DECL(arg);
  intnat r;
  int n;
  Z_MARK_OP;
  Z_CHECK(arg);
#if Z_FAST_PATH
  if (Is_long(arg)) {
    /* fast path */
    r = Long_val(arg);
    if (r == 0) {
      return Val_int(0);
    } else {
      n = ml_z_clz(r > 0 ? r : -r);
      return Val_long(sizeof(intnat) * 8 - n);
    }
  }
#endif
  /* mpn_ version */  
  Z_MARK_SLOW;
  Z_ARG(arg);
  if (size_arg == 0) return Val_int(0);
  n = ml_z_clz(ptr_arg[size_arg - 1]);
  return Val_long(size_arg * Z_LIMB_BITS - n);
}

/* Helper function for trailing_zeros: number of trailing 0 bits in x */

#ifdef _LONG_LONG_LIMB
#define BUILTIN_CTZ __builtin_ctzll
#else
#define BUILTIN_CTZ __builtin_ctzl
#endif

/* Use GCC or Clang built-in if available.  The argument must be != 0. */
#if defined(__clang__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#define ml_z_ctz BUILTIN_CTZ
#else
/* Portable C implementation - Hacker's Delight fig 5.21 */
int ml_z_ctz(mp_limb_t x)
{
  int n;
  mp_limb_t y;
  CAMLassert (x != 0);
#ifdef ARCH_SIXTYFOUR
  n = 63;
  y = x << 32;  if (y != 0) { n = n - 32; x = y; }
#else
  n = 31;
#endif
  y = x << 16;  if (y != 0) { n = n - 16; x = y; }
  y = x <<  8;  if (y != 0) { n = n -  8; x = y; }
  y = x <<  4;  if (y != 0) { n = n -  4; x = y; }
  y = x <<  2;  if (y != 0) { n = n -  2; x = y; }
  y = x <<  1;  if (y != 0) { n = n - 1; }
  return n;
}
#endif

CAMLprim value ml_z_trailing_zeros(value arg)
{
  Z_DECL(arg);
  intnat r;
  mp_size_t i;
  Z_MARK_OP;
  Z_CHECK(arg);
#if Z_FAST_PATH
  if (Is_long(arg)) {
    /* fast path */
    r = Long_val(arg);
    if (r == 0) {
      return Val_long (Max_long);
    } else {
      /* No need to take absolute value of r, as ctz(-x) = ctz(x) */
      return Val_long (ml_z_ctz(r));
    }
  }
#endif
  /* mpn_ version */  
  Z_MARK_SLOW;
  Z_ARG(arg);
  if (size_arg == 0) return Val_long (Max_long);
  for (i = 0; ptr_arg[i] == 0; i++) /* skip */;
  return Val_long(i * Z_LIMB_BITS + ml_z_ctz(ptr_arg[i]));
}

/* helper function for popcount & hamdist: number of bits at 1 in x */
/* maybe we should use the mpn_ function even for small arguments, in case
   the CPU has a fast popcount opcode?
 */
uintnat ml_z_count(uintnat x)
{
#ifdef ARCH_SIXTYFOUR
  x = (x & 0x5555555555555555UL) + ((x >>  1) & 0x5555555555555555UL);
  x = (x & 0x3333333333333333UL) + ((x >>  2) & 0x3333333333333333UL);
  x = (x & 0x0f0f0f0f0f0f0f0fUL) + ((x >>  4) & 0x0f0f0f0f0f0f0f0fUL);
  x = (x & 0x00ff00ff00ff00ffUL) + ((x >>  8) & 0x00ff00ff00ff00ffUL);
  x = (x & 0x0000ffff0000ffffUL) + ((x >> 16) & 0x0000ffff0000ffffUL);
  x = (x & 0x00000000ffffffffUL) + ((x >> 32) & 0x00000000ffffffffUL);
#else
  x = (x & 0x55555555UL) + ((x >>  1) & 0x55555555UL);
  x = (x & 0x33333333UL) + ((x >>  2) & 0x33333333UL);
  x = (x & 0x0f0f0f0fUL) + ((x >>  4) & 0x0f0f0f0fUL);
  x = (x & 0x00ff00ffUL) + ((x >>  8) & 0x00ff00ffUL);
  x = (x & 0x0000ffffUL) + ((x >> 16) & 0x0000ffffUL);
#endif
  return x;
}

CAMLprim value ml_z_popcount(value arg)
{
  Z_DECL(arg);
  intnat r;
  Z_MARK_OP;
  Z_CHECK(arg);
#if Z_FAST_PATH
  if (Is_long(arg)) {
    /* fast path */
    r = Long_val(arg);
    if (r < 0) ml_z_raise_overflow();
    return Val_long(ml_z_count(r));
  }
#endif
  /* mpn_ version */  
  Z_MARK_SLOW;
  Z_ARG(arg);
  if (sign_arg) ml_z_raise_overflow();
  if (!size_arg) return Val_long(0);
  r =  mpn_popcount(ptr_arg, size_arg);
  if (r < 0 || !Z_FITS_INT(r)) ml_z_raise_overflow();
  return Val_long(r);
}

CAMLprim value ml_z_hamdist(value arg1, value arg2)
{
  Z_DECL(arg1); Z_DECL(arg2);
  intnat r;
  mp_size_t sz;
  Z_MARK_OP;
  Z_CHECK(arg1);
  Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    r = Long_val(arg1) ^ Long_val(arg2);
    if (r < 0) ml_z_raise_overflow();
    return Val_long(ml_z_count(r));
  }
#endif
  /* mpn_ version */
  Z_MARK_SLOW;
  Z_ARG(arg1);
  Z_ARG(arg2);
  if (sign_arg1 != sign_arg2) ml_z_raise_overflow();
  /* XXX TODO: case where arg1 & arg2 are both negative */
  if (sign_arg1 || sign_arg2) 
    caml_invalid_argument("Z.hamdist: negative arguments");
  /* distance on common size */
  sz = (size_arg1 <= size_arg2) ? size_arg1 : size_arg2;
  if (sz) {
    r =  mpn_hamdist(ptr_arg1, ptr_arg2, sz);
    if (r < 0 || !Z_FITS_INT(r)) ml_z_raise_overflow();
  }
  else r = 0;
  /* add stray bits */
  if (size_arg1 > size_arg2) {
    r += mpn_popcount(ptr_arg1 + size_arg2, size_arg1 - size_arg2);
    if (r < 0 || !Z_FITS_INT(r)) ml_z_raise_overflow();
  }
  else if (size_arg2 > size_arg1) {
    r += mpn_popcount(ptr_arg2 + size_arg1, size_arg2 - size_arg1);
    if (r < 0 || !Z_FITS_INT(r)) ml_z_raise_overflow();
  }
  return Val_long(r);
}

CAMLprim value ml_z_testbit(value arg, value index)
{
  Z_DECL(arg);
  uintnat b_idx;
  mp_size_t l_idx, i;
  mp_limb_t limb;
  Z_MARK_OP;
  Z_CHECK(arg);
  b_idx = Long_val(index); /* Caml code checked index >= 0 */
#if Z_FAST_PATH
  if (Is_long(arg)) {
    if (b_idx >= Z_LIMB_BITS) b_idx = Z_LIMB_BITS - 1;
    return Val_int((Long_val(arg) >> b_idx) & 1);
  }
#endif
  Z_MARK_SLOW;
  Z_ARG(arg);
  l_idx = b_idx / Z_LIMB_BITS;
  if (l_idx >= size_arg) return Val_bool(sign_arg);
  limb = ptr_arg[l_idx];
  if (sign_arg != 0) {
    /* If arg is negative, its 2-complement representation is
       bitnot(abs(arg) - 1).
       If any of the limbs of abs(arg) below l_idx is nonzero, 
       the carry from the decrement dies before reaching l_idx,
       and we just test bitnot(limb).
       If all the limbs below l_idx are zero, the carry from the
       decrement propagates to l_idx,
       and we test bitnot(limb - 1) = - limb. */
    for (i = 0; i < l_idx; i++) {
      if (ptr_arg[i] != 0) { limb = ~limb; goto extract; }
    }
    limb = -limb;
  }
 extract:
  return Val_int((limb >> (b_idx % Z_LIMB_BITS)) & 1);
}

/*---------------------------------------------------
  FUNCTIONS BASED ON mpz_t
  ---------------------------------------------------*/

/* sets rop to the value in op (limbs are copied) */
void ml_z_mpz_set_z(mpz_t rop, value op)
{
  Z_DECL(op);
  Z_CHECK(op);
  Z_ARG(op);
  if (size_op * Z_LIMB_BITS > INT_MAX)
    caml_invalid_argument("Z: risk of overflow in mpz type");  
  mpz_realloc2(rop, size_op * Z_LIMB_BITS);
  rop->_mp_size = (sign_op >= 0) ? size_op : -size_op;
  ml_z_cpy_limb(rop->_mp_d, ptr_op, size_op);
}

/* inits and sets rop to the value in op (limbs are copied) */
void ml_z_mpz_init_set_z(mpz_t rop, value op)
{
  mpz_init(rop);
  ml_z_mpz_set_z(rop,op);
}

/* returns a new z objects equal to op (limbs are copied) */
value ml_z_from_mpz(mpz_t op)
{
  value r;
  size_t sz = mpz_size(op);
  r = ml_z_alloc(sz);
  ml_z_cpy_limb(Z_LIMB(r), op->_mp_d, sz);
  return ml_z_reduce(r, sz, (mpz_sgn(op) >= 0) ? 0 : Z_SIGN_MASK);
}

#if __GNU_MP_VERSION >= 5
/* not exported by gmp.h */
extern void __gmpn_divexact (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);
#endif

CAMLprim value ml_z_divexact(value arg1, value arg2)
{
  Z_DECL(arg1); Z_DECL(arg2);
  Z_MARK_OP;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q;
    if (!a2) ml_z_raise_divide_by_zero();
    q = a1 / a2;
    if (Z_FITS_INT(q)) return Val_long(q);
  }
#endif
  Z_MARK_SLOW;
#if __GNU_MP_VERSION >= 5
  {
    /* mpn_ version */
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (!size_arg2)
      ml_z_raise_divide_by_zero();
    if (size_arg1 < size_arg2)
      return Val_long(0);
    {
      CAMLparam2(arg1,arg2);
      CAMLlocal1(q);
      q = ml_z_alloc(size_arg1 - size_arg2 + 1);
      Z_REFRESH(arg1); Z_REFRESH(arg2);
      __gmpn_divexact(Z_LIMB(q),
                      ptr_arg1,  size_arg1, ptr_arg2, size_arg2);
      q = ml_z_reduce(q, size_arg1 - size_arg2 + 1, sign_arg1 ^ sign_arg2);
      Z_CHECK(q);
      CAMLreturn(q);
    }
  }
#else
  {
    /* mpz_ version */
    CAMLparam2(arg1,arg2);
    CAMLlocal1(r);
    mpz_t a,b;
    if (!ml_z_sgn(arg2))
      ml_z_raise_divide_by_zero();
    ml_z_mpz_init_set_z(a, arg1);
    ml_z_mpz_init_set_z(b, arg2);
    mpz_divexact(a, a, b);
    r = ml_z_from_mpz(a);
    mpz_clear(a);
    mpz_clear(b);
    CAMLreturn(r);
  }
#endif
}
 
CAMLprim value ml_z_powm(value base, value exp, value mod)
{
  CAMLparam3(base,exp,mod);
  CAMLlocal1(r);
  mpz_t mbase, mexp, mmod;
  ml_z_mpz_init_set_z(mbase, base);
  ml_z_mpz_init_set_z(mexp, exp);
  ml_z_mpz_init_set_z(mmod, mod);
  if (mpz_sgn(mexp) < 0) {
    /* we need to check whether base is invertible to avoid a division by zero
       in mpz_powm, so we can as well use the computed inverse
     */
    if (!mpz_invert(mbase, mbase, mmod))
      ml_z_raise_divide_by_zero();
    mpz_neg(mexp, mexp);
  }
  mpz_powm(mbase, mbase, mexp, mmod);
  r = ml_z_from_mpz(mbase);
  mpz_clear(mbase);
  mpz_clear(mexp);
  mpz_clear(mmod);
  CAMLreturn(r);
}

CAMLprim value ml_z_powm_sec(value base, value exp, value mod)
{
#ifndef HAS_MPIR
#if __GNU_MP_VERSION >= 5
  CAMLparam3(base,exp,mod);
  CAMLlocal1(r);
  mpz_t mbase, mexp, mmod;
  ml_z_mpz_init_set_z(mbase, base);
  ml_z_mpz_init_set_z(mexp, exp);
  ml_z_mpz_init_set_z(mmod, mod);
  if (mpz_sgn(mexp) <= 0)
    caml_invalid_argument("Z.powm_sec: exponent must be positive");
  if (! mpz_odd_p(mmod))
    caml_invalid_argument("Z.powm_sec: modulus must be odd");
  mpz_powm_sec(mbase, mbase, mexp, mmod);
  r = ml_z_from_mpz(mbase);
  mpz_clear(mbase);
  mpz_clear(mexp);
  mpz_clear(mmod);
  CAMLreturn(r);
#else
  MAYBE_UNUSED(base);
  MAYBE_UNUSED(exp);
  MAYBE_UNUSED(mod);
  caml_invalid_argument("Z.powm_sec: not available, needs GMP version >= 5");
#endif
#else
  MAYBE_UNUSED(base);
  MAYBE_UNUSED(exp);
  MAYBE_UNUSED(mod);
  caml_invalid_argument("Z.powm_sec: not available in MPIR, needs GMP version >= 5");
#endif
}

CAMLprim value ml_z_pow(value base, value exp)
{
  CAMLparam2(base,exp);
  CAMLlocal1(r);
  mpz_t mbase;
  intnat e = Long_val(exp);
  mp_size_t sz, ralloc;
  int cnt;
  if (e < 0) 
    caml_invalid_argument("Z.pow: exponent must be non-negative");
  ml_z_mpz_init_set_z(mbase, base);

  /* Safe overapproximation of the size of the result.
     In case this overflows an int, GMP may abort with a message
     "gmp: overflow in mpz type". To avoid this, we test the size before
     calling mpz_pow_ui and raise an OCaml exception.
     Note: we lifted the computation from mpz_n_pow_ui.
   */
  sz = mbase->_mp_size;
  if (sz < 0) sz = -sz;
  cnt = sz > 0 ? ml_z_clz(mbase->_mp_d[sz - 1]) : 0;
  ralloc = (sz * GMP_NUMB_BITS - cnt + GMP_NAIL_BITS) * e / GMP_NUMB_BITS + 5;
  if (ralloc > INT_MAX)
    caml_invalid_argument("Z.pow: risk of overflow in mpz type");

  mpz_pow_ui(mbase, mbase, e);
  r = ml_z_from_mpz(mbase);
  mpz_clear(mbase);
  CAMLreturn(r);
}

CAMLprim value ml_z_root(value a, value b)
{
  CAMLparam2(a,b);
  CAMLlocal1(r);
  mpz_t ma;
  intnat mb = Long_val(b);
  if (mb < 0) 
    caml_invalid_argument("Z.root: exponent must be non-negative");
  ml_z_mpz_init_set_z(ma, a);
  mpz_root(ma, ma, mb);
  r = ml_z_from_mpz(ma);
  mpz_clear(ma);
  CAMLreturn(r);
}

CAMLprim value ml_z_perfect_power(value a)
{
  CAMLparam1(a);
  int r;
  mpz_t ma;
  ml_z_mpz_init_set_z(ma, a);
  r = mpz_perfect_power_p(ma);
  mpz_clear(ma);
  CAMLreturn(r ? Val_true : Val_false);
}

CAMLprim value ml_z_perfect_square(value a)
{
  CAMLparam1(a);
  int r;
  mpz_t ma;
  ml_z_mpz_init_set_z(ma, a);
  r = mpz_perfect_square_p(ma);
  mpz_clear(ma);
  CAMLreturn(r ? Val_true : Val_false);
}

CAMLprim value ml_z_probab_prime(value a, int b)
{
  CAMLparam1(a);
  int r;
  mpz_t ma;
  ml_z_mpz_init_set_z(ma, a);
  r = mpz_probab_prime_p(ma, Int_val(b));
  mpz_clear(ma);
  CAMLreturn(Val_int(r));
}

CAMLprim value ml_z_nextprime(value a)
{
  CAMLparam1(a);
  CAMLlocal1(r);
  mpz_t ma;
  ml_z_mpz_init_set_z(ma, a);
  mpz_nextprime(ma, ma);
  r = ml_z_from_mpz(ma);
  mpz_clear(ma);
  CAMLreturn(r);
}

CAMLprim value ml_z_invert(value base, value mod)
{
  CAMLparam2(base,mod);
  CAMLlocal1(r);
  mpz_t mbase, mmod;
  ml_z_mpz_init_set_z(mbase, base);
  ml_z_mpz_init_set_z(mmod, mod);
  if (!mpz_invert(mbase, mbase, mmod))
    ml_z_raise_divide_by_zero();
  r = ml_z_from_mpz(mbase);
  mpz_clear(mbase);
  mpz_clear(mmod);
  CAMLreturn(r);
}

/* XXX should we support the following?
   mpz_divisible_p
   mpz_congruent_p
   mpz_powm_sec
   mpz_rootrem
   mpz_jacobi
   mpz_legendre
   mpz_kronecker
   mpz_remove
   mpz_fac_ui
   mpz_bin_ui
   mpz_fib_ui
   mpz_lucnum_ui
   mpz_scan0, mpz_scan1
   mpz_setbit, mpz_clrbit, mpz_combit, mpz_tstbit
   mpz_odd_p, mpz_even_p
   random numbers
*/


/*---------------------------------------------------
  CUSTOMS BLOCKS
  ---------------------------------------------------*/

/* With OCaml < 3.12.1, comparing a block an int with OCaml's
   polymorphic compare will give erroneous results (int always
   strictly smaller than block).  OCaml 3.12.1 and above
   give the correct result.
*/
int ml_z_custom_compare(value arg1, value arg2)
{
  Z_DECL(arg1);  Z_DECL(arg2);
  int r;
  Z_CHECK(arg1); Z_CHECK(arg2);
#if Z_FAST_PATH
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    if (arg1 > arg2) return 1;
    else if (arg1 < arg2) return -1;
    else return 0;
  }
#endif
  r = 0;
  Z_ARG(arg1);
  Z_ARG(arg2);
  if (sign_arg1 != sign_arg2) r = 1;
  else if (size_arg1 > size_arg2) r = 1;
  else if (size_arg1 < size_arg2) r = -1;
  else {
    mp_size_t i;
    for (i = size_arg1 - 1; i >= 0; i--) {
      if (ptr_arg1[i] > ptr_arg2[i]) { r = 1; break; }
      if (ptr_arg1[i] < ptr_arg2[i]) { r = -1; break; }
    }
  }
  if (sign_arg1) r = -r;
  return r;
}

#ifndef Z_OCAML_HASH
#define caml_hash_mix_uint32(h,n) ((h) * 65599 + (n))
#endif

static intnat ml_z_custom_hash(value v)
{
  Z_DECL(v);
  mp_size_t i;
  uint32_t acc = 0;
  Z_CHECK(v);
  Z_ARG(v);
  for (i = 0; i < size_v; i++) {
    acc = caml_hash_mix_uint32(acc, (uint32_t)(ptr_v[i]));
#ifdef ARCH_SIXTYFOUR
    acc = caml_hash_mix_uint32(acc, ptr_v[i] >> 32);
#endif
  }
#ifndef ARCH_SIXTYFOUR
  /* To obtain the same hash value on 32- and 64-bit platforms */
  if (size_v % 2 != 0)
    acc = caml_hash_mix_uint32(acc, 0);
#endif
  if (sign_v) acc++;
  return acc;
}

CAMLprim value ml_z_hash(value v)
{
  return Val_long(ml_z_custom_hash(v));
}

/* serialized format:
   - 1-byte sign (1 for negative, 0 for positive)
   - 4-byte size in bytes
   - size-byte unsigned integer, in little endian order
 */
static void ml_z_custom_serialize(value v, 
                                  uintnat * wsize_32,
                                  uintnat * wsize_64)
{
  mp_size_t i,nb;
  Z_DECL(v);
  Z_CHECK(v);
  Z_ARG(v);
  if ((mp_size_t)(uint32_t) size_v != size_v) 
    caml_failwith("Z.serialize: number is too large");
  nb = size_v * sizeof(mp_limb_t);
  caml_serialize_int_1(sign_v ? 1 : 0);
  caml_serialize_int_4(nb);
  for (i = 0; i < size_v; i++) {
    mp_limb_t x = ptr_v[i];
    caml_serialize_int_1(x);
    caml_serialize_int_1(x >> 8);
    caml_serialize_int_1(x >> 16);
    caml_serialize_int_1(x >> 24);
#ifdef ARCH_SIXTYFOUR
    caml_serialize_int_1(x >> 32);
    caml_serialize_int_1(x >> 40);
    caml_serialize_int_1(x >> 48);
    caml_serialize_int_1(x >> 56);
#endif
  }
  *wsize_32 = 4 * (1 + (nb + 3) / 4);
  *wsize_64 = 8 * (1 + (nb + 7) / 8);  
#if Z_PERFORM_CHECK
  /* Add space for canary */
  *wsize_32 += 4;
  *wsize_64 += 8;
#endif
}

/* XXX: serializing a large (i.e., > 2^31) int on a 64-bit machine and
   deserializing on a 32-bit machine will fail (instead of returning a
   block).
 */
static uintnat ml_z_custom_deserialize(void * dst)
{
  mp_limb_t* d = ((mp_limb_t*)dst) + 1;
  int sign = caml_deserialize_uint_1();
  uint32_t sz = caml_deserialize_uint_4();
  uint32_t szw = (sz + sizeof(mp_limb_t) - 1) / sizeof(mp_limb_t);
  uint32_t i = 0;
  mp_limb_t x; 
  /* all limbs but last */
  if (szw > 1) {
    for (; i < szw - 1; i++) {
      x = caml_deserialize_uint_1();
      x |= ((mp_limb_t) caml_deserialize_uint_1()) << 8;
      x |= ((mp_limb_t) caml_deserialize_uint_1()) << 16;
      x |= ((mp_limb_t) caml_deserialize_uint_1()) << 24;
#ifdef ARCH_SIXTYFOUR
      x |= ((mp_limb_t) caml_deserialize_uint_1()) << 32;
      x |= ((mp_limb_t) caml_deserialize_uint_1()) << 40;
      x |= ((mp_limb_t) caml_deserialize_uint_1()) << 48;
      x |= ((mp_limb_t) caml_deserialize_uint_1()) << 56;
#endif
      d[i] = x;
    }
    sz -= i * sizeof(mp_limb_t);
  }
  /* last limb */
  if (sz > 0) {
    x = caml_deserialize_uint_1();
    if (sz > 1) x |= ((mp_limb_t) caml_deserialize_uint_1()) << 8;
    if (sz > 2) x |= ((mp_limb_t) caml_deserialize_uint_1()) << 16;
    if (sz > 3) x |= ((mp_limb_t) caml_deserialize_uint_1()) << 24;
#ifdef ARCH_SIXTYFOUR
    if (sz > 4) x |= ((mp_limb_t) caml_deserialize_uint_1()) << 32;
    if (sz > 5) x |= ((mp_limb_t) caml_deserialize_uint_1()) << 40;
    if (sz > 6) x |= ((mp_limb_t) caml_deserialize_uint_1()) << 48;
    if (sz > 7) x |= ((mp_limb_t) caml_deserialize_uint_1()) << 56;
#endif
    d[i] = x;
    i++;
  }
  while (i > 0 && !d[i-1]) i--;
  d[-1] = i | (sign ? Z_SIGN_MASK : 0);
#if Z_PERFORM_CHECK
  d[szw] = 0xDEADBEEF ^ szw;
  szw++;
#endif
  return (szw+1) * sizeof(mp_limb_t);
}

struct custom_operations ml_z_custom_ops = {
  /* Identifiers starting with _ are normally reserved for the OCaml runtime
     system, but we got authorization form Gallium to use "_z".
     It is very compact and stays in the spirit of identifiers used for
     int32 & co ("_i" & co.).
  */
  "_z",
  custom_finalize_default,
  ml_z_custom_compare,
  ml_z_custom_hash,
  ml_z_custom_serialize,
  ml_z_custom_deserialize,
#if Z_OCAML_COMPARE_EXT
  ml_z_custom_compare,
#else
  custom_compare_ext_default,
#endif
#ifndef Z_OCAML_LEGACY_CUSTOM_OPERATIONS
  custom_fixed_length_default
#endif
};


/*---------------------------------------------------
  CONVERSION WITH MLGMPIDL
  ---------------------------------------------------*/

CAMLprim value ml_z_mlgmpidl_of_mpz(value a)
{
  CAMLparam1(a);
  mpz_ptr mpz = (mpz_ptr)(Data_custom_val(a));
  CAMLreturn(ml_z_from_mpz(mpz));
}

/* stores the Z.t object into an existing Mpz.t one;
   as we never allocate Mpz.t objects, we don't need any pointer to 
   mlgmpidl's custom block ops, and so, can link the function even if
   mlgmpidl is not installed
 */
CAMLprim value ml_z_mlgmpidl_set_mpz(value r, value a)
{
  CAMLparam2(r,a);
  mpz_ptr mpz = (mpz_ptr)(Data_custom_val(r));
  ml_z_mpz_set_z(mpz,a);
  CAMLreturn(Val_unit);
}



/*---------------------------------------------------
  INIT / EXIT
  ---------------------------------------------------*/

/* called at program exit to display performance information */
#if Z_PERF_COUNTER
static void ml_z_dump_count()
{
  printf("Z: %lu asm operations, %lu C operations, %lu slow (%lu%%)\n",
         ml_z_ops_as, ml_z_ops, ml_z_slow, 
         ml_z_ops ? (ml_z_slow*100/(ml_z_ops+ml_z_ops_as)) : 0);
}
#endif

CAMLprim value ml_z_install_frametable()
{
  /* nothing to do for bytecode version */
  return Val_unit;
}

CAMLprim value ml_z_init()
{
  ml_z_2p32 = ldexp(1., 32);
  /* run-time checks */
#ifdef ARCH_SIXTYFOUR
  if (sizeof(intnat) != 8 || sizeof(mp_limb_t) != 8)
    caml_failwith("Z.init: invalid size of types, 8 expected");
#else
  if (sizeof(intnat) != 4 || sizeof(mp_limb_t) != 4)
    caml_failwith("Z.init: invalid size of types, 4 expected");
#endif
  /* install functions */
#if Z_PERF_COUNTER
  atexit(ml_z_dump_count);
#endif
#if Z_CUSTOM_BLOCK
  caml_register_custom_operations(&ml_z_custom_ops);
#endif
  return Val_unit;
}

#ifdef __cplusplus
}
#endif
