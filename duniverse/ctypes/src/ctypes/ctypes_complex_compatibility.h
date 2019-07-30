/*
 * Copyright (c) 2018 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_COMPLEX_COMPATIBILITY_H
#define CTYPES_COMPLEX_COMPATIBILITY_H

/* "Each complex type has the same representation and alignment
    requirements as an array type containing exactly two elements of
    the corresponding real type; the first element is equal to the real
    part, and the second element to the imaginary part, of the complex
    number."
                                                       - C99 6.2.5 (13)
*/
union ctypes_complex_long_double_union {
  long double _Complex z;
  long double parts[2];
};

union ctypes_complex_double_union {
  double _Complex z;
  double parts[2];
};

union ctypes_complex_float_union {
  float _Complex z;
  float parts[2];
};

#if defined(__ANDROID__)
#define CTYPES_USE_STRUCT_BUILDER 1

#include <math.h>

#include <caml/fail.h>

static inline long double ctypes_compat_creall(long double _Complex z)
{ union ctypes_complex_long_double_union u; u.z = z; return u.parts[0]; }

static inline long double ctypes_compat_cimagl(long double _Complex z)
{ union ctypes_complex_long_double_union u; u.z = z; return u.parts[1]; }

static inline long double _Complex ctypes_compat_conjl(long double _Complex z)
{ union ctypes_complex_long_double_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

static inline long double _Complex ctypes_compat_csqrtl(long double _Complex z)
{ caml_failwith("ctypes: csqrtl does not exist on current platform"); }

static inline long double ctypes_compat_cargl(long double _Complex z)
{ return atan2(ctypes_compat_cimagl(z), ctypes_compat_creall(z)); }

static inline double ctypes_compat_creal(double _Complex z)
{ union ctypes_complex_double_union u; u.z = z; return u.parts[0]; }

static inline double ctypes_compat_cimag(double _Complex z)
{ union ctypes_complex_double_union u; u.z = z; return u.parts[1]; }

static inline double _Complex ctypes_compat_conj(double _Complex z)
{ union ctypes_complex_double_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

static inline float ctypes_compat_crealf(float _Complex z)
{ union ctypes_complex_float_union u; u.z = z; return u.parts[0]; }

static inline float ctypes_compat_cimagf(float _Complex z)
{ union ctypes_complex_float_union u; u.z = z; return u.parts[1]; }

static inline float _Complex ctypes_compat_conjf(float _Complex z)
{ union ctypes_complex_float_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

#else

#include <complex.h>

static inline long double ctypes_compat_creall(long double _Complex z)
{ return creall(z); }
static inline long double ctypes_compat_cimagl(long double _Complex z)
{ return cimagl(z); }
static inline long double _Complex ctypes_compat_conjl(long double _Complex z)
{ return conjl(z); }
static inline long double _Complex ctypes_compat_cexpl(long double _Complex z)
{ return cexpl(z); }
static inline long double _Complex ctypes_compat_clogl(long double _Complex z)
{ return clogl(z); }
static inline long double _Complex ctypes_compat_cpowl(long double _Complex x, long double _Complex z)
{ return cpowl(x, z); }
static inline long double _Complex ctypes_compat_csqrtl(long double _Complex z)
{ return csqrtl(z); }
static inline long double ctypes_compat_cargl(long double _Complex z)
{ return cargl(z); }

static inline double ctypes_compat_creal(double _Complex z)
{ return creal(z); }
static inline double ctypes_compat_cimag(double _Complex z)
{ return cimag(z); }
static inline double _Complex ctypes_compat_conj(double _Complex z)
{ return conj(z); }

static inline float ctypes_compat_crealf(float _Complex z)
{ return crealf(z); }
static inline float ctypes_compat_cimagf(float _Complex z)
{ return cimagf(z); }
static inline float _Complex ctypes_compat_conjf(float _Complex z)
{ return conjf(z); }

#if !defined(CMPLXF) || !defined(CMPLX) || !defined(CMPLXL)
#define CTYPES_USE_STRUCT_BUILDER 1
#else
static inline double _Complex ctypes_compat_make_complex(double re, double im)
{ return (CMPLX(re,im)); }
static inline long double _Complex ctypes_compat_make_complexl(long double re, long double im)
{ return (CMPLXL(re,im)); }
static inline float _Complex ctypes_compat_make_complexf(float re, float im)
{ return (CMPLXF(re,im)); }
#endif

#endif

#if defined(__ANDROID__) || defined(__FreeBSD__)
/* Android: As of API level 24, these functions do not exist.
   Freebsd: still missing in FreeBSD 11.0-RELEASE-p2
 */

static inline long double _Complex ctypes_compat_cexpl(long double _Complex z)
{ caml_failwith("ctypes: cexpl does not exist on current platform"); }

static inline long double _Complex ctypes_compat_clogl(long double _Complex z)
{ caml_failwith("ctypes: clogl does not exist on current platform"); }

static inline long double _Complex ctypes_compat_cpowl(long double _Complex x, long double _Complex z)
{ caml_failwith("ctypes: cpowl does not exist on current platform"); }
#endif


#ifdef CTYPES_USE_STRUCT_BUILDER
static inline double _Complex ctypes_compat_make_complex(double re, double im)
{ union ctypes_complex_double_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
static inline float _Complex ctypes_compat_make_complexf(float re, float im)
{ union ctypes_complex_float_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
static inline long double _Complex ctypes_compat_make_complexl(long double re, long double im)
{ union ctypes_complex_long_double_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
#undef CTYPES_USE_STRUCT_BUILDER
#endif

#endif /* CTYPES_COMPLEX_COMPATIBILITY_H */
