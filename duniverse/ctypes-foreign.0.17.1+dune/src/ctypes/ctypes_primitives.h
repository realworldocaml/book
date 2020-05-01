/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_PRIMITIVES_H
#define CTYPES_PRIMITIVES_H

#include <limits.h>
#include <assert.h>

#include <stdint.h>

#include "ocaml_integers.h"

/* The order here must correspond to the constructor order in primitives.ml */
enum ctypes_primitive {
  Ctypes_Char,
  Ctypes_Schar,
  Ctypes_Uchar,
  Ctypes_Bool,
  Ctypes_Short,
  Ctypes_Int,
  Ctypes_Long,
  Ctypes_Llong,
  Ctypes_Ushort,
  Ctypes_Sint,
  Ctypes_Uint,
  Ctypes_Ulong,
  Ctypes_Ullong,
  Ctypes_Size_t,
  Ctypes_Int8_t,
  Ctypes_Int16_t,
  Ctypes_Int32_t,
  Ctypes_Int64_t,
  Ctypes_Uint8_t,
  Ctypes_Uint16_t,
  Ctypes_Uint32_t,
  Ctypes_Uint64_t,
  Ctypes_Camlint,
  Ctypes_Nativeint,
  Ctypes_Float,
  Ctypes_Double,
  Ctypes_LDouble,
  Ctypes_Complex32,
  Ctypes_Complex64,
  Ctypes_Complexld,
};

/* short is at least 16 bits. */
#if USHRT_MAX == UINT16_MAX
#define ctypes_ushort_val Uint16_val
#define ctypes_copy_ushort Integers_val_uint16
#elif USHRT_MAX == UINT32_MAX
#define ctypes_ushort_val Uint32_val
#define ctypes_copy_ushort integers_copy_uint32
#elif USHRT_MAX == UINT64_MAX
#define ctypes_ushort_val Uint64_val
#define ctypes_copy_ushort integers_copy_uint64
#else
# error "No suitable OCaml type available for representing unsigned short values"
#endif

/* int is at least 16 bits. */
#if UINT_MAX == UINT16_MAX
#error "No suitable OCaml type available for representing signed int values"
#define ctypes_uint_val Uint16_val
#define ctypes_copy_uint Integers_val_uint16
#elif UINT_MAX == UINT32_MAX
#define ctypes_sint_val Int32_val
#define ctypes_uint_val Uint32_val
#define ctypes_copy_sint caml_copy_int32
#define ctypes_copy_uint integers_copy_uint32
#elif UINT_MAX == UINT64_MAX
#define ctypes_sint_val Int64_val
#define ctypes_uint_val Uint64_val
#define ctypes_copy_sint caml_copy_int64
#define ctypes_copy_uint integers_copy_uint64
#else
# error "No suitable OCaml type available for representing unsigned int values"
#endif

/* long is at least 32 bits. */
#if ULONG_MAX == UINT32_MAX
#define ctypes_long_val Int32_val
#define ctypes_ulong_val Uint32_val
#define ctypes_copy_long caml_copy_int32
#define ctypes_copy_ulong integers_copy_uint32
#elif ULONG_MAX == UINT64_MAX
#define ctypes_long_val Int64_val
#define ctypes_ulong_val Uint64_val
#define ctypes_copy_long caml_copy_int64
#define ctypes_copy_ulong integers_copy_uint64
#else
# error "No suitable OCaml type available for representing longs"
#endif

/* long long is at least 64 bits. */
#if ULLONG_MAX == UINT64_MAX
#define ctypes_llong_val Int64_val
#define ctypes_ullong_val Uint64_val
#define ctypes_copy_llong caml_copy_int64
#define ctypes_copy_ullong integers_copy_uint64
#else
# error "No suitable OCaml type available for representing long longs"
#endif

#if SIZE_MAX == UINT16_MAX
#define ctypes_size_t_val Uint16_val
#define ctypes_copy_size_t Integers_val_uint16
#elif SIZE_MAX == UINT32_MAX
#define ctypes_size_t_val Uint32_val
#define ctypes_copy_size_t integers_copy_uint32
#elif SIZE_MAX == UINT64_MAX
#define ctypes_size_t_val Uint64_val
#define ctypes_copy_size_t integers_copy_uint64
#else
# error "No suitable OCaml type available for representing size_t values"
#endif


/* Detection of arithmetic types */
enum ctypes_arithmetic_type {
  Ctypes_arith_Int8,
  Ctypes_arith_Int16,
  Ctypes_arith_Int32,
  Ctypes_arith_Int64,
  Ctypes_arith_Uint8,
  Ctypes_arith_Uint16,
  Ctypes_arith_Uint32,
  Ctypes_arith_Uint64,
  Ctypes_arith_Float,
  Ctypes_arith_Double
};

#define CTYPES_FLOATING_FLAG_BIT 15
#define CTYPES_UNSIGNED_FLAG_BIT 14
#define CTYPES_FLOATING ((size_t)1u << CTYPES_FLOATING_FLAG_BIT)
#define CTYPES_UNSIGNED ((size_t)1u << CTYPES_UNSIGNED_FLAG_BIT)
#define CTYPES_CHECK_FLOATING(TYPENAME) \
  ((unsigned)(((TYPENAME) 0.5) != 0) << CTYPES_FLOATING_FLAG_BIT)
#define CTYPES_CHECK_UNSIGNED(TYPENAME) \
  ((unsigned)(((TYPENAME) -1) > 0) << CTYPES_UNSIGNED_FLAG_BIT)
#define CTYPES_CLASSIFY(TYPENAME) (CTYPES_CHECK_FLOATING(TYPENAME) \
                                 | CTYPES_CHECK_UNSIGNED(TYPENAME))
#define CTYPES_ARITHMETIC_TYPEINFO(TYPENAME) (CTYPES_CLASSIFY(TYPENAME) \
                                            | sizeof(TYPENAME))
#define CTYPES_CLASSIFY_ARITHMETIC_TYPE(TYPENAME) \
  ctypes_classify_arithmetic_type(CTYPES_ARITHMETIC_TYPEINFO(TYPENAME))

static inline
enum ctypes_arithmetic_type ctypes_classify_arithmetic_type(size_t typeinfo)
{
  switch (typeinfo)
  {
  case CTYPES_FLOATING | sizeof(float):    return Ctypes_arith_Float;
  case CTYPES_FLOATING | sizeof(double):   return Ctypes_arith_Double;
  case CTYPES_UNSIGNED | sizeof(uint8_t):  return Ctypes_arith_Uint8;
  case CTYPES_UNSIGNED | sizeof(uint16_t): return Ctypes_arith_Uint16;
  case CTYPES_UNSIGNED | sizeof(uint32_t): return Ctypes_arith_Uint32;
  case CTYPES_UNSIGNED | sizeof(uint64_t): return Ctypes_arith_Uint64;
  case                   sizeof(int8_t):   return Ctypes_arith_Int8;
  case                   sizeof(int16_t):  return Ctypes_arith_Int16;
  case                   sizeof(int32_t):  return Ctypes_arith_Int32;
  case                   sizeof(int64_t):  return Ctypes_arith_Int64;
  default: assert(0);
  }
}

static inline
const char *ctypes_arithmetic_type_name(enum ctypes_arithmetic_type t)
{
  switch (t)
  {
  case Ctypes_arith_Int8:   return "Int8";
  case Ctypes_arith_Int16:  return "Int16";
  case Ctypes_arith_Int32:  return "Int32";
  case Ctypes_arith_Int64:  return "Int64";
  case Ctypes_arith_Uint8:  return "Uint8";
  case Ctypes_arith_Uint16: return "Uint16";
  case Ctypes_arith_Uint32: return "Uint32";
  case Ctypes_arith_Uint64: return "Uint64";
  case Ctypes_arith_Float:  return "Float";
  case Ctypes_arith_Double: return "Double";
  default: assert(0);
  }
}

#endif /* CTYPES_PRIMITIVES_H */
