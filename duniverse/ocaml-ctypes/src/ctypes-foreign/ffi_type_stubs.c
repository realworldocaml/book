/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <limits.h>
#include <stdbool.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <ffi.h>

#include "../ctypes/ctypes_primitives.h"
#include "../ctypes/ctypes_raw_pointer.h"
#include "../ctypes/ctypes_managed_buffer_stubs.h"

#if CHAR_MIN < 0
#define ctypes_ffi_type_char ffi_type_schar
#else
#define ctypes_ffi_type_char ffi_type_uchar 
#endif

/* We need a pointer-sized integer type.  SIZEOF_PTR is from caml/config.h. */
#if SIZEOF_PTR == 4
#define ctypes_ffi_type_camlint ffi_type_sint32
#elif SIZEOF_PTR == 8
#define ctypes_ffi_type_camlint ffi_type_sint64
#else
#error "No suitable pointer-sized integer type available"
#endif

/* long long is at least 64 bits. */
#if LLONG_MAX == 9223372036854775807LL
#define ctypes_ffi_type_sllong ffi_type_sint64
#define ctypes_ffi_type_ullong ffi_type_uint64
#else
# error "No suitable OCaml type available for representing longs"
#endif

#if SIZE_MAX == 65535U
#define ctypes_ffi_type_size_t ffi_type_uint16
#elif SIZE_MAX == 4294967295UL
#define ctypes_ffi_type_size_t ffi_type_uint32
#elif SIZE_MAX == 18446744073709551615ULL
#define ctypes_ffi_type_size_t ffi_type_uint64
#else
# error "No suitable OCaml type available for representing size_t values"
#endif

static ffi_type *bool_ffi_type(void)
{
  switch (sizeof(bool)) {
  case sizeof(uint8_t):  return &ffi_type_uint8;
  case sizeof(uint16_t): return &ffi_type_uint16;
  case sizeof(uint32_t): return &ffi_type_uint32;
  case sizeof(uint64_t): return &ffi_type_uint64;
  default: return NULL;
  }
}

/* primitive_ffitype : 'a prim -> 'a ffitype */
value ctypes_primitive_ffitype(value prim)
{
  void *ft = NULL;
  switch ((enum ctypes_primitive)Int_val(prim)) {
    case Ctypes_Char:      ft = &ctypes_ffi_type_char;    break; /* Char */
    case Ctypes_Schar:     ft = &ffi_type_schar;          break; /* Schar */
    case Ctypes_Uchar:     ft = &ffi_type_uchar;          break; /* Uchar */
    case Ctypes_Bool:      ft = bool_ffi_type();          break;
    case Ctypes_Short:     ft = &ffi_type_sshort;         break; /* Short */
    case Ctypes_Int:       ft = &ffi_type_sint;           break; /* Int */
    case Ctypes_Long:      ft = &ffi_type_slong;          break; /* Long */
    case Ctypes_Llong:     ft = &ctypes_ffi_type_sllong;  break; /* Llong */
    case Ctypes_Ushort:    ft = &ffi_type_ushort;         break; /* Ushort */
    case Ctypes_Sint:      ft = &ffi_type_sint;           break; /* Sint */
    case Ctypes_Uint:      ft = &ffi_type_uint;           break; /* Uint */
    case Ctypes_Ulong:     ft = &ffi_type_ulong;          break; /* Ulong */
    case Ctypes_Ullong:    ft = &ctypes_ffi_type_ullong;  break; /* Ullong */
    case Ctypes_Size_t:    ft = &ctypes_ffi_type_size_t;  break; /* Size */
    case Ctypes_Int8_t:    ft = &ffi_type_sint8;          break; /* Int8 */
    case Ctypes_Int16_t:   ft = &ffi_type_sint16;         break; /* Int16 */
    case Ctypes_Int32_t:   ft = &ffi_type_sint32;         break; /* Int32 */
    case Ctypes_Int64_t:   ft = &ffi_type_sint64;         break; /* Int64 */
    case Ctypes_Uint8_t:   ft = &ffi_type_uint8;          break; /* Uint8 */
    case Ctypes_Uint16_t:  ft = &ffi_type_uint16;         break; /* Uint16 */
    case Ctypes_Uint32_t:  ft = &ffi_type_uint32;         break; /* Uint32 */
    case Ctypes_Uint64_t:  ft = &ffi_type_uint64;         break; /* Uint64 */
    case Ctypes_Camlint:   ft = &ctypes_ffi_type_camlint; break; /* Camlint */
    case Ctypes_Nativeint: ft = &ctypes_ffi_type_camlint; break; /* Nativeint */
    case Ctypes_Float:     ft = &ffi_type_float;          break; /* Float */
    case Ctypes_Double:    ft = &ffi_type_double;         break; /* Double */
    case Ctypes_LDouble:   ft = NULL;                     break; /* LDouble */
    case Ctypes_Complex32: ft = NULL;                     break; /* Complex32 */
    case Ctypes_Complex64: ft = NULL;                     break; /* Complex64 */
    case Ctypes_Complexld: ft = NULL;                     break; /* Complexld */
  }
  return CTYPES_FROM_PTR(ft);
}


/* pointer_ffitype : unit -> voidp ffitype */
value ctypes_pointer_ffitype(value _)
{
  return CTYPES_FROM_PTR(&ffi_type_pointer);
}

/* void_ffitype : unit -> unit ffitype */
value ctypes_void_ffitype(value _)
{
  return CTYPES_FROM_PTR(&ffi_type_void);
}

#define Struct_ffitype_val(v) (*(ffi_type **)Data_custom_val(v))

/* allocate_struct_ffitype : int -> managed_buffer */
value ctypes_allocate_struct_ffitype(value nargs_)
{
  CAMLparam1(nargs_);

  int nargs = Int_val(nargs_);
  /* Space for the struct ffi_type plus a null-terminated array of arguments */
  int size = sizeof (ffi_type) + (1 + nargs) * sizeof (ffi_type *);
  CAMLlocal1(block);
  block = ctypes_allocate(Val_int(1), Val_int(size));
  ffi_type *struct_type = Struct_ffitype_val(block);
  struct_type->size = 0;
  struct_type->alignment = 0;
  struct_type->type = FFI_TYPE_STRUCT;
  struct_type->elements = (ffi_type **)(struct_type + 1);
  struct_type->elements[nargs] = NULL;
  CAMLreturn (block);
}

/* struct_ffitype_set_argument : managed_buffer -> int -> _ ffitype -> unit */
value ctypes_struct_ffitype_set_argument(value struct_type_, value index_, value arg_)
{
  int index = Int_val(index_);
  ffi_type *arg = CTYPES_TO_PTR(arg_);

  ffi_type *struct_type = Struct_ffitype_val(struct_type_);
  struct_type->elements[index] = arg;
  return Val_unit;
}


extern void ctypes_check_ffi_status(ffi_status);

 /* complete_struct_type : managed_buffer -> unit */
value ctypes_complete_structspec(value struct_type_)
{
  ffi_cif _dummy_cif;
  ffi_type *struct_type = Struct_ffitype_val(struct_type_);

  ffi_status status = ffi_prep_cif(&_dummy_cif, FFI_DEFAULT_ABI, 0,
                                   struct_type, NULL);
  
  ctypes_check_ffi_status(status);

  return Val_unit;
}
