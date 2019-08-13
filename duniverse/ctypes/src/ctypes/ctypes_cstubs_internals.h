/*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_CSTUBS_INTERNALS_H
#define CTYPES_CSTUBS_INTERNALS_H

/* Types and functions used by generated C code. */

#include "ctypes_primitives.h"
#include "ctypes_complex_stubs.h"
#include "ctypes_ldouble_stubs.h"
#include "ctypes_raw_pointer.h"
#include "ctypes_managed_buffer_stubs.h"
#include <caml/threads.h>
#define CTYPES_PTR_OF_OCAML_STRING(s) \
  (String_val(Field(s, 1)) + Long_val(Field(s, 0)))
#define Ctypes_val_char(c) \
  (Val_int((c + 256) % 256))
#define CTYPES_PAIR_WITH_ERRNO(v)

#include <caml/memory.h>
#include <errno.h>
static inline value ctypes_pair_with_errno(value p)
{
  CAMLparam1 (p);
  CAMLlocal1 (v);
  v = caml_alloc_tuple(2);
  Store_field (v, 0, p);
  Store_field (v, 1, ctypes_copy_sint(errno));
  CAMLreturn (v);
}

#if defined(__MINGW32__) || defined(__MINGW64__)
#define ctypes_printf __mingw_printf
#else
#define ctypes_printf printf
#endif

#endif /* CTYPES_CSTUBS_INTERNALS_H */
