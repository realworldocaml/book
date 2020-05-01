/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_RAW_POINTER_STUBS_H
#define CTYPES_RAW_POINTER_STUBS_H

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdint.h>

#define CTYPES_FROM_PTR(P) caml_copy_nativeint((intptr_t)P)
#define CTYPES_TO_PTR(I) ((void *)Nativeint_val(I))

/* CTYPES_ADDR_OF_FATPTR : _ Ctypes_ptr.Fat.t -> void * */
#define CTYPES_ADDR_OF_FATPTR(P) CTYPES_TO_PTR(Field(P, 1))

#endif /* CTYPES_RAW_POINTER_STUBS_H */
