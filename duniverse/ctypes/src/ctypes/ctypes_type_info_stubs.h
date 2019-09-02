/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_TYPE_INFO_STUBS_H
#define CTYPES_TYPE_INFO_STUBS_H

#include <caml/mlvalues.h>

/* Read a C value from a block of memory */
/* read : 'a prim -> raw_pointer -> 'a */
extern value ctypes_read(value ctype, value buffer);

/* Write a C value to a block of memory */
/* write : 'a prim -> 'a -> raw_pointer -> unit */
extern value ctypes_write(value ctype, value v, value buffer);

#endif /* CTYPES_TYPE_INFO_STUBS_H */
