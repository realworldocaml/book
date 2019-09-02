/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <stdint.h>
#include <string.h>

#include "ctypes_raw_pointer.h"
#include "ctypes_managed_buffer_stubs.h"

static void finalize_free(value v)
{
  free(*((void **)Data_custom_val(v)));
}

static int compare_pointers(value l_, value r_)
{
  /* pointer comparison */
  intptr_t l = (intptr_t)*(void **)Data_custom_val(l_);
  intptr_t r = (intptr_t)*(void **)Data_custom_val(r_);
  return (l > r) - (l < r);
}

static intnat hash_address(value l)
{
  /* address hashing */
  return (intnat)*(void **)Data_custom_val(l);
}

static struct custom_operations managed_buffer_custom_ops = {
  "ocaml-ctypes:managed_buffer",
  finalize_free,
  compare_pointers,
  hash_address,
  /* Managed buffers are not serializable. */
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

/* copy_bytes : void * -> size_t -> managed_buffer */
value ctypes_copy_bytes(void *src, size_t size)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&managed_buffer_custom_ops, sizeof(void*), 0, 1);
  void *dst = malloc(size);
  if (dst == NULL && size != 0) caml_raise_out_of_memory();
  *(void **)Data_custom_val(block) = memcpy(dst, src, size);
  CAMLreturn(block);
}

/* allocate : int -> int -> managed_buffer */
value ctypes_allocate(value count_, value size_)
{
  CAMLparam2(count_, size_);
  intnat size = Long_val(size_);
  intnat count = Long_val(count_);
  CAMLlocal1(block);
  block = caml_alloc_custom(&managed_buffer_custom_ops, sizeof(void*), 0, 1);
  // libc's calloc guarantees the memory is zero-filled
  // malloc may not be used internally
  void *p = calloc(count, size);
  if (p == NULL && count != 0 && size != 0) caml_raise_out_of_memory();
  void **d = (void **)Data_custom_val(block);
  *d = p;
  CAMLreturn(block);
}

/* block_address : managed_buffer -> immediate_pointer */
value ctypes_block_address(value managed_buffer)
{
  return CTYPES_FROM_PTR(*(void **)Data_custom_val(managed_buffer));
}
