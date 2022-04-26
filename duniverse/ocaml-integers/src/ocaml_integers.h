/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef INTEGERS_UNSIGNED_STUBS_H
#define INTEGERS_UNSIGNED_STUBS_H

#include <caml/mlvalues.h>

#include <stdint.h>

#ifndef OCAML_INTEGERS_INTERNAL
#ifdef __cplusplus
extern "C" {
#endif
CAMLextern value integers_copy_uint32(uint32_t u);
CAMLextern value integers_copy_uint64(uint64_t u);
#ifdef __cplusplus
}
#endif
#endif

#define Integers_val_uint8(t) ((Val_int((uint8_t)t)))
#define Integers_val_uint16(t) ((Val_int((uint16_t)t)))

#define Uint8_val(V) ((uint8_t)(Int_val(V)))
#define Uint16_val(V) ((uint16_t)(Int_val(V)))
#define Uint32_val(V) (*((uint32_t *) Data_custom_val(V)))
#define Uint64_val(V) (*((uint64_t *) Data_custom_val(V)))

#endif /* INTEGERS_UNSIGNED_STUBS_H */
