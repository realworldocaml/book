/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#include "ctypes_raw_pointer.h"

#ifndef Caml_ba_layout_val
/* Caml_ba_layout_val was introduced when the representation of layout
   values changed from an integer to a GADT.  Up to that point the 
   OCaml values c_layout and fortran_layout had the same values as
   the C constants CAML_BA_C_LAYOUT and CAML_BA_FORTRAN_LAYOUT */
#define Caml_ba_layout_val(v) (Int_val(v))
#endif

/* address : 'b -> pointer */
value ctypes_bigarray_address(value ba)
{
  return CTYPES_FROM_PTR(Caml_ba_data_val(ba));
}

/* _view : ('a, 'b) kind -> dims:int array -> fatptr -> 'l layout ->
           ('a, 'b, 'l) Bigarray.Genarray.t */
value ctypes_bigarray_view(value kind_, value dims_, value ptr_, value layout_)
{
  int kind = Int_val(kind_);
  int layout = Caml_ba_layout_val(layout_);
  int ndims = Wosize_val(dims_);
  intnat dims[CAML_BA_MAX_NUM_DIMS];
  int i;
  for (i = 0; i < ndims; i++) {
    dims[i] = Long_val(Field(dims_, i));
  }
  int flags = kind | layout | CAML_BA_EXTERNAL;
  void *data = CTYPES_ADDR_OF_FATPTR(ptr_);
  return caml_ba_alloc(flags, ndims, data, dims);
}
