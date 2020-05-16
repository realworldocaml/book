/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <caml/memory.h>
#include <caml/alloc.h>

#include "ctypes_complex_compatibility.h"
#include "ctypes_complex_stubs.h"

static value allocate_complex_value(double r, double i)
{
  value v = caml_alloc(2 * sizeof(double), Double_array_tag);
  Store_double_field(v, 0, r);
  Store_double_field(v, 1, i);
  return v;
}

/* ctypes_copy_float_complex : float _Complex -> Complex.t */
value ctypes_copy_float_complex(float _Complex c)
{
  return allocate_complex_value(ctypes_compat_crealf(c), ctypes_compat_cimagf(c));
}

/* ctypes_copy_double_complex : double _Complex -> Complex.t */
value ctypes_copy_double_complex(double _Complex c)
{
  return allocate_complex_value(ctypes_compat_creal(c), ctypes_compat_cimag(c));
}

/* ctypes_float_complex_val : Complex.t -> float _Complex */
float _Complex ctypes_float_complex_val(value v)
{
  return ctypes_compat_make_complexf(Double_field(v, 0), Double_field(v, 1));
}

/* ctypes_double_complex_val : Complex.t -> double _Complex */
double _Complex ctypes_double_complex_val(value v)
{
  return ctypes_compat_make_complex(Double_field(v, 0), Double_field(v, 1));
}
