#include <math.h>
#include <stdint.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>


#if defined(__GNUC__)
__attribute__((optimize("no-math-errno")))
#endif
int64_t caml_float_iround_half_to_even_unboxed(double x)
{
  return llrint(x);
}

CAMLprim value caml_float_iround_half_to_even(value x)
{
  return caml_copy_int64(caml_float_iround_half_to_even_unboxed(Double_val(x)));
}

double caml_float_min_unboxed(double x, double y)
{
  return x < y ? x : y;
}

CAMLprim value caml_float_min(value x, value y)
{
  return caml_copy_double(caml_float_min_unboxed(Double_val(x), Double_val(y)));
}

double caml_float_max_unboxed(double x, double y)
{
  return x > y ? x : y;
}

CAMLprim value caml_float_max(value x, value y)
{
  return caml_copy_double(caml_float_max_unboxed(Double_val(x), Double_val(y)));
}

double caml_float_round_down_unboxed(double x)
{
  return floor(x);
}

CAMLprim value caml_float_round_down(value x)
{
  return caml_copy_double(caml_float_round_down_unboxed(Double_val(x)));
}

double caml_float_round_up_unboxed(double x)
{
  return ceil(x);
}

CAMLprim value caml_float_round_up(value x)
{
  return caml_copy_double(caml_float_round_up_unboxed(Double_val(x)));
}

double caml_float_round_towards_zero_unboxed(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return trunc(x);
#else
  return (x >= 0.0)? floor(x) : ceil(x);
#endif
}

CAMLprim value caml_float_round_towards_zero(value x)
{
  return caml_copy_double(caml_float_round_towards_zero_unboxed(Double_val(x)));
}

double caml_float_round_current_unboxed(double x)
{
  return rint(x);
}

CAMLprim value caml_float_round_current(value x)
{
  return caml_copy_double(caml_float_round_current_unboxed(Double_val(x)));
}
