#include "ext_pointer.h"

CAMLprim value caml_ext_pointer_load_immediate (value p)
{
  return *((value *)(caml_ext_pointer_decode(p)));
}

CAMLprim value caml_ext_pointer_store_immediate (value p, value v)
{
  *((value *)(caml_ext_pointer_decode(p))) = v;
  return Val_unit;
}

double caml_ext_pointer_load_unboxed_float (value p)
{
  return *((double *)(caml_ext_pointer_decode(p)));
}

value caml_ext_pointer_store_unboxed_float (value p, double d)
{
  *((double *)(caml_ext_pointer_decode(p))) = d;
  return Val_unit;
}

int64_t caml_ext_pointer_load_unboxed_int64 (value p)
{
  return *((int64_t *)(caml_ext_pointer_decode(p)));
}

value caml_ext_pointer_store_unboxed_int64 (value p, int64_t d)
{
  *((int64_t *)(caml_ext_pointer_decode(p))) = d;
  return Val_unit;
}

int32_t caml_ext_pointer_load_unboxed_int32 (value p)
{
  return *((int32_t *)(caml_ext_pointer_decode(p)));
}

value caml_ext_pointer_store_unboxed_int32 (value p, int32_t d)
{
  *((int32_t *)(caml_ext_pointer_decode(p))) = d;
  return Val_unit;
}

intnat caml_ext_pointer_load_unboxed_nativeint (value p)
{
  return *((intnat *)(caml_ext_pointer_decode(p)));
}

value caml_ext_pointer_store_unboxed_nativeint (value p, intnat d)
{
  *((intnat *)(caml_ext_pointer_decode(p))) = d;
  return Val_unit;
}

CAMLprim value caml_ext_pointer_load_unboxed_float_bytecode (value p)
{
  return caml_copy_double(caml_ext_pointer_load_unboxed_float(p));
}

CAMLprim value caml_ext_pointer_store_unboxed_float_bytecode (value p, value v)
{
  return caml_ext_pointer_store_unboxed_float(p, Double_val(v));
}

CAMLprim value caml_ext_pointer_load_unboxed_int64_bytecode (value p)
{
  return caml_copy_int64(caml_ext_pointer_load_unboxed_int64(p));
}

CAMLprim value caml_ext_pointer_store_unboxed_int64_bytecode (value p, value v)
{
  return caml_ext_pointer_store_unboxed_int64(p, Int64_val(v));
}

CAMLprim value caml_ext_pointer_load_unboxed_int32_bytecode (value p)
{
  return caml_copy_int32(caml_ext_pointer_load_unboxed_int32(p));
}

CAMLprim value caml_ext_pointer_store_unboxed_int32_bytecode (value p, value v)
{
  return caml_ext_pointer_store_unboxed_int32(p, Int32_val(v));
}

CAMLprim value caml_ext_pointer_load_unboxed_nativeint_bytecode (value p)
{
  return caml_copy_nativeint(caml_ext_pointer_load_unboxed_nativeint(p));
}

CAMLprim value caml_ext_pointer_store_unboxed_nativeint_bytecode (value p, value v)
{
  return caml_ext_pointer_store_unboxed_nativeint(p, Nativeint_val(v));
}

CAMLprim value caml_ext_pointer_load_untagged_int (value p)
{
  return Val_long(caml_ext_pointer_load_unboxed_nativeint(p));
}

CAMLprim value caml_ext_pointer_store_untagged_int (value p, value v)
{
  return caml_ext_pointer_store_unboxed_nativeint(p, Long_val(v));
}
