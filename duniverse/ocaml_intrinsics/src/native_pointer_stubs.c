#include "ext_pointer.h"

intnat caml_ext_pointer_as_native_pointer (value n)
{
  return (intnat) caml_ext_pointer_decode(n);
}

value caml_native_pointer_load_immediate (intnat p)
{
  return *((value *)p);
}

value caml_native_pointer_store_immediate(intnat p, value v)
{
  *((value *)p) = v;
  return Val_unit;
}

double caml_native_pointer_load_unboxed_float (intnat p)
{
  return *((double *)p);
}

value caml_native_pointer_store_unboxed_float (intnat p, double d)
{
  *((double *)p) = d;
  return Val_unit;
}


int64_t caml_native_pointer_load_unboxed_int64 (intnat p)
{
  return *((int64_t *)p);
}

value caml_native_pointer_store_unboxed_int64 (intnat p, int64_t d)
{
  *((int64_t *)p) = d;
  return Val_unit;
}

int32_t caml_native_pointer_load_unboxed_int32 (intnat p)
{
  return *((int32_t *)p);
}

value caml_native_pointer_store_unboxed_int32 (intnat p, int32_t d)
{
  *((int32_t *)p) = d;
  return Val_unit;
}

intnat caml_native_pointer_load_unboxed_nativeint (intnat p)
{
  return *((intnat *)p);
}

value caml_native_pointer_store_unboxed_nativeint (intnat p, intnat d)
{
  *((intnat *)p) = d;
  return Val_unit;
}

CAMLprim value caml_ext_pointer_as_native_pointer_bytecode (value n)
{
  return caml_copy_nativeint(caml_ext_pointer_as_native_pointer(n));
}

CAMLprim value caml_native_pointer_load_immediate_bytecode (value p)
{
  return caml_native_pointer_load_immediate(Nativeint_val(p));
}

CAMLprim value caml_native_pointer_store_immediate_bytecode (value p, value v)
{
  return caml_native_pointer_store_immediate(Nativeint_val(p), v);
}

CAMLprim value caml_native_pointer_load_unboxed_float_bytecode (value p)
{
  return caml_copy_double(caml_native_pointer_load_unboxed_float(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_unboxed_float_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_float(Nativeint_val(p), Double_val(v));
}

CAMLprim value caml_native_pointer_load_unboxed_int64_bytecode (value p)
{
  return caml_copy_int64(caml_native_pointer_load_unboxed_int64(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_unboxed_int64_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_int64(Nativeint_val(p), Int64_val(v));
}

CAMLprim value caml_native_pointer_load_unboxed_int32_bytecode (value p)
{
  return caml_copy_int32(caml_native_pointer_load_unboxed_int32(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_unboxed_int32_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_int32(Nativeint_val(p), Int32_val(v));
}

CAMLprim value caml_native_pointer_load_unboxed_nativeint_bytecode (value p)
{
  return caml_copy_nativeint(caml_native_pointer_load_unboxed_nativeint(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_unboxed_nativeint_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_nativeint(Nativeint_val(p), Nativeint_val(v));
}

CAMLprim value caml_native_pointer_load_untagged_int_bytecode (value p)
{
  return Val_long(caml_native_pointer_load_unboxed_nativeint(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_untagged_int_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_nativeint(Nativeint_val(p), Long_val(v));
}



