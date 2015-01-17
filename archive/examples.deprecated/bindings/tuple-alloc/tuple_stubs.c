#include <stdio.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value
caml_add_numbers(value v_arg1, value v_arg2)
{
  CAMLparam2(v_arg1, v_arg2);
  CAMLlocal1(v_res);
  int v1 = Int_val(v_arg1);
  int v2 = Int_val(v_arg2);
  printf("From C:     %d+%d=%d\n", v1, v2, (v1+v2));
  v_res = caml_alloc_tuple(3);
  Store_field(v_res, 0, Val_int(v1));
  Store_field(v_res, 1, Val_int(v2));
  Store_field(v_res, 2, caml_copy_int32(v1 + v2));
  CAMLreturn(v_res);
}
