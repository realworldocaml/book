#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_add_numbers(value v_arg1, value v_arg2)
{
  int v1 = Int_val(v_arg1);
  int v2 = Int_val(v_arg2);
  printf("From C:     %d + %d\n", v1, v2);
  return Val_int(v1+v2);
}
