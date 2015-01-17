#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_add_numbers(value v_arg1, value v_arg2)
{
  printf("From C:     %d + %d\n", v_arg1, v_arg2);
  return v_arg1 + v_arg2;
}
