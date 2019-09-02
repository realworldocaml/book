#include <caml/mlvalues.h>

/* The default [Base_am_testing] value is [false]. [ppx_inline_test] overrides
   the default by linking against an implementation of [Base_am_testing] that 
   returns [true]. */
CAMLprim CAMLweakdef value Base_am_testing()
{
  return Val_false;
}
