#include <caml/mlvalues.h>

extern int caml_backtrace_pos;

CAMLprim value Base_clear_caml_backtrace_pos () {
  caml_backtrace_pos = 0;
  return Val_unit;
}
