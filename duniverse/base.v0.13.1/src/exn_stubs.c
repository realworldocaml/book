#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/backtrace.h>

CAMLprim value Base_clear_caml_backtrace_pos () {
  caml_backtrace_pos = 0;
  return Val_unit;
}
