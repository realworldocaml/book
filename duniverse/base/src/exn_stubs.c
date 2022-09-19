#define CAML_INTERNALS
#ifndef CAML_NAME_SPACE
#define CAML_NAME_SPACE
#endif
/* If CAML_NAME_SPACE is not defined, then legacy names like
   [backtrace_last_exn] are in scope, which can lead to confusing errors.
   It's cleaner to disable those names. */
#include <caml/mlvalues.h>
#include <caml/backtrace.h>

CAMLprim value Base_clear_caml_backtrace_pos () {
  caml_backtrace_pos = 0;
  return Val_unit;
}

CAMLprim value Base_caml_exn_is_most_recent_exn (value exn) {
  return Val_bool(caml_backtrace_last_exn == exn);
}
