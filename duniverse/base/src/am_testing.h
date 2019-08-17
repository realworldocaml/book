#ifndef BASE_AM_TESTING_H
#define BASE_AM_TESTING_H
#include <caml/mlvalues.h>

CAMLprim value Base_am_testing ();

static inline int am_testing () {
  return Bool_val (Base_am_testing ());
}

#endif
