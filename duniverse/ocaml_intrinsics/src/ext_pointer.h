#include "caml/config.h"
#include "caml/alloc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

static inline void *caml_ext_pointer_decode(value v)
{
  // Always called with an ocaml representation of int:
  // guarantees that the least significant bit is set.
  return (void*)((intnat)v - 1);
}
