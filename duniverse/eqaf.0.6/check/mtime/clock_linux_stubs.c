#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

CAMLprim value
clock_linux_get_time(value vunit)
{
  struct timespec ts;

  if (clock_gettime(CLOCK_MONOTONIC, &ts))
    caml_invalid_argument("bechamel.clock: unsupported clock");

  return copy_int64(ts.tv_sec * 1000000000LL + ts.tv_nsec);
}
