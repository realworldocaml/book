#ifdef __MACH__
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <unistd.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

// (c) Daniel Bünzli

static mach_timebase_info_data_t s = { 0 };

CAMLprim value
clock_mach_init(value unit)
{
  if (mach_timebase_info (&s) != KERN_SUCCESS)
    caml_raise_sys_error (caml_copy_string("clock_mach_init: mach_timebase_info () failed"));
  if (s.denom == 0)
    caml_raise_sys_error (caml_copy_string("clock_mach_init: mach_timebase_info_data.denom is 0"));

  return Val_unit;
}

CAMLprim value
clock_mach_get_time(value unit)
{
  uint64_t now;

  now = mach_absolute_time();

  return copy_int64(now * s.numer / s.denom);
}
