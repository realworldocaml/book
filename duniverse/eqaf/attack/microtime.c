/* Copyright (c) 2018 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md */

#include <caml/mlvalues.h>

#if defined (__x86_64__)
#define __x86__

#elif defined (__i686__)
#warning Unmark uses unboxed native ints for low-level counters. Proceed with extreme caution.
/* We can still do short benchmarks, tho. */
#define __x86__

#endif

#if defined (__x86__)
#include <x86intrin.h>
#endif

#include <time.h>

#define __unit value unit __attribute__((unused))
#define nsec 1000000000

CAMLprim value caml_rdtsc (__unit) {
#if defined (__x86__)
  return Val_long (__rdtsc ());
#else
#warning Disabling RDTSC.
  return Val_long (0);
#endif
}

/* This will break on some, or all, of: Windows, macOS, Mirage.
 * Please fill in the missing definitions.
 */
CAMLprim value caml_microtime (__unit) {
  struct timespec ts;
  clock_gettime (CLOCK_MONOTONIC, &ts);
  return Val_long ((intnat) ts.tv_sec * nsec + (intnat) ts.tv_nsec);
}
