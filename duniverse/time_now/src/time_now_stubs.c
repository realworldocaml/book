#include <caml/memory.h>
#include <time.h>
#include "ocaml_utils.h"
#include "config.h"

#define NANOS_PER_SECOND 1000000000

#if defined(JSC_POSIX_TIMERS)

/* Note: this is imported noalloc if (and only if) ARCH_SIXTYFOUR is defined.
 * This is OK because caml_alloc_int63 doesn't actually allocate in that case. */
CAMLprim value time_now_nanoseconds_since_unix_epoch_or_zero()
{
  struct timespec ts;

  if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
    return caml_alloc_int63(0);
  else
    return caml_alloc_int63(NANOS_PER_SECOND * (uint64_t)ts.tv_sec + (uint64_t)ts.tv_nsec);
}

#else

#include <sys/types.h>
#include <sys/time.h>

CAMLprim value time_now_nanoseconds_since_unix_epoch_or_zero()
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1)
    return caml_alloc_int63(0);
  else
    return caml_alloc_int63(NANOS_PER_SECOND * (uint64_t)tp.tv_sec + (uint64_t)tp.tv_usec * 1000);
}

#endif
