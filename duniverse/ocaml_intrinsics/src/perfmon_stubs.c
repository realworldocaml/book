#include "caml/config.h"
#include "caml/alloc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

#if (defined(__i386__) || defined(__x86_64__))
#if defined(__GNUC__)
static uint64_t rdpmc(uint32_t c)
{
  uint32_t a, d;
  __asm__ __volatile__ ("rdpmc" : "=a" (a), "=d" (d) : "c" (c));
  return ((uint64_t)a) | (((uint64_t)d)<<32);
}

static uint64_t rdtsc()
{
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ((uint64_t)lo) | (((uint64_t)hi)<<32);
}
#elif defined(_MSC_VER)
#error "Functionality on Windows has not been tested"
#include <intrin.h>
#pragma intrinsic(__rdtsc)

static uint64_t rdtsc() { return __rdtsc(); }

// The rdpmc intrinsic is available in kernel mode only, and the
// routine is only available as an intrinsic.
static uint64_t rdpmc(__attribute__ ((unused)) uint32_t c) { return 0; }
#else
static uint64_t rdpmc(__attribute__ ((unused)) uint32_t c) { return 0; }
static uint64_t rdtsc() { return 0; }
#endif
#else
static uint64_t rdpmc(__attribute__ ((unused)) uint32_t c) { return 0; }
static uint64_t rdtsc() { return 0; }
#endif

uint64_t caml_rdpmc_unboxed(uint32_t v1)
{
  return rdpmc(v1);
}

CAMLprim value caml_rdpmc(value v1)
{
  return caml_copy_int64(rdpmc((uint32_t) (Int32_val(v1))));
}

uint64_t caml_rdtsc_unboxed(void)
{
  return rdtsc();
}

CAMLprim value caml_rdtsc(void)
{
  return caml_copy_int64(rdtsc());
}

