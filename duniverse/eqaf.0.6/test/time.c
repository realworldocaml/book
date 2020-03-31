#include <caml/memory.h>
#include <caml/fail.h>

#include <time.h>

#define __unused(x) x __attribute((unused))
#define __unit() value __unused(unit)

CAMLprim value
caml_rdtsc(__unit ())
{
  unsigned hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return Val_int(((unsigned long long) lo) | (((unsigned long long) hi) << 32));
}

uint64_t
caml_time(__unit ())
{
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts))
    caml_invalid_argument("caml_time");
  return ((uint64_t) ts.tv_sec * (uint64_t) 1000000000LL + (uint64_t) ts.tv_nsec); 
}
