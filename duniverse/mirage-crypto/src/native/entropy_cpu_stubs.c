/*
 * Copyright (c) 2015-2016 David Kaloper Meršinjak
 */

#define _POSIX_C_SOURCE 199309L

#include <caml/mlvalues.h>

#include "mirage_crypto.h"

#if defined (__i386__) || defined (__x86_64__)
#include <x86intrin.h>

#if defined (__x86_64__)
#define random_t unsigned long long
#define _rdseed_step _rdseed64_step
#define _rdrand_step _rdrand64_step

#elif defined (__i386__)
#define random_t unsigned int
#define _rdseed_step _rdseed32_step
#define _rdrand_step _rdrand32_step

#endif
#endif /* __i386__ || __x86_64__ */

/* mc_cycle_counter specification and requirements

   Below the function mc_cycle_counter is defined (with different
   implementations on different platforms. Its lower 32 bits are used for
   entropy collection on every entry to the main event loop.

   The requirement for this function is that every call to it (from OCaml)
   should lead to a different output (in the lower 32 bits), and it should be
   unpredictable (since the timestamp / cpu cycle counter is fine-grained
   enough).

   The executable test/test_entropy.ml tests parts of the requirements by
   calling mc_cycle_counter 10 times and comparing the output to the previous
   output.
*/

#if defined (__arm__)
/*
 * The ideal timing source on ARM are the performance counters, but these are
 * presently masked by Xen.
 * It would work like this:

#if defined (__ARM_ARCH_7A__)
  // Disable counter overflow interrupts.
  __asm__ __volatile__ ("mcr p15, 0, %0, c9, c14, 2" :: "r"(0x8000000f));
  // Program the PMU control register.
  __asm__ __volatile__ ("mcr p15, 0, %0, c9, c12, 0" :: "r"(1 | 16));
  // Enable all counters.
  __asm__ __volatile__ ("mcr p15, 0, %0, c9, c12, 1" :: "r"(0x8000000f));

  // Read:
  unsigned int res;
  __asm__ __volatile__ ("mrc p15, 0, %0, c9, c13, 0": "=r" (res));
*/
#if defined(__ocaml_freestanding__) || defined(__ocaml_solo5__)
static inline uint32_t read_virtual_count ()
{
  uint32_t c_lo, c_hi;
  __asm__ __volatile__("mrrc p15, 1, %0, %1, c14":"=r"(c_lo), "=r"(c_hi));
  return c_lo;
}
#else
/* see https://github.com/mirage/mirage-crypto/issues/113 and
https://chromium.googlesource.com/external/gperftools/+/master/src/base/cycleclock.h
   The performance counters are only available in kernel mode (or if enabled via
   a kernel module also in user mode). Use clock_gettime as fallback.
 */
#include <time.h>
static inline uint32_t read_virtual_count ()
{
  uint32_t pmccntr;
  uint32_t pmuseren;
  uint32_t pmcntenset;
  // Read the user mode perf monitor counter access permissions.
  __asm__ __volatile__ ("mrc p15, 0, %0, c9, c14, 0" : "=r" (pmuseren));
  if (pmuseren & 1) {  // Allows reading perfmon counters for user mode code.
    __asm__ __volatile__ ("mrc p15, 0, %0, c9, c12, 1" : "=r" (pmcntenset));
    if (pmcntenset & 0x80000000ul) {  // Is it counting?
      __asm__ __volatile__ ("mrc p15, 0, %0, c9, c13, 0" : "=r" (pmccntr));
      // The counter is set up to count every 64th cycle
      return pmccntr;
    }
  }
  struct timespec now;
  clock_gettime (CLOCK_MONOTONIC, &now);
  return now.tv_nsec;
}
#endif /* __ocaml_freestanding__ || __ocaml_solo5__ */
#endif /* arm */

#if defined (__aarch64__)
#define	isb() __asm__ __volatile__("isb" : : : "memory")
static inline uint64_t read_virtual_count(void)
{
  uint64_t c;
  isb();
  __asm__ __volatile__("mrs %0, cntvct_el0":"=r"(c));
  return c;
}
#endif /* aarch64 */

#if defined (__powerpc64__)
/* from clang's builtin version and gperftools at
https://chromium.googlesource.com/external/gperftools/+/master/src/base/cycleclock.h
*/
static inline uint64_t read_cycle_counter(void)
{
  uint64_t rval;
  __asm__ __volatile__ ("mfspr %0, 268":"=r" (rval));
  return rval;
}
#endif

#if defined (__riscv) && (64 == __riscv_xlen)
static inline uint64_t rdcycle64(void)
{
  uint64_t rval;
  __asm__ __volatile__ ("rdcycle %0" : "=r" (rval));
  return rval;
}
#endif

#if defined (__s390x__)
static inline uint64_t getticks(void)
{
  uint64_t rval;
  __asm__ __volatile__ ("stck %0" : "=Q" (rval) : : "cc");
  return rval;
}
#endif

#if defined (__mips__)
static inline unsigned long get_count() {
  unsigned long count;
  __asm__ __volatile__ ("rdhwr %[rt], $2" : [rt] "=d" (count));
  return count;
}
#endif


CAMLprim value mc_cycle_counter (value __unused(unit)) {
#if defined (__i386__) || defined (__x86_64__)
  return Val_long (__rdtsc ());
#elif defined (__arm__) || defined (__aarch64__)
  return Val_long (read_virtual_count ());
#elif defined(__powerpc64__)
  return Val_long (read_cycle_counter ());
#elif defined(__riscv) && (64 == __riscv_xlen)
  return Val_long (rdcycle64 ());
#elif defined (__s390x__)
  return Val_long (getticks ());
#elif defined(__mips__)
  return Val_long (get_count());
#else
#error ("No known cycle-counting instruction.")
#endif
}

/* end of mc_cycle_counter */

enum cpu_rng_t {
  RNG_NONE   = 0,
  RNG_RDRAND = 1,
  RNG_RDSEED = 2,
};

static int __cpu_rng = RNG_NONE;

#define RETRIES 10

static void detect () {
#ifdef __mc_ENTROPY__
  random_t r = 0;

  if (mc_detected_cpu_features.rdrand)
    /* AMD Ryzen 3000 bug where RDRAND always returns -1
       https://arstechnica.com/gadgets/2019/10/how-a-months-old-amd-microcode-bug-destroyed-my-weekend/ */
    for (int i = 0; i < RETRIES; i++)
      if (_rdrand_step(&r) == 1 && r != (random_t) (-1)) {
        __cpu_rng = RNG_RDRAND;
        break;
      }

  if (mc_detected_cpu_features.rdseed)
    /* RDSEED could return -1, thus we test it here
       https://www.reddit.com/r/Amd/comments/cmza34/agesa_1003_abb_fixes_rdrandrdseed/ */
    for (int i = 0; i < RETRIES; i++)
      if (_rdseed_step(&r) == 1 && r != (random_t) (-1)) {
        __cpu_rng |= RNG_RDSEED;
        break;
      }
#endif
}

CAMLprim value mc_cpu_rdseed (value __unused(unit)) {
#ifdef __mc_ENTROPY__
  random_t r = 0;
  int ok = 0;
  int i = RETRIES;
  do { ok = _rdseed_step (&r); _mm_pause (); } while ( !(ok | !--i) );
  return Val_long(r);
#else
  /* ARM: CPU-assisted randomness here. */
  return Val_long (0);
#endif
}

CAMLprim value mc_cpu_rdrand (value __unused(unit)) {
#ifdef __mc_ENTROPY__
  random_t r = 0;
  int ok = 0;
  int i = RETRIES;
  do { ok = _rdrand_step (&r); } while ( !(ok | !--i) );
  return Val_long(r);
#else
  /* ARM: CPU-assisted randomness here. */
  return Val_long (0);
#endif
}

CAMLprim value mc_cpu_rng_type (value __unused(unit)) {
  return Val_int (__cpu_rng);
}

CAMLprim value mc_entropy_detect (value __unused(unit)) {
  detect ();
  return Val_unit;
}
