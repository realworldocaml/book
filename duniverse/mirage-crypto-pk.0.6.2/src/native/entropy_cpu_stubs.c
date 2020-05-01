/*
 * Copyright (c) 2015-2016 David Kaloper Meršinjak
 */

#include <caml/mlvalues.h>

#include "mirage_crypto.h"

#if defined (__i386__) || defined (__x86_64__)
#define __x86__

#include <x86intrin.h>
#include <cpuid.h>

/* because clang... */
#if !defined(bit_RDSEED)
#define bit_RDSEED 0x00040000
#endif

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

#if defined (__arm__)

/* Replace with `read_virtual_count` from MiniOS when that symbol
 * gets exported. */
static inline uint32_t read_virtual_count () {
  uint32_t c_lo, c_hi;
  __asm__ __volatile__("mrrc p15, 1, %0, %1, c14":"=r"(c_lo), "=r"(c_hi));
  return c_lo;
}
#endif /* arm */
#if defined (__aarch64__)
#define	isb()		__asm __volatile("isb" : : : "memory")
static inline uint64_t read_virtual_count(void)
{
  uint64_t c;
  isb();
  __asm__ __volatile__("mrs %0, cntvct_el0":"=r"(c));
  return c;
}
#endif /* aarch64 */

enum cpu_rng_t {
  RNG_NONE   = 0,
  RNG_RDRAND = 1,
  RNG_RDSEED = 2,
};

static enum cpu_rng_t __cpu_rng = RNG_NONE;

#define RETRIES 10

static void detect () {
#if defined (__x86__)

  unsigned int sig, eax, ebx, ecx, edx;
  int max = __get_cpuid_max (0, &sig);
  random_t r = 0;

  if (max < 1) return;

  if (sig == signature_INTEL_ebx || sig == signature_AMD_ebx) {
    __cpuid (1, eax, ebx, ecx, edx);
    if (ecx & bit_RDRND)
      /* AMD Ryzen 3000 bug where RDRAND always returns -1
         https://arstechnica.com/gadgets/2019/10/how-a-months-old-amd-microcode-bug-destroyed-my-weekend/ */
      for (int i = 0; i < RETRIES; i++)
        if (_rdrand_step(&r) == 1 && r != (random_t) (-1)) {
          __cpu_rng = RNG_RDRAND;
          break;
        }
    if (max > 7) {
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      if (ebx & bit_RDSEED)
        /* RDSEED could return -1 as well, thus we test it here as well
           https://www.reddit.com/r/Amd/comments/cmza34/agesa_1003_abb_fixes_rdrandrdseed/ */
        for (int i = 0; i < RETRIES; i++)
          if (_rdseed_step(&r) == 1 && r != (random_t) (-1)) {
            __cpu_rng = RNG_RDSEED;
            break;
          }
    }
  }
#endif
}

CAMLprim value caml_cycle_counter (value __unused(unit)) {
#if defined (__x86__)
  return Val_long (__rdtsc ());
#elif defined (__arm__) || defined (__aarch64__)
  return Val_long (read_virtual_count ());
#else
#error ("No known cycle-counting instruction.")
#endif
}

CAMLprim value caml_cpu_checked_random (value __unused(unit)) {
#if defined (__x86__)
  random_t r = 0;
  int ok = 0;
  int i = RETRIES;
  switch (__cpu_rng) {
  case RNG_RDSEED:
    do { ok = _rdseed_step (&r); _mm_pause (); } while ( !(ok | !--i) );
    break;
  case RNG_RDRAND:
    do { ok = _rdrand_step (&r); } while ( !(ok | !--i) );
    break;
  case RNG_NONE:
    break;
  }
  return Val_long(r);
#else
  /* ARM: CPU-assisted randomness here. */
  return Val_long (0);
#endif
}

CAMLprim value caml_cpu_unchecked_random (value __unused(unit)) {
#if defined (__x86__)
  random_t r = 0;
  /* rdrand/rdseed may fail (and return CR = 0) if insufficient entropy is
     available (or the hardware DRNG is in the middle of reseeding).

     we could handle these by retrying in a loop - which would be
     computationally expensive, but since this code is run whenever the Lwt
     event loop is entered, and only used to feed entropy into the pool, it is
     fine to add not-so-random entropy.
 */
  switch (__cpu_rng) {
  case RNG_RDSEED:
    _rdseed_step (&r);
    break;
  case RNG_RDRAND:
    _rdrand_step (&r);
    break;
  case RNG_NONE:
    break;
  }
  return Val_long (r);
#else
  /* ARM: CPU-assisted randomness here. */
  return Val_long (0);
#endif
}

CAMLprim value caml_cpu_rng_type (value __unused(unit)) {
  return Val_int (__cpu_rng);
}

CAMLprim value caml_entropy_detect (value __unused(unit)) {
  detect ();
  return Val_unit;
}

/*
 * XXX
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
