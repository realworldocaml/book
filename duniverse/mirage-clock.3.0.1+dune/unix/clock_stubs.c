/*
 * Copyright (c) 2015 Matt Gray <matthew.thomas.gray@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_MIRAGE_CLOCK_DARWIN

#elif defined (__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_MIRAGE_CLOCK_POSIX
 #endif

#elif defined(_WIN32)
  #define OCAML_MIRAGE_CLOCK_WINDOWS

#endif

#if defined(OCAML_MIRAGE_CLOCK_DARWIN)

#include <time.h>
#include <sys/time.h>
#include <mach/clock.h>
#include <mach/mach.h>
#include <mach/mach_time.h>

/* PCLOCK */

CAMLprim value ocaml_posix_clock_gettime_s_ns (value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(time_s_ns);
  time_s_ns = caml_alloc(2, 0);
  struct timeval now;
  if (gettimeofday(&now, NULL) == -1) { caml_failwith("gettimeofday"); }

  Store_field(time_s_ns, 0, caml_copy_int64(now.tv_sec));
  Store_field(time_s_ns, 1, caml_copy_int64(now.tv_usec * 1000L));

  CAMLreturn (time_s_ns);
}

CAMLprim value ocaml_posix_clock_period_ns (value unit)
{
  return caml_copy_int64 (0L);
}

/* MCLOCK */

CAMLprim value ocaml_monotonic_clock_elapsed_ns (value unit)
{
  static uint64_t start = 0L;
  static mach_timebase_info_data_t scale;
  if (start == 0L)
  {
    start = mach_absolute_time ();
    mach_timebase_info (&scale);
    if (scale.denom == 0) { scale.numer = 0; scale.denom = 1; }
  }

  uint64_t now = mach_absolute_time ();
  return caml_copy_int64 (((now - start) * scale.numer) / scale.denom);
}

#elif defined(OCAML_MIRAGE_CLOCK_POSIX)

#include <time.h>
#include <stdint.h>

/* PCLOCK */

CAMLprim value ocaml_posix_clock_gettime_s_ns (value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(time_s_ns);
  time_s_ns = caml_alloc(2, 0);
  struct timespec now;
  if (clock_gettime(CLOCK_REALTIME, &now) == -1) { caml_failwith("clock_gettime(CLOCK_REALTIME, ..)"); }

  Store_field(time_s_ns, 0, caml_copy_int64(now.tv_sec));
  Store_field(time_s_ns, 1, caml_copy_int64(now.tv_nsec));

  CAMLreturn (time_s_ns);
}

CAMLprim value ocaml_posix_clock_period_ns (value unit)
{
  struct timespec clock_period;
  if (clock_getres (CLOCK_REALTIME, &clock_period)) return caml_copy_int64 (0L);
  return caml_copy_int64 ((uint64_t) clock_period.tv_nsec);
}

/* MCLOCK */

CAMLprim value ocaml_monotonic_clock_elapsed_ns (value unit)
{
  static struct timespec start = { 0, 0 };
  if (start.tv_sec == 0)
  {
    if (clock_gettime (CLOCK_MONOTONIC, &start)) return caml_copy_int64 (0L);
  }
  struct timespec now;
  if (clock_gettime (CLOCK_MONOTONIC, &now)) return caml_copy_int64 (0L);
  return caml_copy_int64 ((uint64_t)(now.tv_sec - start.tv_sec) *
                          (uint64_t)1000000000 +
                          (uint64_t)(now.tv_nsec - start.tv_nsec));
}

#elif defined(OCAML_MIRAGE_CLOCK_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static double performance_frequency;
static void set_performance_frequency(void)
{
  LARGE_INTEGER t_freq;
  if (!QueryPerformanceFrequency(&t_freq)) { caml_failwith("QueryPerformanceFrequency()"); }
  performance_frequency = (1000000000.0 / t_freq.QuadPart);
}

/* MCLOCK */

CAMLprim value ocaml_monotonic_clock_elapsed_ns (value unit)
{
  (void) unit;
  static LARGE_INTEGER start;
  if (performance_frequency == 0.0) {
    set_performance_frequency();
  }
  if ( start.QuadPart == 0 )
  {
    if (!QueryPerformanceCounter(&start)) { caml_failwith("QueryPerformanceCounter(&start)"); }
  }
  static LARGE_INTEGER now;
  if ( !QueryPerformanceCounter(&now)) { caml_failwith("QueryPerformanceCounter(&now)"); }
  uint64_t ret = (now.QuadPart - start.QuadPart) * performance_frequency;
  return caml_copy_int64(ret);
}

/* PCLOCK */

#ifndef CLOCK_REALTIME
#define CLOCK_REALTIME 1
#endif

#define POW10_7                 10000000
#define DELTA_EPOCH_IN_100NS    INT64_C(116444736000000000)

typedef void (WINAPI *GetSystemTimeAsFileTime_t)(LPFILETIME lpSystemTimeAsFileTime);
static GetSystemTimeAsFileTime_t i_GetSystemTimeAsFileTime = GetSystemTimeAsFileTime;
static int clock_gettime_init_called = 0;

static void clock_gettime_init(void) {
  /* Use GetSystemTimePreciseAsFileTime when available */
  HMODULE h ;
  clock_gettime_init_called = 1;
  h = LoadLibrary("kernel32.dll");
  if (h != NULL) {
    GetSystemTimeAsFileTime_t proc = (GetSystemTimeAsFileTime_t)GetProcAddress(h, "GetSystemTimePreciseAsFileTime");
    if (proc != NULL) {
      i_GetSystemTimeAsFileTime = proc;
    }
    else {
      FreeLibrary(h);
    }
  }
}

CAMLprim value ocaml_posix_clock_gettime_s_ns (value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(time_s_ns);

  uint64_t t;
  FILETIME ft;
  ULARGE_INTEGER lft;
  time_s_ns = caml_alloc(2, 0);

  if ( clock_gettime_init_called == 0 ){ clock_gettime_init(); }
  i_GetSystemTimeAsFileTime(&ft);
  lft.LowPart  = ft.dwLowDateTime;
  lft.HighPart = ft.dwHighDateTime;
  t = lft.QuadPart - DELTA_EPOCH_IN_100NS;

  Store_field(time_s_ns, 0, caml_copy_int64(t / POW10_7));
  Store_field(time_s_ns, 1, caml_copy_int64(((int) (t % POW10_7)) * 100));

  CAMLreturn (time_s_ns);
}

CAMLprim value ocaml_posix_clock_period_ns (value unit)
{
  (void) unit;
  if (performance_frequency == 0.0) {
    set_performance_frequency();
  }
  if ( performance_frequency <= 0.0 ) return caml_copy_int64 (0L);
  value ret;
  value p = caml_copy_int64(performance_frequency);
  Begin_roots1(p);
  ret = caml_alloc_small(1,0);
  Field(ret,0) = p;
  End_roots();
  return ret;
}

#else
#warning Mirage PCLOCK - unsupported platform
CAMLprim value ocaml_posix_clock_gettime_s_ns (value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(time_s_ns);
  time_s_ns = caml_alloc(2, 0);

  Store_field(time_s_ns, 0, caml_copy_int64(0));
  Store_field(time_s_ns, 1, caml_copy_int64(0L));

  CAMLreturn (time_s_ns);
}

CAMLprim value ocaml_posix_clock_period_ns (value unit)
{
  return caml_copy_int64 (0L);
}
#endif
