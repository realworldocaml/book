/*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define Val_none Val_int(0)
#define OCAML_PTIME_DAY_MAX 2932896 // See Ptime.max
#define OCAML_PTIME_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Ptime_clock: " ERR)); } \
  while (0)

/* Detect platforms and call in their includes */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_PTIME_DARWIN
  #include <time.h>
  #include <sys/time.h>

#elif defined(__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_PTIME_POSIX
   #include <time.h>
 #endif

#elif defined(_WIN32)
  #define OCAML_PTIME_WIN
  #include <windows.h>

#else
  #warning OCaml Ptime_clock module: unsupported platform
  #define OCAML_PTIME_UNSUPPORTED
#endif


/* Clock now */

#if defined(OCAML_PTIME_POSIX)

CAMLprim value ocaml_ptime_clock_now_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (pair);
  struct timespec now;

  if (clock_gettime (CLOCK_REALTIME, &now))
    OCAML_PTIME_RAISE_SYS_ERROR ("can't determine current time");

  /* Make sure to return valid Ptime.t values. */

  /* We only handle valid timespec structs as per POSIX def (§2.8.5 in 2013) */
  if (now.tv_nsec < 0 || now.tv_nsec > 999999999)
    OCAML_PTIME_RAISE_SYS_ERROR ("invalid tv_nsec in timespec");

  /* To make it easier, we do not lie, this can't possibly be now.
     See e.g. Ptime.Span.of_int_s if this is a problem. */
  if (now.tv_sec < 0)
    OCAML_PTIME_RAISE_SYS_ERROR ("negative tv_sec in timespec");

  int d = now.tv_sec / 86400;
  if (d > OCAML_PTIME_DAY_MAX)
    OCAML_PTIME_RAISE_SYS_ERROR ("can't represent timespec in Ptime.t");

  pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (d));
  Store_field (pair, 1,
               /* Given the above checks, in the right range for Ptime */
               caml_copy_int64 ((now.tv_sec % 86400) * 1000000000000L +
                                (now.tv_nsec * 1000L)));
  CAMLreturn (pair);
}

#elif defined(OCAML_PTIME_DARWIN)

CAMLprim value ocaml_ptime_clock_now_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (pair);
  struct timeval now;

  gettimeofday(&now, NULL);

  /* Make sure to return valid Ptime.t values. */

  /* We only handle reasonable timevals (not specified in POSIX it seems) */
  if (now.tv_usec < 0 || now.tv_usec > 999999)
    OCAML_PTIME_RAISE_SYS_ERROR ("unreasonable tv_usec in timeval");

  /* To make it easier, we do not lie, this can't possibly be now.
     See e.g. Ptime.Span.of_int_s if this is a problem. */
  if (now.tv_sec < 0)
    OCAML_PTIME_RAISE_SYS_ERROR ("negative tv_sec in timeval");

  int d = now.tv_sec / 86400;
  if (d > OCAML_PTIME_DAY_MAX)
    OCAML_PTIME_RAISE_SYS_ERROR ("can't represent timeval in Ptime.t");

  pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (d));
  Store_field (pair, 1,
               /* Given the above checks, in the right range for Ptime */
               caml_copy_int64 ((now.tv_sec % 86400) * 1000000000000L +
                                (now.tv_usec * 1000000L)));
  CAMLreturn (pair);
}

#elif defined(OCAML_PTIME_WIN)

CAMLprim value ocaml_ptime_clock_now_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (pair);
  long sec, usec;
  SYSTEMTIME stime;
  FILETIME ftime;
  ULARGE_INTEGER time;

  GetSystemTime (&stime);
  SystemTimeToFileTime (&stime, &ftime);
  time.LowPart = ftime.dwLowDateTime;
  time.HighPart = ftime.dwHighDateTime;

#define EPOCH (116444736000000000ULL)
  sec = (long)((time.QuadPart - EPOCH) / 10000000L);
#undef EPOCH
  usec = (long)(stime.wMilliseconds * 1000);

  if (usec < 0 || usec > 999999)
    OCAML_PTIME_RAISE_SYS_ERROR ("unreasonable usec in FILETIME");

  if (sec < 0)
    OCAML_PTIME_RAISE_SYS_ERROR ("negative sec in FILETIME");

  int d = sec / 86400;
  if (d > OCAML_PTIME_DAY_MAX)
    OCAML_PTIME_RAISE_SYS_ERROR ("can't represent FILETIME in Ptime.t");

  pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (d));
  Store_field (pair, 1,
               caml_copy_int64 ((sec % 86400) * 1000000000000L +
                                (usec * 1000000L)));
  CAMLreturn (pair);
}

#else

CAMLprim value ocaml_ptime_clock_now_d_ps (value unit)
{
  OCAML_PTIME_RAISE_SYS_ERROR ("unsupported platform");
}

#endif


/* Clock period */

#if defined(OCAML_PTIME_POSIX)

CAMLprim value ocaml_ptime_clock_period_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal2 (some, pair);
  struct timespec res;

  if (clock_getres (CLOCK_REALTIME, &res)) CAMLreturn (Val_none);

  /* Make sure to return valid Ptime.Span.t values. */

  /* We only handle valid timespec structs as per POSIX def (§2.8.5 in 2013) */
  if (res.tv_nsec < 0 || res.tv_nsec > 999999999) CAMLreturn (Val_none);

  /* Negative periods are dubious */
  if (res.tv_sec < 0) CAMLreturn (Val_none);

  some = caml_alloc (1, 0);
  pair = caml_alloc (2, 0);
  Store_field (some, 0, pair);
  Store_field (pair, 0, Val_int (res.tv_sec / 86400));
  Store_field (pair, 1,
               /* Given the above checks, in the right range for Ptime */
               caml_copy_int64 ((res.tv_sec % 86400) * 1000000000000L +
                                (res.tv_nsec * 1000L)));
  CAMLreturn (some);
}

#else /* OCAML_PTIME_DARWIN || OCAML_PTIME_WIN || OCAML_PTIME_UNSUPPORTED */

CAMLprim value ocaml_ptime_clock_period_d_ps (value unit)
{ return Val_none; }

#endif


/* Timezone offset (local time - UTC time) */

#if defined(OCAML_PTIME_DARWIN) || defined (OCAML_PTIME_POSIX)

CAMLprim value ocaml_ptime_clock_current_tz_offset_s (value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(some);
  struct tm *tm;

  time_t now_utc = time (NULL);
  if (now_utc == (time_t)-1) return Val_none;

  tm = localtime (&now_utc);
  if (tm == NULL) return Val_none;
  struct tm local = *tm;

  tm = gmtime (&now_utc);
  if (tm == NULL) return Val_none;
  struct tm utc = *tm;

  int dd = local.tm_yday - utc.tm_yday;
  int dh = local.tm_hour - utc.tm_hour;
  int dm = dh * 60 + (local.tm_min - utc.tm_min);
  dm = (dd ==  1 || dd < -1 /* year wrap */) ? dm + (24 * 60) :
       (dd == -1 || dd >  1 /* year wrap */) ? dm - (24 * 60) :
       dm /* same day */;

  some = caml_alloc (1, 0);
  Store_field (some, 0, Val_int (dm * 60));
  CAMLreturn(some);
}

#elif defined(OCAML_PTIME_WIN)

CAMLprim value ocaml_ptime_clock_current_tz_offset_s (value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(some);
  TIME_ZONE_INFORMATION tz;
  int bias;

  DWORD ret = GetTimeZoneInformation(&tz);
  if (ret == TIME_ZONE_ID_UNKNOWN)
    bias = tz.Bias;
  else if (ret == TIME_ZONE_ID_STANDARD)
    bias = tz.Bias + tz.StandardBias;
  else if (ret == TIME_ZONE_ID_DAYLIGHT)
    bias = tz.Bias + tz.DaylightBias;
  else {
    OCAML_PTIME_RAISE_SYS_ERROR("GetTimeZoneInformation failed");
  }

  some = caml_alloc (1, 0);
  /* Note that on Windows 'bias' is defined as (UTC - localtime),
     while ptime uses (localtime - UTC) */
  Store_field (some, 0, Val_int (-bias * 60));
  CAMLreturn(some);
}

#else /* OCAML_PTIME_UNSUPPORTED */

CAMLprim value ocaml_ptime_clock_current_tz_offset_s (value unit)
{ return Val_none; }

#endif

/*---------------------------------------------------------------------------
   Copyright (c) 2014 The ptime programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
   --------------------------------------------------------------------------*/
