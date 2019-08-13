/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdint.h>

#define Val_none Val_int(0)
#define OCAML_MTIME_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Mtime_clock: " ERR)); } \
  while (0)

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_MTIME_DARWIN

#elif defined(__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_MTIME_POSIX
 #endif
#endif

/* Darwin */

#if defined(OCAML_MTIME_DARWIN)

#include <mach/mach_time.h>

static mach_timebase_info_data_t scale = {0};

void ocaml_mtime_clock_init_scale (void)
{
  if (mach_timebase_info (&scale) != KERN_SUCCESS)
    OCAML_MTIME_RAISE_SYS_ERROR ("mach_timebase_info () failed");

  if (scale.denom == 0)
    OCAML_MTIME_RAISE_SYS_ERROR ("mach_timebase_info_data.denom is 0");
}

CAMLprim value ocaml_mtime_clock_elapsed_ns (value unit)
{
  static uint64_t start = 0L;
  if (start == 0L) { start = mach_absolute_time (); }
  if (scale.denom == 0) { ocaml_mtime_clock_init_scale (); }
  uint64_t now = mach_absolute_time ();
  return caml_copy_int64 (((now - start) * scale.numer) / scale.denom);
}

CAMLprim value ocaml_mtime_clock_now_ns (value unit)
{
  if (scale.denom == 0) { ocaml_mtime_clock_init_scale (); }
  uint64_t now = mach_absolute_time ();
  return caml_copy_int64 ((now * scale.numer) / scale.denom);
}

CAMLprim value ocaml_mtime_clock_period_ns (value unit)
{ return Val_none; }

/* POSIX */

#elif defined(OCAML_MTIME_POSIX)

#include <time.h>

CAMLprim value ocaml_mtime_clock_elapsed_ns (value unit)
{
  static struct timespec start = {0};
  struct timespec now;

  if (start.tv_sec == 0)
  {
    if (clock_gettime (CLOCK_MONOTONIC, &start))
      OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");
  }

  if (clock_gettime (CLOCK_MONOTONIC, &now))
    OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");

  return caml_copy_int64 ((uint64_t)(now.tv_sec - start.tv_sec) *
                          (uint64_t)1000000000 +
                          (uint64_t)(now.tv_nsec - start.tv_nsec));
}

CAMLprim value ocaml_mtime_clock_now_ns (value unit)
{
  struct timespec now;

  if (clock_gettime (CLOCK_MONOTONIC, &now))
    OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");

  return caml_copy_int64 ((uint64_t)(now.tv_sec) *
                          (uint64_t)1000000000 +
                          (uint64_t)(now.tv_nsec));
}

CAMLprim value ocaml_mtime_clock_period_ns (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (some);
  struct timespec res;

  if (clock_getres (CLOCK_MONOTONIC, &res)) { CAMLreturn (Val_none); }

  /* We only handle valid timespec structs as per POSIX def (§2.8.5 in 2013) */
  if (res.tv_nsec < 0 || res.tv_nsec > 999999999) CAMLreturn (Val_none);

  /* Negative periods are dubious */
  if (res.tv_sec < 0) CAMLreturn (Val_none);

  some = caml_alloc (1, 0);
  Store_field (some, 0,
               caml_copy_int64 ((uint64_t)(res.tv_sec) *
                                (uint64_t)1000000000 +
                                (uint64_t)(res.tv_nsec)));
  CAMLreturn (some);
}

/* Unsupported */

#else

#warning OCaml Mtime_clock module: unsupported platform

CAMLprim value ocaml_mtime_clock_elapsed_ns (value unit)
{ OCAML_MTIME_RAISE_SYS_ERROR ("unsupported platform"); }

CAMLprim value ocaml_mtime_clock_now_ns (value unit)
{ OCAML_MTIME_RAISE_SYS_ERROR ("unsupported platform"); }

CAMLprim value ocaml_mtime_clock_period_ns (value unit)
{ OCAML_MTIME_RAISE_SYS_ERROR ("unsupported platform"); }

#endif

/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

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
  ---------------------------------------------------------------------------*/
