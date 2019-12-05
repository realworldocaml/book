/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include "ctypes_primitives.h"

#define _XOPEN_SOURCE 500
#include <caml/mlvalues.h>

#include <assert.h>

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#if (!defined _WIN32 || defined __CYGWIN__) && !defined MINIOS
#include <pthread.h>
#endif
#include <time.h>

#include <stdint.h>

#define EXPOSE_TYPEINFO_COMMON(TYPENAME,STYPENAME)           \
  value ctypes_typeof_ ## TYPENAME(value unit)               \
  {                                                          \
    enum ctypes_arithmetic_type underlying =                 \
      CTYPES_CLASSIFY_ARITHMETIC_TYPE(STYPENAME);            \
    return Val_int(underlying);                              \
  }

#define EXPOSE_ALIGNMENT_COMMON(TYPENAME,STYPENAME)          \
  value ctypes_alignmentof_ ## TYPENAME(value unit)          \
  {                                                          \
    struct s { char c; STYPENAME t; };                       \
    return Val_int(offsetof(struct s, t));                   \
  }

#define EXPOSE_TYPESIZE_COMMON(TYPENAME,STYPENAME)           \
  value ctypes_sizeof_ ## TYPENAME(value unit)               \
  {                                                          \
    return Val_int(sizeof(STYPENAME));                       \
  }

#if !defined _WIN32 || defined __CYGWIN__
  #define UNDERSCORE(X) X
#else
  #define UNDERSCORE(X) _## X
#endif

#define EXPOSE_TYPEINFO(X) EXPOSE_TYPEINFO_COMMON(X, X)
#define EXPOSE_TYPEINFO_S(X) EXPOSE_TYPEINFO_COMMON(X, UNDERSCORE(X))
#define EXPOSE_TYPESIZE(X) EXPOSE_TYPESIZE_COMMON(X, X)
#define EXPOSE_TYPESIZE_S(X) EXPOSE_TYPESIZE_COMMON(X, UNDERSCORE(X))
#define EXPOSE_ALIGNMENT(X) EXPOSE_ALIGNMENT_COMMON(X, X)
#define EXPOSE_ALIGNMENT_S(X) EXPOSE_ALIGNMENT_COMMON(X, UNDERSCORE(X))

#ifdef __NetBSD__
/* NetBSD defines these types as macros, which expands to the wrong thing
 * in the EXPOSE_* macros above. I have no idea how to prevent cpp from
 * expanding macro arguments, so just hack around it for now. */
#undef off_t
#undef mode_t
#undef pid_t
typedef __off_t off_t;
typedef __mode_t mode_t;
typedef __pid_t pid_t;
#endif

EXPOSE_TYPEINFO(clock_t)
EXPOSE_TYPEINFO_S(dev_t)
EXPOSE_TYPEINFO_S(ino_t)
EXPOSE_TYPEINFO_S(mode_t)
EXPOSE_TYPEINFO_S(off_t)
EXPOSE_TYPEINFO_S(pid_t)
EXPOSE_TYPEINFO(ssize_t)
EXPOSE_TYPEINFO(time_t)
EXPOSE_TYPEINFO(useconds_t)
#if !defined _WIN32 || defined __CYGWIN__
  EXPOSE_TYPEINFO(nlink_t)
#else
  /* the mingw port of fts uses an int for nlink_t */
  EXPOSE_TYPEINFO_COMMON(nlink_t, int)
#endif


EXPOSE_TYPESIZE_S(sigset_t)
EXPOSE_ALIGNMENT_S(sigset_t)
