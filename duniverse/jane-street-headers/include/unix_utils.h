#ifndef UNIX_UTILS_H
#define UNIX_UTILS_H

#define _GNU_SOURCE

#include <sys/uio.h>
#include <errno.h>
#include "ocaml_utils.h"
#include "core_params.h"

/* Utility definitions */

static inline char * get_bstr(value v_bstr, value v_pos)
{
  return (char *) Caml_ba_data_val(v_bstr) + Long_val(v_pos);
}

static inline struct iovec * copy_iovecs(size_t *total_len, value v_iovecs, int n)
{
  struct iovec *iovecs = caml_stat_alloc(sizeof(struct iovec) * n);
  for (--n; n >= 0; --n) {
    struct iovec *iovec = &iovecs[n];
    value v_iovec = Field(v_iovecs, n);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    size_t iov_len = Long_val(Field(v_iovec, 2));
    iovec->iov_len = iov_len;
    *total_len += iov_len;
    iovec->iov_base = get_bstr(v_iov_base, v_iov_pos);
  }
  return iovecs;
}

static inline ssize_t jane_writev(int fd, struct iovec *iov, int iovcnt)
{
  ssize_t ret = writev(fd, iov, iovcnt);
  if (ret == -1 && errno == EINVAL && iovcnt == 0) {
    /* On OSX [count = 0] is an error, but it's not on Linux. For
       simplicity we always ignore [EINVAL] when [iovcnt] is 0. */
    ret = 0;
  }
  return ret;
}


/* if https://caml.inria.fr/mantis/view.php?id=5154 gets done we can remove these */
static inline value core_Val_some(value v)
{
  CAMLparam1(v);
  value some;

  some = caml_alloc_small(1, 0);
  Field(some, 0) = v;

  CAMLreturn(some);
}

#define core_Val_none Val_int(0)

#endif /* UNIX_UTILS_H */
