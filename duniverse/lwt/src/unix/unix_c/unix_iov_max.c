/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#define _GNU_SOURCE

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <limits.h>

CAMLprim value lwt_unix_iov_max(value unit)
{
    CAMLparam1(unit);
    CAMLlocal1(res);

#ifdef IOV_MAX
    res = caml_alloc(1, 0);
    Store_field(res, 0, Val_int(IOV_MAX));
#else
    res = Val_int(0);
#endif

    CAMLreturn(res);
}

#endif
