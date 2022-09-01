/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#define _GNU_SOURCE

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "lwt_unix.h"

#if defined(HAVE_GET_CREDENTIALS_LINUX)
#define CREDENTIALS_TYPE struct ucred
#define CREDENTIALS_FIELD(id) id
#elif defined(HAVE_GET_CREDENTIALS_NETBSD)
#define CREDENTIALS_TYPE struct sockcred
#define CREDENTIALS_FIELD(id) sc_##id
#elif defined(HAVE_GET_CREDENTIALS_OPENBSD)
#define CREDENTIALS_TYPE struct sockpeercred
#define CREDENTIALS_FIELD(id) id
#elif defined(HAVE_GET_CREDENTIALS_FREEBSD)
#define CREDENTIALS_TYPE struct cmsgcred
#define CREDENTIALS_FIELD(id) cmsgcred_##id
#endif

#if defined(CREDENTIALS_TYPE)

CAMLprim value lwt_unix_get_credentials(value fd)
{
    CAMLparam1(fd);
    CAMLlocal1(res);
    CREDENTIALS_TYPE cred;
    socklen_t cred_len = sizeof(cred);

    if (getsockopt(Int_val(fd), SOL_SOCKET, SO_PEERCRED, &cred, &cred_len) ==
        -1)
        uerror("get_credentials", Nothing);

    res = caml_alloc_tuple(3);
    Store_field(res, 0, Val_int(cred.CREDENTIALS_FIELD(pid)));
    Store_field(res, 1, Val_int(cred.CREDENTIALS_FIELD(uid)));
    Store_field(res, 2, Val_int(cred.CREDENTIALS_FIELD(gid)));
    CAMLreturn(res);
}

#elif defined(HAVE_GETPEEREID)

CAMLprim value lwt_unix_get_credentials(value fd)
{
    CAMLparam1(fd);
    CAMLlocal1(res);
    uid_t euid;
    gid_t egid;

    if (getpeereid(Int_val(fd), &euid, &egid) == -1)
        uerror("get_credentials", Nothing);

    res = caml_alloc_tuple(3);

    Store_field(res, 0, Val_int(-1));
    Store_field(res, 1, Val_int(euid));
    Store_field(res, 2, Val_int(egid));
    CAMLreturn(res);
}

#else

LWT_NOT_AVAILABLE1(unix_get_credentials)

#endif


#endif
