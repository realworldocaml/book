/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#ifdef HAVE_ACCEPT4

#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

CAMLprim value lwt_unix_accept4(value vcloexec, value vnonblock, value vsock)
{
    CAMLparam3(vcloexec, vnonblock, vsock);
    CAMLlocal2(vaddr, res);

    union sock_addr_union addr;
    socklen_param_type addr_len;
    int cloexec = Is_some(vcloexec) && Bool_val(Some_val(vcloexec)) ? SOCK_CLOEXEC : 0;
    int nonblock = Bool_val(vnonblock) ? SOCK_NONBLOCK : 0;
    addr_len = sizeof(addr);

    int fd =
        accept4(Int_val(vsock), &addr.s_gen, &addr_len, cloexec | nonblock);
    if (fd == -1)
        uerror("accept", Nothing);

    vaddr = alloc_sockaddr(&addr, addr_len, fd);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = Val_int(fd);
    Field(res, 1) = vaddr;

    CAMLreturn(res);
}

#else

#include "lwt_unix.h"
LWT_NOT_AVAILABLE3(unix_accept4)

#endif
