/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if defined(LWT_ON_WINDOWS)

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

CAMLprim value lwt_unix_pwrite(value fd, value buf, value vfile_offset,
                               value vofs, value vlen)
{
    intnat ofs, len, file_offset, written;
    DWORD numbytes, numwritten;
    DWORD err = 0;

    Begin_root(buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    file_offset = Long_val(vfile_offset);
    written = 0;
    if (len > 0) {
        numbytes = len;
        if (Descr_kind_val(fd) == KIND_SOCKET) {
            caml_invalid_argument("Lwt_unix.pwrite");
        } else {
            HANDLE h = Handle_val(fd);
            OVERLAPPED overlapped;
            memset( &overlapped, 0, sizeof(overlapped));
            overlapped.OffsetHigh = (DWORD)(file_offset >> 32);
            overlapped.Offset = (DWORD)(file_offset & 0xFFFFFFFFLL);
            if (!WriteFile(h, &Byte(buf, ofs), numbytes, &numwritten,
                           &overlapped))
                err = GetLastError();
        }
        if (err) {
            win32_maperr(err);
            uerror("pwrite", Nothing);
        }
        written = numwritten;
    }
    End_roots();
    return Val_long(written);
}
#endif
