/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if defined(LWT_ON_WINDOWS)

#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include "lwt_unix.h"

struct job_bytes_read {
    struct lwt_unix_job job;
    union {
        HANDLE handle;
        SOCKET socket;
    } fd;
    int kind;
    char *buffer;
    DWORD length;
    DWORD result;
    DWORD error_code;
    value ocaml_buffer;
};

static void worker_bytes_read(struct job_bytes_read *job)
{
    if (job->kind == KIND_SOCKET) {
        int ret;
        ret = recv(job->fd.socket, job->buffer, job->length, 0);
        if (ret == SOCKET_ERROR) job->error_code = WSAGetLastError();
        job->result = ret;
    } else {
        if (!ReadFile(job->fd.handle, job->buffer, job->length, &(job->result),
                      NULL))
            job->error_code = GetLastError();
    }
}

static value result_bytes_read(struct job_bytes_read *job)
{
    value result;
    DWORD error = job->error_code;
    caml_remove_generational_global_root(&job->ocaml_buffer);
    if (error == ERROR_BROKEN_PIPE) {
        /* The write handle for an anonymous pipe has been closed. We match the
           Unix behavior, and treat this as a zero-read instead of a Unix_error.
           See OCaml PR #4790. */
        job->result = 0;
    } else if (error) {
        lwt_unix_free_job(&job->job);
        win32_maperr(error);
        uerror("bytes_read", Nothing);
    }
    result = Val_long(job->result);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_bytes_read_job(value val_fd, value val_buffer,
                                       value val_offset, value val_length)
{
    struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
    LWT_UNIX_INIT_JOB(job, bytes_read, 0);
    job->kind = fd->kind;
    if (fd->kind == KIND_HANDLE)
        job->fd.handle = fd->fd.handle;
    else
        job->fd.socket = fd->fd.socket;
    job->buffer = (char *)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
    job->length = Long_val(val_length);
    job->error_code = 0;
    job->ocaml_buffer = val_buffer;
    caml_register_generational_global_root(&job->ocaml_buffer);
    return lwt_unix_alloc_job(&(job->job));
}
#endif
