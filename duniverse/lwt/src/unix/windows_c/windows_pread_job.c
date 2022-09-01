/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if defined(LWT_ON_WINDOWS)

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include "lwt_unix.h"

struct job_pread {
    struct lwt_unix_job job;
    HANDLE handle;
    DWORD length;
    DWORD Offset;
    DWORD OffsetHigh;
    DWORD result;
    DWORD error_code;
    value string;
    DWORD offset;
    char buffer[];
};

static void worker_pread(struct job_pread *job)
{
    OVERLAPPED overlapped;
    memset( &overlapped, 0, sizeof(overlapped));
    overlapped.OffsetHigh = job->OffsetHigh;
    overlapped.Offset = job->Offset;
    if (!ReadFile(job->handle, job->buffer, job->length, &(job->result),
                  &overlapped))
    job->error_code = GetLastError();
}

static value result_pread(struct job_pread *job)
{
    value result;
    DWORD error = job->error_code;
    if (error == ERROR_BROKEN_PIPE) {
        /* The write handle for an anonymous pipe has been closed. We match the
           Unix behavior, and treat this as a zero-read instead of a Unix_error.
           See OCaml PR #4790. */
        job->result = 0;
    } else if (error) {
        caml_remove_generational_global_root(&job->string);
        lwt_unix_free_job(&job->job);
        win32_maperr(error);
        uerror("pread", Nothing);
    }
    memcpy(Bytes_val(job->string) + job->offset, job->buffer, job->result);
    result = Val_long(job->result);
    caml_remove_generational_global_root(&job->string);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_pread_job(value val_fd, value val_string,
                                  value val_file_offset, value val_offset,
                                  value val_length)
{
    struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
    long length = Long_val(val_length);
    DWORDLONG file_offset = Long_val(val_file_offset);
    if (fd->kind != KIND_HANDLE) {
        caml_invalid_argument("Lwt_unix.pread");
    } else {
        LWT_UNIX_INIT_JOB(job, pread, length);
        job->handle = fd->fd.handle;
        job->length = length;
        job->OffsetHigh = (DWORD)(file_offset >> 32);
        job->Offset = (DWORD)(file_offset & 0xFFFFFFFFLL);
        job->error_code = 0;
        job->string = val_string;
        job->offset = Long_val(val_offset);
        caml_register_generational_global_root(&(job->string));
        return lwt_unix_alloc_job(&(job->job));
    }
}
#endif
