/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if defined(LWT_ON_WINDOWS)

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include "lwt_unix.h"

struct job_pwrite {
    struct lwt_unix_job job;
    HANDLE handle;
    DWORD length;
    DWORD Offset;
    DWORD OffsetHigh;
    DWORD result;
    DWORD error_code;
    char buffer[];
};

static void worker_pwrite(struct job_pwrite *job)
{
    OVERLAPPED overlapped;
    memset( &overlapped, 0, sizeof(overlapped));
    overlapped.OffsetHigh = job->OffsetHigh;
    overlapped.Offset = job->Offset;
    if (!WriteFile(job->handle, job->buffer, job->length, &(job->result),
                   &overlapped))
    job->error_code = GetLastError();
}

static value result_pwrite(struct job_pwrite *job)
{
    value result;
    DWORD error = job->error_code;
    if (error) {
        lwt_unix_free_job(&job->job);
        win32_maperr(error);
        uerror("pwrite", Nothing);
    }
    result = Val_long(job->result);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_pwrite_job(value val_fd, value val_string,
                                   value val_file_offset, value val_offset,
                                   value val_length)
{
    struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
    long length = Long_val(val_length);
    DWORDLONG file_offset = Long_val(val_file_offset);
    if (fd->kind != KIND_HANDLE) {
        caml_invalid_argument("Lwt_unix.pwrite");
    } else {
        LWT_UNIX_INIT_JOB(job, pwrite, length);
        job->handle = fd->fd.handle;
        memcpy(
            job->buffer, String_val(val_string) + Long_val(val_offset), length);
        job->length = length;
        job->OffsetHigh = (DWORD)(file_offset >> 32);
        job->Offset = (DWORD)(file_offset & 0xFFFFFFFFLL);
        job->error_code = 0;
        return lwt_unix_alloc_job(&(job->job));
    }
}
#endif
