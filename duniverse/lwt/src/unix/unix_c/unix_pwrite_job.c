/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <string.h>

#include "lwt_unix.h"

struct job_pwrite {
    struct lwt_unix_job job;
    int fd;
    long length;
    off_t file_offset;
    long result;
    int error_code;
    char buffer[];
};

static void worker_pwrite(struct job_pwrite *job)
{
    job->result = pwrite(job->fd, job->buffer, job->length, job->file_offset);
    job->error_code = errno;
}

static value result_pwrite(struct job_pwrite *job)
{
    long result = job->result;
    LWT_UNIX_CHECK_JOB(job, result < 0, "pwrite");
    lwt_unix_free_job(&job->job);
    return Val_long(result);
}

CAMLprim value lwt_unix_pwrite_job(value val_fd, value val_string,
                                   value val_file_offset, value val_offset,
                                   value val_length)
{
    long length = Long_val(val_length);
    LWT_UNIX_INIT_JOB(job, pwrite, length);
    job->fd = Int_val(val_fd);
    job->length = length;
    job->file_offset = Long_val(val_file_offset);
    memcpy(job->buffer, String_val(val_string) + Long_val(val_offset), length);
    return lwt_unix_alloc_job(&(job->job));
}
#endif
