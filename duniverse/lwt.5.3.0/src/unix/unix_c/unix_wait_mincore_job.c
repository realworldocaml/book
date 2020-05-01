/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <string.h>

#include "lwt_unix.h"

#ifdef __CYGWIN__
LWT_NOT_AVAILABLE2(unix_wait_mincore_job)
#else
struct job_wait_mincore {
    struct lwt_unix_job job;
    value ocaml_buffer;
    char *ptr;
};

static void worker_wait_mincore(struct job_wait_mincore *job)
{
    /* Read the byte to force the kernel to fetch the page: */
    char dummy;
    memcpy(&dummy, job->ptr, 1);
}

static value result_wait_mincore(struct job_wait_mincore *job)
{
    caml_remove_generational_global_root(&job->ocaml_buffer);
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_wait_mincore_job(value val_buffer, value val_offset)
{
    LWT_UNIX_INIT_JOB(job, wait_mincore, 0);
    job->ptr = (char *)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
    job->ocaml_buffer = val_buffer;
    caml_register_generational_global_root(&job->ocaml_buffer);
    return lwt_unix_alloc_job(&(job->job));
}
#endif
#endif
