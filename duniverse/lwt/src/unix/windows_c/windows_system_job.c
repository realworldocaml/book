/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if defined(LWT_ON_WINDOWS)

#if OCAML_VERSION < 41300
#define CAML_INTERNALS
#endif
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/misc.h>
#include <caml/unixsupport.h>
#include <caml/osdeps.h>

#include "lwt_unix.h"

struct job_system {
    struct lwt_unix_job job;
    HANDLE handle;
};

static void worker_system(struct job_system *job)
{
    WaitForSingleObject(job->handle, INFINITE);
}

static value result_system(struct job_system *job)
{
    HANDLE handle = job->handle;
    DWORD code;
    DWORD err;
    lwt_unix_free_job(&job->job);
    if (!GetExitCodeProcess(handle, &code)) {
        err = GetLastError();
        CloseHandle(handle);
        win32_maperr(err);
        uerror("GetExitCodeProcess", Nothing);
    }
    CloseHandle(handle);
    return Val_int(code);
}

CAMLprim value lwt_unix_system_job(value cmdline)
{
    CAMLparam1(cmdline);
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    DWORD flags = CREATE_UNICODE_ENVIRONMENT;
    BOOL ret;

    char_os *cmdlines = caml_stat_strdup_to_os(String_val(cmdline));

    ZeroMemory(&si, sizeof(si));
    ZeroMemory(&pi, sizeof(pi));
    si.cb = sizeof(si);

    ret = CreateProcess(NULL, cmdlines, NULL, NULL, TRUE, flags,
                        NULL, NULL, &si, &pi);
    caml_stat_free(cmdlines);
    if (!ret) {
        win32_maperr(GetLastError());
        uerror("CreateProcess", Nothing);
    } else {
        LWT_UNIX_INIT_JOB(job, system, 0);
        CloseHandle(pi.hThread);
        job->handle = pi.hProcess;
        CAMLreturn(lwt_unix_alloc_job(&(job->job)));
    }
}
#endif
