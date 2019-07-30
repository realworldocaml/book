#define _GNU_SOURCE

#include <assert.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdbool.h>
#include "ocaml_utils.h"

/* this type must be kept strictly in sync with the OCaml type */
typedef enum {
  CALL_ABORT = 0,
  CALL_GCORE = 1,
  DUMP_TYPE_LIMIT = 2 /* used to catch mismatch between this type and the OCaml type */
} core_dump_type;

static int num_ticks = 0; /* updated by regular Async job */
static int core_dump_count = 0;

#define CORE_FILENAME_MAX_LEN (4 + 1 + 10 + 1)

/* max pid is 2^22 on 64 bit systems, which is 7 digits + 1 for terminating NULL. */
#define PID_STR_MAX_LEN 10

static void dump_core (core_dump_type dump_type)
{
  pid_t main_pid = getpid ();
  pid_t fork_pid;
  int status;
  char gcore_path[] = "/usr/bin/gcore";
  char pid_str[PID_STR_MAX_LEN];
  char core_filename[CORE_FILENAME_MAX_LEN]; /* core.<count>. */
  char *args[] = { NULL, NULL, NULL, NULL, NULL };
  char *env[] = { NULL };

  core_dump_count = core_dump_count + 1;

  switch (dump_type) {
    case CALL_ABORT:
      abort ();
      break;
    case CALL_GCORE:
      fork_pid = fork ();
      if (fork_pid) {
        waitpid (fork_pid, &status, 0);
      } else {
        assert (snprintf (core_filename, CORE_FILENAME_MAX_LEN, "core.%i",
                          core_dump_count)
                < CORE_FILENAME_MAX_LEN);
        assert (snprintf (pid_str, PID_STR_MAX_LEN, "%d", main_pid) < PID_STR_MAX_LEN);
        args[0] = gcore_path;
        args[1] = "-o";
        args[2] = core_filename;
        args[3] = pid_str;
        execve(gcore_path, args, env);
      };
      break;
    case DUMP_TYPE_LIMIT:
      caml_leave_blocking_section();
      caml_failwith ("bug in dump_core_on_job_delay_dump_core");
  };
}

CAMLprim value dump_core_on_job_delay_dump_core (value v_dump_type)
{
  CAMLparam1 (v_dump_type);
  core_dump_type dump_type = Int_val (v_dump_type);
  if ( dump_type >= DUMP_TYPE_LIMIT )
    caml_failwith ("bug in dump_core_on_job_delay_dump_core");
  dump_core (dump_type);
  CAMLreturn (Val_unit);
}

CAMLprim value dump_core_on_job_delay_watch (value v_dump_if_delayed_by,
                                             value v_dump_type)
{
  CAMLparam2 (v_dump_if_delayed_by, v_dump_type);

  useconds_t dump_if_delayed_by  = Double_val (v_dump_if_delayed_by) * 1000 * 1000;
  core_dump_type dump_type       = CALL_ABORT;
  int last_num_ticks_seen        = num_ticks;
  bool already_dumped_this_cycle = false;

  dump_type = Int_val (v_dump_type);
  if ( dump_type >= DUMP_TYPE_LIMIT )
    caml_failwith ("bug in dump_core_on_job_delay_watch");

  /* We give up the CAML lock because we intend to run the following
     loop for the life of the program. */
  caml_enter_blocking_section();

  for (;;) {
    usleep (dump_if_delayed_by);

    /* If [last_num_ticks_seen] is the same as the last time we woke
       up, then the Async tick job has been delayed. */
    if (last_num_ticks_seen == num_ticks) {
      if (!already_dumped_this_cycle) {
        already_dumped_this_cycle = true;
        dump_core (dump_type);
      }
    } else {
      /* Otherwise, if the count has changed, and we reset everything. */
      already_dumped_this_cycle = false;
      last_num_ticks_seen = num_ticks;
    };
  };

  caml_leave_blocking_section();

  CAMLreturn (Val_unit);
}

CAMLprim value dump_core_on_job_delay_tick (value v_unit)
{
  /* Not strictly needed, but it keeps the compiler from complaining
     about an unused [v_unit] arg, and there really isn't a need to make
     this as fast as possible. */
  CAMLparam1 (v_unit);
  num_ticks = num_ticks + 1;
  CAMLreturn (Val_unit);
}


