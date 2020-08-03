/* Core_unix support functions written in C. */

#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <assert.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/wait.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/threads.h>

#include "fork_exec.h"

#define PIPE_READ 0
#define PIPE_WRITE 1

/*
  If you want to turn on debugging you may use:

  #define fork_side_assert(v) assert(v)

  Note that assert uses non async-signal-safe functions. Do not leave this on
  in any production code
 */

#define fork_side_assert(ignore) ((void) 0)

#define SYSCALL(x)                                 \
  while ((x) == -1) {                              \
    if (errno != EINTR) {                          \
      report_errno_on_pipe (pfd[PIPE_WRITE],errno);\
    }                                              \
  }                                                \

#define NONINTR(x)                              \
  while ((x) == -1){ assert(errno == EINTR); }  \

/* Copy an ocaml string array in a c string array terminated by
   a null pointer the result need to be free'd with a stat_free
   It is a copy of cstringvect in the ocaml unix's module.
 */
static const char ** copy_stringvect(const value arg)
{
  const char ** res;
  mlsize_t size, i;

  size = Wosize_val(arg);
  res = (const char **) caml_stat_alloc((size + 1) * sizeof(const char *));
  for (i = 0; i < size; i++) res[i] = String_val(Field(arg, i));
  res[size] = NULL;
  return res;
}

#ifdef __GNUC__
/* Giving Gcc as much info as possible */
static void report_errno_on_pipe (int fd,int my_err) __attribute__((noreturn));
#endif

/*
  Write an int to an fd.
  This function is designed to be used on the fork side and therefor only uses
  async-signal-safe functions.
*/
static void report_errno_on_pipe (int fd, int my_err) {
  size_t offset = 0;
  ssize_t out_chars;
  while (offset < sizeof(int)) {;
    switch (out_chars=write (fd,
                             (char *) &my_err + offset,
                             sizeof(int) - offset)) {
    case -1:
      fork_side_assert (errno==EINTR);
      continue;
    default:
      offset += (size_t) out_chars;
    }
  }
  fork_side_assert (offset == sizeof(int));
  _exit(254);
}

static void clear_sigprocmask(void){
  sigset_t empty;
  (void) sigemptyset (&empty);
  (void) sigprocmask (SIG_SETMASK, &empty, (sigset_t *) NULL);
}


 /*
  Returns 0 if there was no errno printed on the pipe and -1 if there was one.
 */
static int errno_from_pipe (int fd,int *my_errno) {
   ssize_t in_chars;
   size_t offset = 0;
   while (true) {
     in_chars=read(fd,
                   (((char *) my_errno) + offset),
                   sizeof(int) - offset);
     switch (in_chars) {
     case -1 :
       assert (errno==EINTR);
       continue;
     case 0:
       if (offset == 0) {
         /* The fd was closed with nothing written to it; no error */
         return 0;
       };
       assert (offset == sizeof(int));
       return -1;
     default:
       offset += (size_t)in_chars;
     }
   };
}


/*
  [set_cloexec(fd,value)]
  Set the close on exec flag of fd to value. Is async-signal-safe.
  Returns 0 on success and -1 on error. Sets errno in case of errors.
 */
static int set_cloexec (int fd,int v) {
  int flags,new_flags;
  if ((flags = fcntl(fd, F_GETFD)) == -1) return -1;

  new_flags = (v ? flags | FD_CLOEXEC : flags & ~FD_CLOEXEC);

  if(new_flags == flags)
    return 0;

  return fcntl(fd, F_SETFD, new_flags);
}

CAMLprim value extended_ml_spawn
(
 value v_stdin, /* Fd to connect to the forked stdin... */
 value v_stdout,
 value v_stderr,
 value v_working_dir, /* A directory we want to chdir too. [String option] */
 value v_setuid, /* setuid on the fork side [int option] */
 value v_setgid, /* setgid on the fork side [int option] */
 value v_env, /* The Environment to set for execve. pass None to call an
                 execv instead. [string array option]*/
 value v_prog, /* Program name [string] */
 value v_args /* Full list of args passed to executable [string array] */
 )
{
  CAMLparam5(v_prog, v_args, v_stdin, v_stdout, v_stderr);
  CAMLxparam4(v_working_dir,v_setuid,v_setgid,v_env);
  int stdin_fd = Int_val (v_stdin);
  int stdout_fd = Int_val (v_stdout);
  int stderr_fd = Int_val (v_stderr);
  const char** envp  = NULL;
  int my_errno,forked_error;
  int pfd[2]; /* The pipe used to report errors.. */

  /* It's ok to hold pointers into the O'Caml heap, since the memory
     space gets duplicated upon the fork, during which we keep the
     O'Caml lock. */
  const char* prog = String_val(v_prog);
  const char* working_dir = NULL;

  pid_t child_pid;

  const char** args;

  /* We use a pipe to report errors on the forked side */
  if (pipe(pfd) == -1) uerror("extended_ml_spawn::pipe",Nothing);

  /* Set both side of the pipe close_on_exec... */
  (void) set_cloexec(pfd[PIPE_WRITE],true);
  (void) set_cloexec(pfd[PIPE_READ],true);

  args = copy_stringvect(v_args);

  if (Is_block(v_env))
    envp = copy_stringvect(Field(v_env,0));

  if (Is_block(v_working_dir))
    working_dir = String_val(Field(v_working_dir,0));

  /* This function deliberately doesn't release the O'Caml lock (i.e. it
     doesn't call caml_enter_blocking_section) during the fork.  This is
     because we hold pointers into the ML heap across a fork, and
     releasing the lock immediately before the fork could theoretically
     cause the GC to run and move blocks before the fork duplicates the
     memory space. */
  switch (child_pid = fork()) {
  case -1:
    my_errno = errno;
    caml_stat_free(args);
    if (envp)
      caml_stat_free(envp);
    NONINTR(close(pfd[PIPE_READ]));
    NONINTR(close(pfd[PIPE_WRITE]));
    unix_error(my_errno,"extended_ml_spawn: fork failed", Nothing);
  case 0:
    /* Child process.
       Since we've just lost all of our threads we need to be very careful
       not to call any function that might use a thread lock. This includes
       malloc,setenv and stdio functions... This is stated in the POSIX norm as:

       If a multi-threaded process calls fork(), the new process shall contain a
       replica of the calling thread and its entire address space, possibly
       including the states of mutexes and other resources. Consequently, to
       avoid errors, the child process may only execute async-signal-safe
       operations until such time as one of the exec functions is called.

       [http://pubs.opengroup.org/onlinepubs/009695399/functions/fork.html]

       The list of functions that we can call on the fork side can be found
       here:
       [http://pubs.opengroup.org/onlinepubs/009695399/functions/xsh_chap02_04.html]

       We also need to use _exit instead of [exit] because we do not want
       [at_exit] registered functions to be called.
     */

    /* Reset the sigmask to get rid of the inherited one */
    clear_sigprocmask();

    /* Just in case any of the pipes' file descriptors are 0, 1 or 2
       (not inconceivable, especially when running as a daemon),
       duplicate all three descriptors we need in the child to fresh
       descriptors before duplicating them onto stdin, stdout and stderr.

       This will ensure that there is one and only one copy of the file
       descriptors passed as arguments with id's higher than 2.

       F_DUPFD cannot get EINTR so we'll go only once through the
       loop
    */
    SYSCALL(stdin_fd = fcntl(stdin_fd,F_DUPFD,3));
    SYSCALL(stdout_fd= fcntl(stdout_fd,F_DUPFD,3));
    SYSCALL(stderr_fd= fcntl(stderr_fd,F_DUPFD,3));

    /* We clear out the close on exec on the fds... */
    SYSCALL(set_cloexec(stdin_fd,false));
    SYSCALL(set_cloexec(stdout_fd,false));
    SYSCALL(set_cloexec(stderr_fd,false));

    /* We must dup2 the descriptors back in place... */
    SYSCALL(dup2(stdin_fd,0));
    SYSCALL(dup2(stdout_fd,1));
    SYSCALL(dup2(stderr_fd,2));

    /* And close the old fds... */
    SYSCALL(close(stdin_fd));
    SYSCALL(close(stdout_fd));
    SYSCALL(close(stderr_fd));

    if (working_dir) {
      SYSCALL(chdir(working_dir));
    }

    if (Is_block(v_setuid)) {
      uid_t uid = (uid_t) Int_val(Field(v_setuid,0));
      if (getuid() != 0)
        report_errno_on_pipe (pfd[PIPE_WRITE],EPERM);
      SYSCALL(setuid(uid));
    }

    if (Is_block(v_setgid)) {
      gid_t gid = (gid_t) Int_val(Field(v_setgid,0));
      if (getuid() != 0)
        report_errno_on_pipe (pfd[PIPE_WRITE],EPERM);
      SYSCALL(setgid(gid));
    }

    if (envp) {
      /* path lookups should be done on the parent side of the fork so no
         execvp*/
      SYSCALL(execve(prog,(char **) args,(char **) envp));
    }else {
      SYSCALL(execv(prog,(char **) args));
    };

  default: /* Parent process */

    caml_enter_blocking_section();
    NONINTR(close (pfd[PIPE_WRITE])); /* Close unused write end */
    /* C side cleanup and looking for errors */
    forked_error = errno_from_pipe(pfd[PIPE_READ],&my_errno);
    NONINTR(close (pfd[PIPE_READ]));
    if (forked_error)
      NONINTR(waitpid(child_pid, 0, 0));

    caml_leave_blocking_section();

    /* Caml side cleanup */
    caml_stat_free(args);
    if (envp)
      caml_stat_free(envp);

    /* Returning the result */
    if (forked_error)
      unix_error(my_errno,"extended_ml_spawn::forked_side" ,
                 Nothing);

    /* Reading the pipe.. */
    CAMLreturn(Val_int(child_pid));
  }
}

CAMLprim value extended_ml_spawn_bc(value *argv, int argn)
{
  if (argn != 9) {
    caml_failwith("Unix.ml_spawn_bc got the wrong number of \
     arguments. This is due to an error in the FFI.");
  }
  return
    extended_ml_spawn(argv[0], argv[1], argv[2],
                      argv[3], argv[4], argv[5],
                      argv[6], argv[7], argv[8]);
}
