#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/mlvalues.h>
#include <unistd.h>

/* #include <caml/io.h> */

/* The definition of channel should be kept in sync with upstream ocaml  */
/* Start of duplicated code from caml/io.h */
#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 65536
#endif

#if defined(_WIN32)
typedef __int64 file_offset;
#elif defined(HAS_OFF_T)
#include <sys/types.h>
typedef off_t file_offset;
#else
typedef long file_offset;
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  file_offset offset;           /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel * next, * prev;/* Double chaining of channels (flush_all) */
  int revealed;                 /* For Cash only */
  int old_revealed;             /* For Cash only */
  int refcount;                 /* For flush_all and for Cash */
  int flags;                    /* Bitfield */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
  char * name;                  /* Optional name (to report fd leaks) */
};

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/* End of duplicated code from caml/io.h */

/* Start of duplicated code from caml/sys.h */
#define NO_ARG Val_int(0)
CAMLextern void caml_sys_error (value);
/* End of duplicated code from caml/sys.h */

static int expect_test_collector_saved_stdout;
static int expect_test_collector_saved_stderr;

CAMLprim value expect_test_collector_before_test (value voutput, value vstdout, value vstderr) {
  struct channel* output = Channel(voutput);
  struct channel* stdout = Channel(vstdout);
  struct channel* stderr = Channel(vstderr);
  int fd, ret;
  fd = dup(stdout->fd);
  if(fd == -1) caml_sys_error(NO_ARG);
  expect_test_collector_saved_stdout = fd;
  fd = dup(stderr->fd);
  if(fd == -1) caml_sys_error(NO_ARG);
  expect_test_collector_saved_stderr = fd;
  ret = dup2(output->fd, stdout->fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = dup2(output->fd, stderr->fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  return Val_unit;
}

CAMLprim value expect_test_collector_after_test (value vstdout, value vstderr) {
  struct channel* stdout = Channel(vstdout);
  struct channel* stderr = Channel(vstderr);
  int ret;
  ret = dup2(expect_test_collector_saved_stdout, stdout->fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = dup2(expect_test_collector_saved_stderr, stderr->fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = close(expect_test_collector_saved_stdout);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = close(expect_test_collector_saved_stderr);
  if(ret == -1) caml_sys_error(NO_ARG);
  return Val_unit;
}

CAMLprim value caml_out_channel_pos_fd (value vchan) {
  struct channel* chan = Channel(vchan);
  file_offset ret;
  caml_enter_blocking_section();
  ret = lseek(chan->fd, 0, SEEK_CUR);
  caml_leave_blocking_section();
  if (ret == -1) caml_sys_error(NO_ARG);
  if (ret > Max_long) caml_failwith("caml_out_channel_pos_fd: overflow");
  return Val_long(ret);
}
