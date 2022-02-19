#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <unistd.h>

// Detect platform
#if defined(_WIN32)
#define OCAML_ALCOTEST_WINDOWS
#elif defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__))
#if defined(_POSIX_VERSION)
#define OCAML_ALCOTEST_POSIX
#endif
#endif

// Windows support
#if defined(OCAML_ALCOTEST_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#include <windows.h>

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal2(result, pair);

	CONSOLE_SCREEN_BUFFER_INFO csbi;
	int success = GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
	if (success)
	{
		result = caml_alloc(1, 0);
		pair = caml_alloc(2, 0);
		Store_field(result, 0, pair);
		Store_field(pair, 0, Val_int((int)(csbi.dwSize.Y)));
		Store_field(pair, 1, Val_int((int)(csbi.dwSize.X)));
	}
	else
	{
		result = Val_int(0);
	}

	CAMLreturn(result);
}

// POSIX support
#elif defined(OCAML_ALCOTEST_POSIX)
#include <sys/ioctl.h>

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal2(result, pair);
	struct winsize ws;
	int z = ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);
	if (z == 0)
	{
		result = caml_alloc(1, 0);
		pair = caml_alloc(2, 0);
		Store_field(result, 0, pair);
		Store_field(pair, 0, Val_int(ws.ws_row));
		Store_field(pair, 1, Val_int(ws.ws_col));
	}
	else
	{
		result = Val_int(0);
	}

	CAMLreturn(result);
}

// Unsupported platform
#else

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal2(result, pair);

	result = Val_int(0);
	CAMLreturn(result);
}

#endif

/* duplicated from caml/sys.h and io.c */
CAMLextern value caml_channel_descriptor(value vchannel);
#define NO_ARG Val_int(0)
CAMLextern void caml_sys_error (value);
/* End of code duplication */

static int alcotest_saved_stdout;
static int alcotest_saved_stderr;

CAMLprim value alcotest_before_test (value voutput, value vstdout, value vstderr) {
  int output_fd, stdout_fd, stderr_fd, fd, ret;
  stdout_fd = Int_val(caml_channel_descriptor(vstdout));
  stderr_fd = Int_val(caml_channel_descriptor(vstderr));
  output_fd = Int_val(caml_channel_descriptor(voutput));
  fd = dup(stdout_fd);
  if(fd == -1) caml_sys_error(NO_ARG);
  alcotest_saved_stdout = fd;
  fd = dup(stderr_fd);
  if(fd == -1) caml_sys_error(NO_ARG);
  alcotest_saved_stderr = fd;
  ret = dup2(output_fd, stdout_fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = dup2(output_fd, stderr_fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  return Val_unit;
}

CAMLprim value alcotest_after_test (value vstdout, value vstderr) {
  int stdout_fd, stderr_fd, ret;
  stdout_fd = Int_val(caml_channel_descriptor(vstdout));
  stderr_fd = Int_val(caml_channel_descriptor(vstderr));
  ret = dup2(alcotest_saved_stdout, stdout_fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = dup2(alcotest_saved_stderr, stderr_fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = close(alcotest_saved_stdout);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = close(alcotest_saved_stderr);
  if(ret == -1) caml_sys_error(NO_ARG);
  return Val_unit;
}
