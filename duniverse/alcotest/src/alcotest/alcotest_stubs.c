#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

// Detect platform
#if defined(_WIN32)
#define OCAML_ALCOTEST_WINDOWS
#elif defined(__unix__) || defined(__unix)
#include <unistd.h>
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
