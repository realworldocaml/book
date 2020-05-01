#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/misc.h>

#include <windows.h>

static LARGE_INTEGER frequency;

CAMLprim value
clock_windows_init(value unit)
{
  QueryPerformanceFrequency(&frequency);
  frequency.QuadPart = 1000000000L / frequency.QuadPart;

  return Val_unit;
}

CAMLprim value
clock_windows_get_time(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  LARGE_INTEGER now;

  QueryPerformanceCounter(&now);

  res = copy_int64(now.QuadPart * frequency.QuadPart);

  CAMLreturn(res);
}
