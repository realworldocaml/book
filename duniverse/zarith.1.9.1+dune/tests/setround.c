/* Auxiliary function to control FP rounding mode.  Assumes ISO C99. */

#include <fenv.h>
#include <caml/mlvalues.h>

#ifndef FE_DOWNWARD
#define FE_DOWNWARD (-1)
#endif
#ifndef FE_TONEAREST
#define FE_TONEAREST (-1)
#endif
#ifndef FE_TOWARDZERO
#define FE_TOWARDZERO (-1)
#endif
#ifndef FE_UPWARD
#define FE_UPWARD (-1)
#endif

static int modes[4] = {
  FE_DOWNWARD, FE_TONEAREST, FE_TOWARDZERO, FE_UPWARD
};

CAMLprim value caml_ztest_setround(value vmode)
{
  int rc = fesetround(modes[Int_val(vmode)]);
  return Val_bool(rc == 0);
}
