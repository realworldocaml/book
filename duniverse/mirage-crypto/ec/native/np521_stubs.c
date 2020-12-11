#include "mirage_crypto.h"

#ifdef ARCH_64BIT
#include "np521_64.h"
#define LIMBS 9
#define WORD uint64_t
#define WORDSIZE 64
#else
#include "np521_32.h"
#define LIMBS 17
#define WORD uint32_t
#define WORDSIZE 32
#endif

#define LEN_PRIME 521
#define CURVE_DESCRIPTION fiat_np521

#include "inversion_template.h"

#include <caml/memory.h>

CAMLprim value mc_np521_inv(value out, value in)
{
	CAMLparam2(out, in);
	inversion(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np521_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_np521_mul(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np521_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_np521_add(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np521_one(value out)
{
	CAMLparam1(out);
	fiat_np521_set_one(Caml_ba_data_val(out));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np521_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_np521_from_bytes(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np521_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_np521_to_bytes(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np521_from_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_np521_from_montgomery(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np521_to_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_np521_to_montgomery(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

