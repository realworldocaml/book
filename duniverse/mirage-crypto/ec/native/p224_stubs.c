#include "mirage_crypto.h"

#ifdef ARCH_64BIT
#include "p224_64.h"
#define LIMBS 4
#define WORD uint64_t
#define WORDSIZE 64
#else
#include "p224_32.h"
#define LIMBS 7
#define WORD uint32_t
#define WORDSIZE 32
#endif

#define LEN_PRIME 224
#define CURVE_DESCRIPTION fiat_p224

#include "inversion_template.h"
#include "point_operations.h"

#include <caml/memory.h>

CAMLprim value mc_p224_sub(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p224_sub(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p224_add(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p224_mul(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p224_from_bytes(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p224_to_bytes(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_sqr(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p224_square(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_from_montgomery(value x)
{
	CAMLparam1(x);
	WORD *l = Caml_ba_data_val(x);
	fiat_p224_from_montgomery(l, l);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_to_montgomery(value x)
{
	CAMLparam1(x);
	WORD *l = Caml_ba_data_val(x);
	fiat_p224_to_montgomery(l, l);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_nz(value x)
{
	CAMLparam1(x);
	CAMLreturn(Val_bool(fe_nz(Caml_ba_data_val(x))));
}

CAMLprim value mc_p224_set_one(value x)
{
	CAMLparam1(x);
        fiat_p224_set_one(Caml_ba_data_val(x));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_inv(value out, value in)
{
	CAMLparam2(out, in);
	inversion(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_point_double(value out, value in)
{
	CAMLparam2(out, in);
	point_double(
		Caml_ba_data_val(Field(out, 0)),
		Caml_ba_data_val(Field(out, 1)),
		Caml_ba_data_val(Field(out, 2)),
		Caml_ba_data_val(Field(in, 0)),
		Caml_ba_data_val(Field(in, 1)),
		Caml_ba_data_val(Field(in, 2))
	);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_point_add(value out, value p, value q)
{
	CAMLparam3(out, p, q);
	point_add(
		Caml_ba_data_val(Field(out, 0)),
		Caml_ba_data_val(Field(out, 1)),
		Caml_ba_data_val(Field(out, 2)),
		Caml_ba_data_val(Field(p, 0)),
		Caml_ba_data_val(Field(p, 1)),
		Caml_ba_data_val(Field(p, 2)),
		0,
		Caml_ba_data_val(Field(q, 0)),
		Caml_ba_data_val(Field(q, 1)),
		Caml_ba_data_val(Field(q, 2))
	);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_select(value out, value bit, value t, value f)
{
	CAMLparam4(out, bit, t, f);
	fe_cmovznz(
		Caml_ba_data_val(out),
		Bool_val(bit),
		Caml_ba_data_val(f),
		Caml_ba_data_val(t)
	);
	CAMLreturn(Val_unit);
}
