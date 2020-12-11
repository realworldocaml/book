#include "mirage_crypto.h"

#ifdef ARCH_64BIT
#include "p521_64.h"
#define LIMBS 9
#define WORD uint64_t
#define WORDSIZE 64
#else
#include "p521_32.h"
#define LIMBS 17
#define WORD uint32_t
#define WORDSIZE 32
#endif

#define LEN_PRIME 521
#define CURVE_DESCRIPTION fiat_p521

#include "inversion_template.h"
#include "point_operations.h"

#include <caml/memory.h>

CAMLprim value mc_p521_sub(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p521_sub(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p521_add(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p521_mul(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p521_from_bytes(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p521_to_bytes(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_sqr(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p521_square(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_from_montgomery(value x)
{
	CAMLparam1(x);
	WORD *l = Caml_ba_data_val(x);
	fiat_p521_from_montgomery(l, l);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_to_montgomery(value x)
{
	CAMLparam1(x);
	WORD *l = Caml_ba_data_val(x);
	fiat_p521_to_montgomery(l, l);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_nz(value x)
{
	CAMLparam1(x);
	CAMLreturn(Val_bool(fe_nz(Caml_ba_data_val(x))));
}

CAMLprim value mc_p521_set_one(value x)
{
	CAMLparam1(x);
        fiat_p521_set_one(Caml_ba_data_val(x));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_inv(value out, value in)
{
	CAMLparam2(out, in);
	inversion(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_point_double(value out, value in)
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

CAMLprim value mc_p521_point_add(value out, value p, value q)
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

CAMLprim value mc_p521_select(value out, value bit, value t, value f)
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
