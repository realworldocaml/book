#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include "Hacl_Curve25519.h"

CAMLprim value ml_Hacl_Curve25519_crypto_scalarmult(value pk, value sk, value basepoint) {
    CAMLparam3(pk, sk, basepoint);
    Hacl_Curve25519_crypto_scalarmult(Caml_ba_data_val(pk),
                                      Caml_ba_data_val(sk),
                                      Caml_ba_data_val(basepoint));
    CAMLreturn(Val_unit);
}

