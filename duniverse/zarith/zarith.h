/** 
  Public C interface for Zarith.

  This is intended for C libraries that wish to convert between mpz_t and
  Z.t objects.


  This file is part of the Zarith library 
  http://forge.ocamlcore.org/projects/zarith .
  It is distributed under LGPL 2 licensing, with static linking exception.
  See the LICENSE file included in the distribution.

  Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
  Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
  a joint laboratory by:
  CNRS (Centre national de la recherche scientifique, France),
  ENS (École normale supérieure, Paris, France),
  INRIA Rocquencourt (Institut national de recherche en informatique, France).

*/


/* gmp.h or mpir.h must be included manually before zarith.h */

#ifdef __cplusplus
extern "C" {
#endif

#include <caml/mlvalues.h>

/* sets rop to the value in op (limbs are copied) */
void ml_z_mpz_set_z(mpz_t rop, value op);

/* inits and sets rop to the value in op (limbs are copied) */
void ml_z_mpz_init_set_z(mpz_t rop, value op);

/* returns a new z objects equal to op (limbs are copied) */
value ml_z_from_mpz(mpz_t op);

#ifdef __cplusplus
}
#endif
