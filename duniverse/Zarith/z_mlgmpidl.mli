(**
   Conversion between Zarith and MLGmpIDL integers and rationals.


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

 *)

val z_of_mpz: Mpz.t -> Z.t
val mpz_of_z: Z.t -> Mpz.t
val z_of_mpzf: Mpzf.t -> Z.t
val mpzf_of_z: Z.t -> Mpzf.t
val q_of_mpq: Mpq.t -> Q.t
val mpq_of_q: Q.t -> Mpq.t
val q_of_mpqf: Mpqf.t -> Q.t
val mpqf_of_q: Q.t -> Mpqf.t
