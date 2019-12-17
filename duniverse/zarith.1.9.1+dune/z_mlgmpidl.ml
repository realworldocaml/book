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

external mlgmpidl_of_mpz: Mpz.t -> Z.t = "ml_z_mlgmpidl_of_mpz"
external mlgmpidl_set_mpz:  Mpz.t -> Z.t -> unit = "ml_z_mlgmpidl_set_mpz"

let z_of_mpz x = 
  mlgmpidl_of_mpz x

let mpz_of_z x =
  let r = Mpz.init () in
  mlgmpidl_set_mpz r x;
  r

let z_of_mpzf x = 
  z_of_mpz (Mpzf.mpz x)

let mpzf_of_z x = 
  Mpzf.mpzf (mpz_of_z x)

let q_of_mpq x =
  let n,d = Mpz.init (), Mpz.init () in
  Mpq.get_num n x;
  Mpq.get_den d x;
  Q.make (z_of_mpz n) (z_of_mpz d)

let mpq_of_q x =
  Mpq.of_mpz2 (mpz_of_z x.Q.num) (mpz_of_z x.Q.den)

let q_of_mpqf x = 
  q_of_mpq (Mpqf.mpq x)

let mpqf_of_q x = 
  Mpqf.mpqf (mpq_of_q x)
