(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the complex number tests. *)

open Ctypes

(* These functions can be bound either dynamically using Foreign or statically
   using stub generation. *)
module Common(F : Ctypes.FOREIGN) =
struct
  let bind' typ1 typ2 name =
    F.(foreign name
         (ptr typ1 @-> ptr typ2 @-> ptr typ2 @-> returning void))
  let bind typ name = bind' typ typ name

  let add_complexd = bind complex64 "add_complexd"
  let mul_complexd = bind complex64 "mul_complexd"
  let rotdist_complexd = bind' complex64 double "rotdist_complexd"
  let add_complexld = bind complexld "add_complexld"
  let mul_complexld = bind complexld "mul_complexld"
  let rotdist_complexld = bind' complexld ldouble "rotdist_complexld"
  let add_complexf = bind complex32 "add_complexf"
  let mul_complexf = bind complex32 "mul_complexf"
  let rotdist_complexf = bind' complex32 float "rotdist_complexf"
end

(* These functions can only be bound using stub generation, since Foreign
   doesn't support passing complex numbers by value. *)
module Stubs_only(F : Ctypes.FOREIGN) =
struct
  let bind' typ1 typ2 name =
    F.(foreign name (typ1 @-> typ2 @-> returning typ2))
  let bind typ name = bind' typ typ name

  let add_complexd_val = bind complex64 "add_complexd_val"
  let mul_complexd_val = bind complex64 "mul_complexd_val"
  let rotdist_complexd_val = bind' complex64 double "rotdist_complexd_val"
  let add_complexld_val = bind complexld "add_complexld_val"
  let mul_complexld_val = bind complexld "mul_complexld_val"
  let rotdist_complexld_val = bind' complexld ldouble "rotdist_complexld_val"
  let add_complexf_val = bind complex32 "add_complexf_val"
  let mul_complexf_val = bind complex32 "mul_complexf_val"
  let rotdist_complexf_val = bind' complex32 float "rotdist_complexf_val"
end

module Stubs (F: Ctypes.FOREIGN) =
struct
  include Common(F)
  include Stubs_only(F)
end
