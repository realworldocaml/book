(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* tableBackend.ml *)

(* Encodings of entries in the enabled reductions bitmap. *)

let encode_MayReduce =              (* 0 *)
  0

let encode_MayNotReduce =           (* 1 *)
  1

(* The enabled reductions table allows testing at runtime whether in some
   state [s] it is permitted to reduce some production [prod]. This table
   is normally not needed by an LR engine. We include it because we wish
   to allow the user to force reductions. This is useful when programming
   error recovery strategies. *)

(* Like the error bitmap, this is a nonsparse, two-dimensional bitmap. It
   is pretty big: in a version of OCaml's grammar, there are 1500 states
   and 700 productions, so the enabled reductions table has over a million
   entries, which translates to about 128 kilobytes. *)

let enabled_reduction node prod =
  if Invariant.may_reduce node prod then
    encode_MayReduce
  else
    encode_MayNotReduce

let _enabled_reduction =
  define_and_measure (
    "enabled_reduction",
    flatten_and_marshal11_list (
      Lr1.map (fun node ->
        Production.map (fun prod ->
          enabled_reduction node prod
        )
      )
    )
  )
