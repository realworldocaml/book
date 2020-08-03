(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* This module compresses a two-dimensional table, where some values
   are considered insignificant, via row displacement. *)

(* A compressed table is represented as a pair of arrays. The
   displacement array is an array of offsets into the data array. *)

type 'a table =
    int array * (* displacement *)
     'a array   (* data *)

(* [compress equal insignificant dummy m n t] turns the two-dimensional table
   [t] into a compressed table. The parameter [equal] is equality of data
   values. The parameter [wildcard] tells which data values are insignificant,
   and can thus be overwritten with other values. The parameter [dummy] is
   used to fill holes in the data array. [m] and [n] are the integer
   dimensions of the table [t]. *)

val compress:
  ('a -> 'a -> bool) ->
  ('a -> bool) ->
  'a ->
  int -> int ->
  'a array array ->
  'a table

(* [get ct i j] returns the value found at indices [i] and [j] in the
   compressed table [ct]. This function call is permitted only if the
   value found at indices [i] and [j] in the original table is
   significant -- otherwise, it could fail abruptly. *)

(* Together, [compress] and [get] have the property that, if the value
   found at indices [i] and [j] in an uncompressed table [t] is
   significant, then [get (compress t) i j] is equal to that value. *)

val get:
  'a table ->
  int -> int ->
  'a

(* [getget] is a variant of [get] which only requires read access,
   via accessors, to the two components of the table. *)

val getget:
  ('displacement -> int -> int) ->
  ('data -> int -> 'a) ->
  'displacement * 'data ->
  int -> int ->
  'a

