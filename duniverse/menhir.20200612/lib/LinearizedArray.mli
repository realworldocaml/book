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

(* An array of arrays (of possibly different lengths!) can be ``linearized'',
   i.e., encoded as a data array (by concatenating all of the little arrays)
   and an entry array (which contains offsets into the data array). *)

type 'a t =
  (* data: *)   'a array *
  (* entry: *) int array

(* [make a] turns the array of arrays [a] into a linearized array. *)

val make: 'a array array -> 'a t

(* [read la i j] reads the linearized array [la] at indices [i] and [j].
   Thus, [read (make a) i j] is equivalent to [a.(i).(j)]. *)

val read: 'a t -> int -> int -> 'a

(* [write la i j v] writes the value [v] into the linearized array [la]
   at indices [i] and [j]. *)

val write: 'a t -> int -> int -> 'a -> unit

(* [length la] is the number of rows of the array [la]. Thus, [length (make
   a)] is equivalent to [Array.length a]. *)

val length: 'a t -> int

(* [row_length la i] is the length of the row at index [i] in the linearized
   array [la]. Thus, [row_length (make a) i] is equivalent to [Array.length
   a.(i)]. *)

val row_length: 'a t -> int -> int

(* [read_row la i] reads the row at index [i], producing a list. Thus,
   [read_row (make a) i] is equivalent to [Array.to_list a.(i)]. *)

val read_row: 'a t -> int -> 'a list

(* The following variants read the linearized array via accessors
   [get_data : int -> 'a] and [get_entry : int -> int]. *)

val row_length_via:
  (* get_entry: *) (int -> int) ->
  (* i: *)         int ->
                   int

val read_via:
  (* get_data: *)  (int -> 'a) ->
  (* get_entry: *) (int -> int) ->
  (* i: *)         int ->
  (* j: *)         int ->
                   'a

val read_row_via:
  (* get_data: *)  (int -> 'a) ->
  (* get_entry: *) (int -> int) ->
  (* i: *)         int ->
                   'a list

