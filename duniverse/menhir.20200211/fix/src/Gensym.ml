(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

type gensym =
  unit -> int

let postincrementor c () =
  let y = !c in
  c := y + 1;
  y

let make () =
  postincrementor (ref 0)

(* We do not worry about overflow. On today's 64-bit machines, it won't occur
   in a lifetime. *)

type generator =
  int ref

let generator () =
  ref 0

let fresh c =
  let y = !c in
  c := y + 1;
  y

let current c =
  !c
