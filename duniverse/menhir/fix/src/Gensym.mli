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

(* A gensym is a generator of unique integer identifiers. *)

type gensym =
  unit -> int

(* [make()] produces a new gensym. *)

val make : unit -> gensym

(* A slightly more powerful abstraction is a generator whose current state
   can be inspected (without modification). *)

type generator

(* [generator()] creates a new generator. [fresh generator] causes the
   generator to create and return a fresh integer identifier. [current
   generator] returns the generator's current state, that is, the next
   available integer identifier. *)

val generator: unit -> generator
val fresh: generator -> int
val current: generator -> int
