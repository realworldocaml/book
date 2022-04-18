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

(**This module offers a simple facility for {b generating fresh integer
   identifiers}. *)

(**A gensym is a generator of unique integer identifiers. *)
type gensym =
  unit -> int

(**[make()] produces a new gensym. *)
val make : unit -> gensym

(**A generator whose current state can be inspected (but not modified). *)
type generator

(**[generator()] creates a new generator. *)
val generator: unit -> generator

(**[fresh g] causes the generator [g] to create and return a fresh
   integer identifier. *)
val fresh: generator -> int

(**[current g] returns the current state of the generator [g], that
   is, the next available integer identifier. *)
val current: generator -> int
