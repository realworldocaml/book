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

(**This module offers facilities for {b constructing a (possibly recursive)
   memoized function}, that is, a function that lazily records its
   input/output graph, so as to avoid repeated computation. *)

open Sigs

(**{!Make} constructs a memoizer for a type [key] that is
   equipped with an implementation of imperative maps. *)
module Make
  (M : MINIMAL_IMPERATIVE_MAPS)
     : MEMOIZER with type key = M.key
                 and type 'a t = 'a M.t

(**{!ForOrderedType} is a special case of {!Make} where it
   suffices to pass an ordered type [T] as an argument.
   A reference to a persistent map is used to hold the
   memoization table. *)
module ForOrderedType
  (T : OrderedType)
     : MEMOIZER with type key = T.t
                 and type 'a t = 'a Map.Make(T).t ref

(**{!ForHashedType} is a special case of {!Make} where it
   suffices to pass a hashed type [T] as an argument. A
   hash table is used to hold the memoization table. *)
module ForHashedType
  (T : HashedType)
     : MEMOIZER with type key = T.t
                 and type 'a t = 'a Hashtbl.Make(T).t

(**{!ForType} is a special case of {!Make} where it suffices
   to pass an arbitrary type [T] as an argument. A hash table
   is used to hold the memoization table. OCaml's built-in
   generic equality and hash functions are used. *)
module ForType
  (T : TYPE)
     : MEMOIZER with type key = T.t

(**A memoizer for the type [char]. *)
module Char
     : MEMOIZER with type key = char

(**A memoizer for the type [int]. *)
module Int
     : MEMOIZER with type key = int

(**A memoizer for the type [string]. *)
module String
     : MEMOIZER with type key = string
