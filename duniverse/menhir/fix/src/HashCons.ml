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

open Sigs

(* The interface that we expose is not fully safe: it is possible, by applying
   the functor [Make] twice, to construct two instances of the hash-consing
   service that produce hash-consed values of *compatible* type [M.key cell]. *)

(* To achieve greater safety, one might wish to make the functor [Make]
   generative, so that each application of [Make] creates a fresh abstract
   type [t] which is convertible (in one direction only) into [M.key cell].
   However, that would render [Make] impossible to use in situations where the
   user wishes to hash-cons a type of trees. Indeed, the user needs to first
   define a (concrete, recursive) type of trees, then create an instance of
   the hash-consing service. If [Make] produces an abstract type, then the
   type definition and the functor application must be mutually recursive,
   which is not permitted. *)

type 'data cell =
  { id: int; data: 'data }

let id x =
  x.id

let data x =
  x.data

let equal x y =
  x.id = y.id
    (* We could also use physical equality, saving two reads. *)

let compare x y =
  compare x.id y.id
    (* To compare two cells, we compare their unique identifiers. *)

let hash x =
  Hashtbl.hash x.id
    (* To hash a cell, we hash its unique identifier. *)
    (* We could also return [x.id] without hashing it. *)

module type SERVICE = sig
  type data
  val make: data -> data cell
end

(* Creating a fresh hash-consing service is a simple matter of:
   1- creating a new gensym;
   2- memoizing the function [fun data -> { id = gensym(); data }]. *)

module Make (M : MEMOIZER) = struct

  type data =
    M.key

  let gensym =
    Gensym.make()

  let make =
    M.memoize (fun data -> { id = gensym(); data })

end

module ForHashedType
  (T : HashedType)
     = Make(Memoize.ForHashedType(T))
