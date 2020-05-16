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

module Make
  (M : IMPERATIVE_MAPS)
  (G : GRAPH with type t = M.key)
= struct

  (* Set up a facility for numbering vertices. *)

  module N =
    Numbering.Make(M)

  (* Implement a depth-first search. The functions [N.has_been_encoded]
     and [N.encode] allow us not only to assign a unique number to each
     vertex, but also to mark a vertex and test whether a vertex has been
     marked. *)

  let frontier =
    Stack.create()

  let push x =
    Stack.push x frontier

  let rec visit () =
    match Stack.pop frontier with
    | exception Stack.Empty ->
        (* The stack is empty: we are done. *)
        ()
    | x ->
        if N.has_been_encoded x then
          (* [x] is known already: ignore it. *)
          visit()
        else
          (* Assign a number to [x]. *)
          let (_ : int) = N.encode x in
          G.foreach_successor x push;
          visit()

  (* Perform the depth-first search. *)

  let () =
    G.foreach_root push;
    visit()

  (* We are done! This defines [n], [encode], [decode]. *)

  include N.Done()

end

module ForOrderedType (T : OrderedType) =
  Make(Glue.PersistentMapsToImperativeMaps(Map.Make(T)))

module ForHashedType (T : HashedType) =
  Make(Glue.HashTablesAsImperativeMaps(T))

module ForType (T : TYPE) =
  ForHashedType(Glue.TrivialHashedType(T))
