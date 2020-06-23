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

module CHAR = struct
  type t = char
end

module INT = struct
  type t = int
end

module STRING = struct
  type t = string
end

module TrivialOrderedType (T : TYPE) = struct
  include T
  let compare = compare
end

module TrivialHashedType (T : TYPE) = struct
  include T
  let equal = (=)
  let hash = Hashtbl.hash
end

module InjectOrderedType
  (U : OrderedType)
  (I : INJECTION with type u := U.t)
= struct
  type t = I.t
  let compare x y = U.compare (I.encode x) (I.encode y)
end

module InjectHashedType
  (U : HashedType)
  (I : INJECTION with type u := U.t)
= struct
  type t = I.t
  let equal x y = U.equal (I.encode x) (I.encode y)
  let hash x = U.hash (I.encode x)
end

module InjectMinimalImperativeMaps
  (M : MINIMAL_IMPERATIVE_MAPS)
  (I : INJECTION with type u := M.key)
= struct
  type key = I.t
  type 'data t = 'data M.t
  let create = M.create
  let add x y m = M.add (I.encode x) y m
  let find x m = M.find (I.encode x) m
end

module InjectImperativeMaps
  (M : IMPERATIVE_MAPS)
  (I : INJECTION with type u := M.key)
  (J : sig val decode: M.key -> I.t end)
= struct
  include InjectMinimalImperativeMaps(M)(I)
  let clear = M.clear
  let iter f m =
    M.iter (fun x y ->
      f (J.decode x) y
    ) m
end

module PersistentMapsToImperativeMaps (M : PERSISTENT_MAPS) = struct

  type key =
    M.key

  type 'data t =
    'data M.t ref

  let create () =
    ref M.empty

  let clear t =
    t := M.empty

  let add k d t =
    t := M.add k d !t

  let find k t =
    M.find k !t

  let iter f t =
    M.iter f !t

end

module ArraysAsImperativeMaps (K : sig val n: int end) = struct

  open K

  type key =
    int

  type 'data t =
    'data option array

  let create () =
    Array.make n None

  let clear m =
    Array.fill m 0 n None

  let add key data m =
    m.(key) <- Some data

  let find key m =
    match m.(key) with
    | None ->
	raise Not_found
    | Some data ->
	data

  let iter f m =
    Array.iteri (fun key data ->
      match data with
      | None ->
	  ()
      | Some data ->
	  f key data
    ) m

end

module Adapt (T : Hashtbl.S) = struct

  include T
    (* types:  [key], ['data t] *)
    (* values: [clear], [iter]  *)

  let create () =
    T.create 1023

  let add key data table =
    T.add table key data

  let find table key =
    T.find key table

end

module HashTablesAsImperativeMaps (H : HashedType) =
  Adapt(Hashtbl.Make(H))
