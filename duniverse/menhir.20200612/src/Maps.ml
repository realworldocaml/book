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

(* BEGIN PERSISTENT_MAPS *)
module type PERSISTENT_MAPS = sig
  type key
  type 'data t
  val empty: 'data t
  val add: key -> 'data -> 'data t -> 'data t
  val find: key -> 'data t -> 'data
  val iter: (key -> 'data -> unit) -> 'data t -> unit
end
(* END PERSISTENT_MAPS *)

(* BEGIN IMPERATIVE_MAPS *)
module type IMPERATIVE_MAPS = sig
  type key
  type 'data t
  val create: unit -> 'data t
  val clear: 'data t -> unit
  val add: key -> 'data -> 'data t -> unit
  val find: key -> 'data t -> 'data
  val iter: (key -> 'data -> unit) -> 'data t -> unit
end
(* END IMPERATIVE_MAPS *)

(* BEGIN IMPERATIVE_MAP *)
module type IMPERATIVE_MAP = sig
  type key
  type data
  val set: key -> data -> unit
  val get: key -> data option
end
(* END IMPERATIVE_MAP *)

module PersistentMapsToImperativeMaps
  (M : PERSISTENT_MAPS)
     : IMPERATIVE_MAPS with type key = M.key
                        and type 'data t = 'data M.t ref
= struct

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

module ImperativeMapsToImperativeMap
  (M : IMPERATIVE_MAPS)
  (D : sig type data end)
     : IMPERATIVE_MAP with type key = M.key
                       and type data = D.data
= struct

  type key =
      M.key

  type data =
      D.data

  let m =
    M.create()

  let set k d =
    M.add k d m

  let get k =
    try
      Some (M.find k m)
    with Not_found ->
      None

end

module ArrayAsImperativeMaps
  (K : sig val n: int end)
  : IMPERATIVE_MAPS with type key = int
                     and type 'data t = 'data option array

= struct

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

module HashTableAsImperativeMaps
  (H : Hashtbl.HashedType)
     : IMPERATIVE_MAPS with type key = H.t
= struct

  include Hashtbl.Make(H)

  let create () =
    create 1023

  let add key data table =
    add table key data

  let find table key =
    find key table

end

module TrivialHashedType
  (T : sig type t end)
     : Hashtbl.HashedType with type t = T.t
= struct

  include T

  let equal =
    (=)

  let hash =
    Hashtbl.hash

end

