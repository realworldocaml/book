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
  (F : FINITE_TYPE)
  (M : MINIMAL_IMPERATIVE_MAPS with type key = F.t)
= struct

  type key = M.key

  let tabulate (f : key -> 'a) : key -> 'a =
    let table = M.create() in
    F.foreach (fun x -> M.add x (f x) table);
    fun x ->
      try
        M.find x table
      with Not_found ->
        (* This cannot happen if [foreach] is exhaustive. *)
        let msg = Printf.sprintf "\n  Fix.Tabulate says: \
          please check that your \"foreach\" function is \
          exhaustive.\n  %s\n" __LOC__ in
        raise (Invalid_argument msg)

end

module ForOrderedType
  (F : FINITE_TYPE)
  (T : OrderedType with type t = F.t)
=
  Make(F)(Glue.PersistentMapsToImperativeMaps(Map.Make(T)))

module ForHashedType
  (F : FINITE_TYPE)
  (T : HashedType with type t = F.t)
=
  Make(F)(Glue.HashTablesAsImperativeMaps(T))

module ForType (F : FINITE_TYPE) =
  ForHashedType(F)(Glue.TrivialHashedType(F))

module ForIntSegment (K : sig val n: int end) = struct

  type key = int

  let tabulate (f : key -> 'a) : key -> 'a =
    let table = Array.init K.n f in
    fun x ->
      table.(x)

end
