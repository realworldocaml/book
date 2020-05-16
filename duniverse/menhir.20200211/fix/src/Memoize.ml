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

(* [rev_take accu n xs] is [accu @ rev (take n xs)], where [take n xs]
   takes the first [n] elements of the list [xs]. The length of [xs] must
   be at least [n]. *)

let rec rev_take accu n xs =
  match n, xs with
  | 0, _ ->
      accu
  | _, [] ->
      (* The list is too short. This cannot happen. *)
      assert false
  | _, x :: xs ->
      rev_take (x :: accu) (n - 1) xs

module Make (M : IMPERATIVE_MAPS) = struct

  type key = M.key

  let add x y table =
    M.add x y table;
    y

  (* [memoize] could be defined as a special case of [fix] via the declaration
     [let memoize f = fix (fun _ x -> f x)]. The following direct definition is
     perhaps easier to understand and may give rise to more efficient code. *)

  type 'a t =
    'a M.t

  let visibly_memoize (f : key -> 'a) : (key -> 'a) * 'a t =
    let table = M.create() in
    let f x =
      try
	M.find x table
      with Not_found ->
        add x (f x) table
    in
    f, table

  let memoize (f : key -> 'a) : key -> 'a =
    let f, _table = visibly_memoize f in
    f

  let fix (ff : (key -> 'a) -> (key -> 'a)) : key -> 'a =
    let table = M.create() in
    let rec f x =
      try
	M.find x table
      with Not_found ->
	add x (ff f x) table
    in
    f

  (* In the implementation of [defensive_fix], we choose to use two tables.
     A permanent table, [table] maps keys to values. Once a pair [x, y] has
     been added to this table, it remains present forever: [x] is stable,
     and a call to [f x] returns [y] immediately. A transient table, [marked],
     is used only while a call is in progress. This table maps keys to integers:
     for each key [x], it records the depth of the stack at the time [x] was
     pushed onto the stack. Finally, [stack] is a list of the keys currently
     under examination (most recent key first), and [depth] is the length of
     the list [stack]. Recording integer depths in the table [marked] allows
     us to identify the desired cycle, a prefix of the list [stack], without
     requiring an equality test on keys. *)

  exception Cycle of key list * key

  let defensive_fix (ff : (key -> 'a) -> (key -> 'a)) : key -> 'a =
    (* Create the permanent table. *)
    let table = M.create() in
    (* Define the main recursive function. *)
    let rec f stack depth marked (x : key) : 'a =
      try
	M.find x table
      with Not_found ->
        match M.find x marked with
        | i ->
            (* [x] is marked, and was pushed onto the stack at a time when the
               stack depth was [i]. We have found a cycle. Fail. Cut a prefix
               of the reversed stack, which represents the cycle that we have
               detected, and reverse it on the fly. *)
            raise (Cycle (rev_take [] (depth - i) stack, x))
        | exception Not_found ->
           (* [x] is not marked. Mark it while we work on it. There is no need
              to unmark [x] afterwards; inserting it into [table] indicates
              that it has stabilized. There also is no need to catch and
              re-raise the exception [Cycle]; we just let it escape. *)
            M.add x depth marked;
            let stack = x :: stack
            and depth = depth + 1 in
            let y = ff (f stack depth marked) x in
	    add x y table
    in
    fun x ->
      (* Create the transient table. *)
      let marked = M.create()
      and stack = []
      and depth = 0 in
      (* Answer this query. *)
      f stack depth marked x

end

module ForOrderedType (T : OrderedType) =
  Make(Glue.PersistentMapsToImperativeMaps(Map.Make(T)))

module ForHashedType (T : HashedType) =
  Make(Glue.HashTablesAsImperativeMaps(T))

module ForType (T : TYPE) =
  ForHashedType(Glue.TrivialHashedType(T))

module Char =
  ForType(Glue.CHAR)

module Int =
  ForType(Glue.INT)

module String =
  ForType(Glue.STRING)
