(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

type comparison = LT | EQ | GT

module type CompareSig = sig
   type t
   val compare : t -> t -> comparison
end

module type SetSig = sig
   module Compare : CompareSig

   type t
   type elt = Compare.t
   val empty   : t
   val add     : elt -> t -> t
   val mem     : elt -> t -> bool
   val find    : elt -> t -> elt
   val compare : t -> t -> comparison
end

module MakeSet (Compare : CompareSig)
 : SetSig with module Compare = Compare =
struct
    module Compare = Compare
    type elt = Compare.t
    type color = Red | Black
    type t = Leaf | Node of color * elt * t * t
    let empty = Leaf

    let balance = function
       Black, z, Node (Red, y, Node (Red, x, a, b), c), d
     | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
     | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
     | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
          Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
     | a, b, c, d ->
          Node (a, b, c, d)

    let add x s =
       let rec insert = function
          Leaf -> Node (Red, x, Leaf, Leaf)
        | Node (color, y, a, b) as s ->
             match Compare.compare x y with
                LT -> balance (color, y, insert a, b)
              | GT -> balance (color, y, a, insert b)
              | EQ -> s
       in
          match insert s with  (* guaranteed to be non-empty *)
             Node (_, y, a, b) -> Node (Black, y, a, b)
           | Leaf -> raise (Invalid_argument "insert")

    let rec find x = function
       Leaf -> raise Not_found
     | Node (_, y, left, right) ->
          match Compare.compare x y with
             LT -> find x left
           | GT -> find x right
           | EQ -> y

    let mem x s =
       try ignore (find x s); true with
          Not_found -> false

    let rec to_list l = function
       Leaf -> l
     | Node (_, x, left, right) ->
          to_list (x :: to_list l right) left

    let rec compare_lists l1 l2 =
       match l1, l2 with
          [], [] -> EQ
        | [], _ :: _ -> LT
        | _ :: _, [] -> GT
        | x1 :: t1, x2 :: t2 ->
             match Compare.compare x1 x2 with
                EQ -> compare_lists t1 t2
              | LT | GT as cmp -> cmp

    let compare s1 s2 =
       compare_lists (to_list [] s1) (to_list [] s2)
end

type 'set element = Int of int | Set of 'set

module rec Compare
 : CompareSig with type t = Set.t element =
struct
   type t = Set.t element
   let compare x1 x2 =
      match x1, x2 with
         Int i1, Int i2 ->
            if i1 < i2 then LT else if i1 > i2 then GT else EQ
       | Int _, Set _ -> LT
       | Set _, Int _ -> GT
       | Set s1, Set s2 -> Set.compare s1 s2
end

and Set : SetSig with module Compare = Compare = MakeSet (Compare)

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
