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

type 'a parent =
   Root of int
 | Parent of 'a vertex

and 'a vertex = 'a * 'a parent ref

let union ((_, p1) as u1) ((_, p2) as u2) =
   match !p1, !p2 with
      Root size1, Root size2 when size1 > size2 ->
         p2 := Parent u1;
         p1 := Root (size1 + size2)
    | Root size1, Root size2 ->
         p1 := Parent u2;
         p2 := Root (size1 + size2)
    | _ ->
         raise (Invalid_argument "union: not roots")

let rec compress root (_, p) =
   match !p with
      Root _ -> ()
    | Parent v -> p := Parent root; compress root v

let rec simple_find ((_, p) as v) =
   match !p with
      Root _ -> v
    | Parent v -> simple_find v

let find v =
   let root = simple_find v in
   compress root v;
   root

(* An edge is a triple (weight, vertex1, vertex2) *)
type 'a edge = float * 'a vertex * 'a vertex

(* A list of edges, sorted by increasing weight *)
let kruskal edges =
   let spanning_tree = ref [] in
   List.iter (fun ((_, v1, v2) as edge) ->
      let u1 = find v1 in
      let u2 = find v2 in
      if u1 != u2 then begin
         (* v1 and v2 belong to different components *)
         spanning_tree := edge :: !spanning_tree;
         union u1 u2
      end) edges;
   !spanning_tree

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
