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

type 'a vertex =
   (* Vertex (label, out-edges, dfs-mark, dfs-index) *)
   Vertex of 'a * 'a vertex list ref * bool ref * int option ref

type 'a directed_graph = 'a vertex list

let rec dfs graph =
   let cyclic = ref false in
   let dfs_counter = ref 0 in

   (* The main DFS search *)
   let rec search (Vertex (_, u_edges, u_mark, u_index)) =
      if not !u_mark then begin
         let c = !dfs_counter in
         u_index := Some c;
         dfs_counter := c + 1;
         List.iter (fun (Vertex (_, _, v_mark, v_index) as v) ->
            match !v_index with
               Some index ->
                  if index < c && not !v_mark then
                     cyclic := true
             | None ->
                  search v) !u_edges;
         u_mark := true
      end
   in

   (* Reset the graph state *)
   List.iter (fun (Vertex (_, _, mark, index)) ->
      mark := false;
      index := None) graph;

   (* DFS over all the vertices in the graph *)
   List.iter search graph

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
