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
open Printf

module type Pipeline = sig
   type t
   val f : t -> unit
end

module type Filter = functor (P : Pipeline) -> Pipeline

module Print = struct
   type t = string
   let f s = print_string s; print_char '\n'
end

module Cat (Stdout : Pipeline with type t = string) =
struct
   type t = string

   let f filename =
      let fin = open_in filename in
      try
         while true do Stdout.f (input_line fin) done
      with End_of_file -> close_in fin
end

module CatFile = Cat (Print)

module Uniq (Stdout : Pipeline with type t = string)
 : Pipeline with type t = string =
struct
   type t = string
   let last_line = ref "\n"
   let f s =
      if s <> !last_line then
         Stdout.f s;
      last_line := s
end

module Grep
 (R : sig val regex : string end)
 (P : Pipeline with type t = string) =
struct
   type t = string
   let regexp = Str.regexp R.regex
   let f s =
      if Str.string_match regexp s 0 then
         P.f s
end

let grep regex filename =
   let module P = Cat (Grep (struct let regex = regex end) (Print)) in
   P.f filename

module StringOfChar (P : Pipeline with type t = char)
 : Pipeline with type t = string =
struct
   type t = string
   let f s = String.iter P.f s
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
