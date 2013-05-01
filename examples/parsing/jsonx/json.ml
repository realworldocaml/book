(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2013 Jason Hickey
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
 * @email{jasonh@gmail.com}
 * @end[license]
 *)
open Printf

type value = [
| `Object of (string * value) list
| `Array of value array
| `String of string
| `Int of int
| `Float of float
| `True
| `False
| `Null ]

let rec output_value outc = function
| `Object obj -> print_object outc obj
| `Array arr -> print_array outc arr
| `String s -> printf "\"%s\"" s
| `Int i -> printf "%d" i
| `Float x -> printf "%f" x
| `True -> output_string outc "true"
| `False -> output_string outc "false"
| `Null -> output_string outc "null"

and print_object outc obj =
  output_string outc "{ ";
  let sep = ref "" in
  List.iter (fun (key, value) ->
    printf "%s%s: %a" !sep key output_value value;
    sep := ", ") obj;
  output_string outc " }"

and print_array outc arr =
  output_string outc "[";
  Array.iteri (fun i v ->
    if i > 0 then
      output_string outc ", ";
    output_value outc v) arr;
  output_string outc "]"

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
