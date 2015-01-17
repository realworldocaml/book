(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2012 Jason Hickey
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

open Printf;;

let print_prices prices =
   print_string "--------------------------------\n\
                 | Size   | Hexcode    | Price  |\n\
                 --------------------------------\n";
   List.iter (fun (size, code, price) ->
         printf "| %-6s | 0x%08x | %6.2f |\n" size code price) prices;
   print_string "--------------------------------\n";;

print_prices
   [("small", 0x35, 1.02);
    ("medium", 0x7a1, 50.75);
    ("large", 0xbad, 400.8);
    ("vente", 0x11, 4136.);
    ("enormous", 100000, 100.)];;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
