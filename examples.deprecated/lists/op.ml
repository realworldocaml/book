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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

let rec my_append l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t -> h :: my_append t l2;;

my_append ["a"; "b"] ["x"; "y"; "z"];;

let rec my_map ~f = function
  | [] -> []
  | h :: t -> f h :: my_map ~f t;;

my_map ~f:(fun i -> string_of_int (i * 10)) [1; 2; 3];;

let my_length l =
  let rec length i = function
    | [] -> i
    | _ :: t -> length (i + 1) t
  in
  length 0 l;;

let rec my_nth l i =
  match l with
  | [] -> None
  | h :: t -> if i = 0 then Some h else my_nth t (i - 1);;

let rec my_fold_left ~init ~f = function
  | [] -> init
  | h :: t -> my_fold_left ~init:(f init h) ~f t;;

let my_exists ~f l =
  List.fold_left ~init:false ~f:(fun result x -> result || f x) l;;

let my_for_all ~f l =
  List.fold_left ~init:true ~f:(fun result x -> result && f x) l;;

let rec my_exists ~f = function
  | [] -> false
  | h :: t -> f h || my_exists ~f t;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
