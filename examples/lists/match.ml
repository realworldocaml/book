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
open Core.Std

let rec add1 l =
  match l with
  | [] -> []
  | h :: t -> (h + 1) :: (add1 t);;

let optional_default ~default opt =
  match opt with
  | Some v -> v
  | None -> default;;

let broken_third l =
  match l with
  | _ :: _ :: x :: _ -> Some x;;

let third l =
  match l with
  | _ :: _ :: x :: _ -> Some x
  | _ -> None;;

let third l =
  match l with
  | _ :: _ :: x :: _ -> Some x
  | []
  | [_]
  | [_; _] -> None;;

let third l1 =
  match List.tl l1 with
  | None -> None
  | Some l2 ->
    match List.tl l2 with
    | None -> None
    | Some l3 ->
      List.hd l3;;

let (>>=) v f =
  match v with
  | None -> None
  | Some x -> f x;;

let third l =
  Some l >>= List.tl >>= List.tl >>= List.hd;;

let third l =
  let (>>=) = Option.(>>=) in
  Some l >>= List.tl >>= List.tl >>= List.hd;;

let head1 l =
  match l with
  | [] -> None
  | hd :: _ -> Some hd;;

let head2 = function
  | [] -> None
  | h :: _ -> Some h;;
