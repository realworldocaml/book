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

type inst =
  | Add
  | Sub
  | Mul
  | Const of int
  | Pop
  | Dup0 | Dup1 | Dup2
  | Store0 | Store1
  | If of program
  | While of program

  and program = inst list

let rec eval program stack =
  match program, stack with
  | [], i :: _ -> Some i
  | Add :: p, i1 :: i2 :: s -> eval p (i1 + i2 :: s)
  | Sub :: p, i1 :: i2 :: s -> eval p (i1 - i2 :: s)
  | Mul :: p, i1 :: i2 :: s -> eval p (i1 * i2 :: s)
  | Const i :: p, s -> eval p (i :: s)
  | Pop :: p, _ :: s -> eval p s
  | Dup0 :: p, i :: _
  | Dup1 :: p, _ :: i :: _
  | Dup2 :: p, _ :: _ :: i :: _ -> eval p (i :: stack)
  | Store0 :: p, i :: _ :: s -> eval p (i :: s)
  | Store1 :: p, i1 :: i2 :: _ :: s -> eval p (i2 :: i1 :: s)
  | If p1 :: p2, i :: s ->
    if i <> 0 then
      eval (List.append p1 p2) s
    else
      eval p2 s
  | While p1 :: p2, i :: s ->
    if i <> 0 then
      eval (List.append p1 program) s
    else
      eval p2 s

  | [], []
  | (Add | Sub | Mul | Dup1 | Store0) :: _, ([] | [_])
  | (Pop | Dup0 | If _ | While _) :: _, []
  | (Store1 | Dup2) :: _, ([] | [_] | [_; _]) ->
    None;;

let fact =
           [Const 1;
            Dup1;
            While [Dup1; Mul; Const 1; Dup2; Sub; Store1]]

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
