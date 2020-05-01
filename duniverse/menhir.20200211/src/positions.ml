(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open Lexing

type t =
  (* Start and end positions. *)
  position * position

type 'a located =
    {
      value    : 'a;
      position : t;
    }

let value { value = v } =
  v

let position { position = p } =
  p

let decompose { value; position } =
  (value, position)

let with_pos p v =
  {
    value     = v;
    position  = p;
  }

let with_loc =
  (* The location is converted from the type [position * position]
     to the type [t]. *)
  with_pos

let map f v =
  {
    value     = f v.value;
    position  = v.position;
  }

let pmap f v =
  {
    value     = f v.position v.value;
    position  = v.position
  }

let iter f { value = v } =
  f v

let mapd f v =
  let w1, w2 = f v.value in
  let pos = v.position in
  { value = w1; position = pos },
  { value = w2; position = pos }

let dummy =
  (dummy_pos, dummy_pos)

let unknown_pos v =
  {
    value     = v;
    position  = dummy
  }

let start_of_position (p, _) = p

let end_of_position (_, p) = p

let filename_of_position p =
  (start_of_position p).pos_fname

let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

let join x1 x2 =
(
  start_of_position (if x1 = dummy then x2 else x1),
  end_of_position   (if x2 = dummy then x1 else x2)
)

let import x =
  x

let join_located l1 l2 f =
  {
    value    = f l1.value l2.value;
    position = join l1.position l2.position;
  }

let string_of_lex_pos p =
  let c = p.pos_cnum - p.pos_bol in
  (string_of_int p.pos_lnum)^":"^(string_of_int c)

let string_of_pos p =
  let filename = filename_of_position p in
  (* [filename] is hopefully not "". *)
  let l = line (start_of_position p) in
  let c1, c2 = characters (start_of_position p) (end_of_position p) in
  Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

let pos_or_undef = function
  | None -> dummy
  | Some x -> x

let cpos lexbuf =
  (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

let with_cpos lexbuf v =
  with_pos (cpos lexbuf) v

let string_of_cpos lexbuf =
  string_of_pos (cpos lexbuf)

let joinf f t1 t2 =
  join (f t1) (f t2)

let ljoinf f =
  List.fold_left (fun p t -> join p (f t)) dummy

let join_located_list ls f =
  {
    value     = f (List.map (fun l -> l.value) ls);
    position  = ljoinf (fun x -> x.position) ls
  }

(* The functions that print error messages and warnings require a list of
   positions. The following auxiliary functions help build such lists. *)

type positions =
    t list

let one (pos : position) : positions =
  [ import (pos, pos) ]

let lexbuf (lexbuf : lexbuf) : positions =
  [ import (lexbuf.lex_start_p, lexbuf.lex_curr_p) ]

let print (pos : position) =
  Printf.printf
    "{ pos_fname = \"%s\"; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }\n"
      pos.pos_fname
      pos.pos_lnum
      pos.pos_bol
      pos.pos_cnum
