(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

module Position = struct

  type t = Lexing.position =  {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

  let to_string t =
    Printf.sprintf "%s%d:%d"
      (if t.pos_fname = "" then "" else t.pos_fname ^ ":")
      t.pos_lnum
      (t.pos_cnum - t.pos_bol)

  let zero = { pos_fname = "";
               pos_lnum = 1;
               pos_bol = 0;
               pos_cnum = 0 }

  let column p = p.pos_cnum - p.pos_bol
end

module Region = struct
  open Position
  type t = Position.t * Position.t

  let fst = fst
  let snd = snd

  let create p1 p2 = (p1,p2)

  let start_column (p,_) = column p
  let end_column (_,p) = column p

  let start_line (p,_) = p.pos_lnum
  let end_line (_,p) = p.pos_lnum

  let char_offset (p, _) = p.pos_cnum
  let length (p1, p2) = p2.Position.pos_cnum - p1.Position.pos_cnum

  let zero = (Position.zero, Position.zero)

  let translate (p,p') diff =
    { p  with pos_cnum = p .pos_cnum + diff },
    { p' with pos_cnum = p'.pos_cnum + diff }
end


