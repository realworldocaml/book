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

(* This simple function counts the number of newline characters
   in a string. *)

let newline = ('\010' | '\013' | "\013\010")

let ordinary = [^ '\010' '\013']+

rule count n = parse
| eof
    { n }
| newline
    { count (n + 1) lexbuf }
| ordinary
    { count n lexbuf }

