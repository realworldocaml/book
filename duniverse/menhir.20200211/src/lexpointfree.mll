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

{

  exception InvalidPointFreeAction

}

(* See [ParserAux.validate_pointfree_action]. *)

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']

let op         =
  symbolchar+
  (* An approximation of OCaml's rules. *)

let whitespace = [ ' ' '\t' '\n' ]

rule validate_pointfree_action = parse
| whitespace* (lowercase | uppercase | '`') (identchar | '.')* whitespace* eof
| whitespace* '(' op ')' whitespace* eof
    (* We have got a nonempty point-free action: <id>. *)
    { true }
| whitespace* eof
    (* We have got an empty point-free action: <>. *)
    { false }
| _
    { raise InvalidPointFreeAction }

(* See [ParserAux.valid_ocaml_identifier]. *)

and valid_ocaml_identifier = parse
| lowercase identchar* eof
    { true }
| _
| eof
    { false }
