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

(* Chopping [_inlined] off a name, if there is one,
   and returning the numeric suffix that follows, if there is one. *)

rule chop = parse
| (_* as x) "_inlined" (['0'-'9']+ as n) eof
    { x, int_of_string n }
| (_* as x) "_inlined" eof
    { x, 0 }
| (_* as x) eof
    { x, 0 }
