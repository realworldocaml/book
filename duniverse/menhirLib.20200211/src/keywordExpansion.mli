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

open BasicSyntax

(* [expand_grammar] expands away the keywords [$startpos] and [$endpos], as well
   the entire [ofs] family of keywords. Doing this early simplifies some aspects
   later on, in particular %inlining. *)

val expand_grammar: grammar -> grammar

