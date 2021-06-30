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

(* This module defines many internal naming conventions for use by the
   two code generators, [CodeBackend] and [TableBackend]. It also offers
   a few code generation facilities. *)

open IL
open Grammar

(* ------------------------------------------------------------------------ *)

(* Naming conventions. *)

(* A variable used to hold a semantic value. *)

val semv : string

(* A variable used to hold a stack. *)

val stack: string

(* A variable used to hold a state. *)

val state: string

(* A variable used to hold a token. *)

val token: string

(* Variables used to hold start and end positions. *)

val beforeendp: string
val startp: string
val endp: string
val startpos: string array -> int -> string
val endpos: string array -> int -> string

(* ------------------------------------------------------------------------ *)

(* Types for semantic values. *)

(* [semvtypent nt] is the type of the semantic value associated
   with nonterminal [nt]. *)

val semvtypent : Nonterminal.t -> typ

(* [semvtypetok tok] is the type of the semantic value associated with
   token [tok]. There is no such type if the token does not have a
   semantic value. *)

val semvtypetok : Terminal.t -> typ list

(* [semvtype symbol] is the type of the semantic value associated with
   [symbol]. *)

val semvtype : Symbol.t -> typ list

(* [has_semv symbol] indicates whether [symbol] carries a semantic value. *)

val has_semv : Symbol.t -> bool

(* ------------------------------------------------------------------------ *)

(* Patterns for tokens. *)

(* [tokpat tok pat] is a pattern that matches the token [tok] and binds
   its semantic value (if it has one) to the pattern [pat]. *)

val tokpat:  Terminal.t -> pattern -> pattern

(* [tokspat toks] is a pattern that matches any token in the set [toks],
   without binding its semantic value. *)

val tokspat: TerminalSet.t -> pattern

(* [destructuretokendef name codomain bindsemv branch] generates the
   definition of a function that destructure tokens. [name] is the
   name of the function that is generated. [codomain] is its return
   type. [bindsemv] tells whether the variable [semv] should be
   bound. [branch] is applied to each (non-pseudo) terminal and must
   produce code for each branch. *)

val destructuretokendef: string -> typ -> bool -> (Terminal.t -> expr) -> valdef

(* ------------------------------------------------------------------------ *)

(* A global variable holds the exception [Error]. *)

(* A reference to this global variable. *)

val errorval: expr

(* ------------------------------------------------------------------------ *)

(* The structure items [mbasics grammar] define and include the internal
   sub-module [Basics], which contains the definitions of the exception
   [Error] and of the type [token]. Then, they define the global variable
   mentioned above, which holds the exception [Error]. *)

val basics: string

val mbasics: BasicSyntax.grammar -> structure
