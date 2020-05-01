(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* This module provides a thin decoding layer for the generated tables, thus
   providing an API that is suitable for use by [Engine.Make]. It is part of
   [MenhirLib]. *)

(* The exception [Error] is declared within the generated parser. This is
   preferable to pre-declaring it here, as it ensures that each parser gets
   its own, distinct [Error] exception. This is consistent with the code-based
   back-end. *)

(* This functor is invoked by the generated parser. *)

module MakeEngineTable
  (T : TableFormat.TABLES)
: EngineTypes.TABLE
    with type state = int
     and type token = T.token
     and type semantic_value = Obj.t
     and type production = int
     and type terminal = int
     and type nonterminal = int
