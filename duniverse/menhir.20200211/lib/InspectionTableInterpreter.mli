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

(* This functor is invoked inside the generated parser, in [--table] mode. It
   produces no code! It simply constructs the types [symbol] and [xsymbol] on
   top of the generated types [terminal] and [nonterminal]. *)

module Symbols (T : sig

  type 'a terminal
  type 'a nonterminal

end)

: IncrementalEngine.SYMBOLS
  with type 'a terminal := 'a T.terminal
   and type 'a nonterminal := 'a T.nonterminal

(* This functor is invoked inside the generated parser, in [--table] mode. It
   constructs the inspection API on top of the inspection tables described in
   [InspectionTableFormat]. *)

module Make
  (TT : TableFormat.TABLES)
  (IT : InspectionTableFormat.TABLES
        with type 'a lr1state = int)
  (ET : EngineTypes.TABLE
        with type terminal = int
         and type nonterminal = int
         and type semantic_value = Obj.t)
  (E : sig
     type 'a env = (ET.state, ET.semantic_value, ET.token) EngineTypes.env
   end)

: IncrementalEngine.INSPECTION
  with type 'a terminal := 'a IT.terminal
   and type 'a nonterminal := 'a IT.nonterminal
   and type 'a lr1state := 'a IT.lr1state
   and type production := int
   and type 'a env := 'a E.env
