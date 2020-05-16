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

(* This signature defines the format of the tables that are produced (in
   addition to the tables described in [TableFormat]) when the command line
   switch [--inspection] is enabled. It is used as an argument to
   [InspectionTableInterpreter.Make]. *)

module type TABLES = sig

  (* The types of symbols. *)

  include IncrementalEngine.SYMBOLS

  (* The type ['a lr1state] describes an LR(1) state. The generated parser defines
     it internally as [int]. *)

  type 'a lr1state

  (* Some of the tables that follow use encodings of (terminal and
     nonterminal) symbols as integers. So, we need functions that
     map the integer encoding of a symbol to its algebraic encoding. *)

  val    terminal: int -> xsymbol
  val nonterminal: int -> xsymbol

  (* The left-hand side of every production already appears in the
     signature [TableFormat.TABLES], so we need not repeat it here. *)

  (* The right-hand side of every production. This a linearized array
     of arrays of integers, whose [data] and [entry] components have
     been packed. The encoding of symbols as integers in described in
     [TableBackend]. *)

  val rhs: PackedIntArray.t * PackedIntArray.t

  (* A mapping of every (non-initial) state to its LR(0) core. *)

  val lr0_core: PackedIntArray.t

  (* A mapping of every LR(0) state to its set of LR(0) items. Each item is
     represented in its packed form (see [Item]) as an integer. Thus the
     mapping is an array of arrays of integers, which is linearized and
     packed, like [rhs]. *)

  val lr0_items: PackedIntArray.t * PackedIntArray.t

  (* A mapping of every LR(0) state to its incoming symbol, if it has one. *)

  val lr0_incoming: PackedIntArray.t

  (* A table that tells which non-terminal symbols are nullable. *)

  val nullable: string
    (* This is a packed int array of bit width 1. It can be read
       using [PackedIntArray.get1]. *)

  (* A two-table dimensional table, indexed by a nonterminal symbol and
     by a terminal symbol (other than [#]), encodes the FIRST sets. *)

  val first: int (* width of the bitmap *) * string (* second component of [PackedIntArray.t] *)

end

