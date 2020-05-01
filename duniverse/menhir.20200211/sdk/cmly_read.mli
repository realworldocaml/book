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

(* The functor [Read] reads a .cmly file. If the file is unreadable,
   the exception [Error] is raised. Otherwise, the functor builds a
   module of type [Cmly_api.GRAMMAR], which gives access to a description
   of the grammar and automaton. *)

exception Error of string

module Read (X : sig val filename : string end) : Cmly_api.GRAMMAR
