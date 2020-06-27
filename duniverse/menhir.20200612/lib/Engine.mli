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

open EngineTypes

(* The LR parsing engine. *)

module Make (T : TABLE)
: ENGINE
  with type state = T.state
   and type token = T.token
   and type semantic_value = T.semantic_value
   and type production = T.production
   and type 'a env = (T.state, T.semantic_value, T.token) EngineTypes.env

(* We would prefer not to expose the definition of the type [env].
   However, it must be exposed because some of the code in the
   inspection API needs access to the engine's internals; see
   [InspectionTableInterpreter]. Everything would be simpler if
   --inspection was always ON, but that would lead to bigger parse
   tables for everybody. *)
