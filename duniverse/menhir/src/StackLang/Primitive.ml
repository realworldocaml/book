(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLang
open Reg.Set
let (+) = union
let (-) = diff

let registers prim =
  match prim with
  | PrimLexerCall vs ->
      Value.registers vs
  | PrimOCamlFieldAccess (v, _fields) ->
      Value.registers [v]
  | PrimOCamlAction (bindings, _prod, action) ->
      let vars = import (Action.vars action) in
      Bindings.codomain bindings + (vars - Bindings.domain bindings)

let prim_ocaml_action bs prod action =
  let vars = import (Action.vars action) in
  let bs = Bindings.restrict vars bs in
  PrimOCamlAction (bs, prod, action)

let apply bs prim =
  match prim with
  | PrimLexerCall vs ->
      PrimLexerCall (List.map (Bindings.apply bs) vs)
  | PrimOCamlAction (bs', prod, action) ->
      prim_ocaml_action (Bindings.seq bs bs') prod action
  | PrimOCamlFieldAccess (v, field) ->
      PrimOCamlFieldAccess (Bindings.apply bs v, field)

let pure prim =
  match prim with
  | PrimLexerCall _
  | PrimOCamlAction _ ->
      false
  | PrimOCamlFieldAccess _ ->
      true
