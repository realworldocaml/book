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
open IL
open CodeBits

(* -------------------------------------------------------------------------- *)

(* The [Error] exception. *)

let excname =
  "Error"

let excdef = {
  excname = excname;
  exceq = (if Settings.fixedexc then Some "Parsing.Parse_error" else None);
}

(* -------------------------------------------------------------------------- *)

(* The type of the monolithic entry point for the start symbol [symbol]. *)

let entrytypescheme grammar symbol =
  let typ = TypTextual (ocamltype_of_start_symbol grammar symbol) in
  type2scheme (marrow [ arrow tlexbuf TokenType.ttoken; tlexbuf ] typ)

(* -------------------------------------------------------------------------- *)

(* When the table back-end is active, the generated parser contains,
   as a sub-module, an application of [Engine.Make]. This sub-module
   is named as follows. *)

let interpreter =
  "MenhirInterpreter"

let checkpoint t =
  TypApp (interpreter ^ ".checkpoint", [ t ])

let lr1state =
  "lr1state"

let tlr1state a : typ =
  TypApp (lr1state, [a])

(* -------------------------------------------------------------------------- *)

(* The name of the sub-module that contains the incremental entry points. *)

let incremental =
  "Incremental"

(* The type of the incremental entry point for the start symbol [symbol]. *)

let entrytypescheme_incremental grammar symbol =
  let t = TypTextual (ocamltype_of_start_symbol grammar symbol) in
  type2scheme (marrow [ tposition ] (checkpoint t))

(* -------------------------------------------------------------------------- *)

(* The name of the sub-module that contains the inspection API. *)

let inspection =
  "Inspection"

(* -------------------------------------------------------------------------- *)

(* The monolithic (traditional) API: the type [token], the exception [Error],
   and the parser's entry points. *)

let monolithic_api grammar =

  TokenType.tokentypedef grammar @

  IIComment "This exception is raised by the monolithic API functions." ::
  IIExcDecls [ excdef ] ::

  IIComment "The monolithic API." ::
  IIValDecls (
    StringSet.fold (fun symbol decls ->
      (Misc.normalize symbol, entrytypescheme grammar symbol) :: decls
    ) grammar.start_symbols []
  ) ::

  []

(* -------------------------------------------------------------------------- *)

(* The inspection API. *)

let inspection_api grammar () =

  let a = "a" in

  (* Define the types [terminal] and [nonterminal]. *)

  TokenType.tokengadtdef grammar @
  NonterminalType.nonterminalgadtdef grammar @

  (* Include the signature that lists the inspection functions, with
     appropriate type instantiations. *)

  IIComment "The inspection API." ::
  IIInclude (
    with_types WKDestructive
      "MenhirLib.IncrementalEngine.INSPECTION" [
        [ a ], "lr1state", tlr1state (TypVar a);
        [], "production", TypApp ("production", []);
        [ a ], TokenType.tctokengadt, TokenType.ttokengadt (TypVar a);
        [ a ], NonterminalType.tcnonterminalgadt, NonterminalType.tnonterminalgadt (TypVar a);
        [ a ], "env", TypApp ("env", [ TypVar a ]);
      ]
  ) ::

  []

(* -------------------------------------------------------------------------- *)

(* The incremental API. *)

let incremental_engine () : module_type =
  with_types WKNonDestructive
    "MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE"
    [
      [],
      "token", (* NOT [tctoken], which is qualified if [--external-tokens] is used *)
      TokenType.ttoken
    ]

let incremental_entry_points grammar : interface =

  IIComment "The entry point(s) to the incremental API." ::
  IIModule (incremental, MTSigEnd [
    IIValDecls (
      StringSet.fold (fun symbol decls ->
        (symbol, entrytypescheme_incremental grammar symbol) :: decls
      ) grammar.start_symbols []
    )
  ]) ::

  []

let incremental_api grammar () : interface =

  IIModule (
    interpreter,
    MTSigEnd (
      IIComment "The incremental API." ::
      IIInclude (incremental_engine()) ::
      listiflazy Settings.inspection (inspection_api grammar)
    )
  ) ::

  (* The entry points must come after the incremental API, because
     their type refers to the type [checkpoint]. *)
  incremental_entry_points grammar

(* -------------------------------------------------------------------------- *)

(* The complete interface of the generated parser. *)

let interface grammar = [
  IIFunctor (grammar.parameters,
    monolithic_api grammar @
    listiflazy Settings.table (incremental_api grammar)
  )
]

(* -------------------------------------------------------------------------- *)

(* Writing the interface to a file. *)

let write grammar () =
  (* We have a dependency on [TokenType], which takes care of the case
     where [token_type_mode] is [TokenTypeOnly]. *)
  assert (Settings.token_type_mode <> Settings.TokenTypeOnly);
  let mli = open_out (Settings.base ^ ".mli") in
  let module P = Printer.Make (struct
    let f = mli
    let locate_stretches = None
  end) in
  P.interface (interface grammar);
  close_out mli
