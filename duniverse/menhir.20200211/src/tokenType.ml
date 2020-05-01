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

(* This module deals with a few details regarding the definition of
   the [token] type. In particular, if [--only-tokens] was specified,
   it emits the type definition and exits. *)

open BasicSyntax
open IL
open CodeBits

(* This is the conventional name of the [token] type, with no
   prefix. A prefix is possibly appended to it below, where
   [tctoken] is redefined before being exported. *)

let tctoken =
  "token"

let ttoken =
  TypApp (tctoken, [])

(* This is the conventional name of the token GADT, which describes
   the tokens. Same setup as above. *)

let tctokengadt =
  "terminal"

let ttokengadt a =
  TypApp (tctokengadt, [ a ])

(* This is the conventional name of the data constructors of
   the token GADT. *)

let ttokengadtdata token =
  "T_" ^ token

(* This is the definition of the type of tokens. It is defined as an algebraic
   data type, unless [--external-tokens M] is set, in which case it is defined
   as an abbreviation for the type [M.token]. *)

let tokentypedef grammar =
  let typerhs =
    match Settings.token_type_mode with
    | Settings.TokenTypeOnly
    | Settings.TokenTypeAndCode ->

        (* Algebraic data type. *)

        TDefSum (
          List.map (fun (tok, typo) -> {
            dataname = tok;
            datavalparams = (match typo with None -> [] | Some t -> [ TypTextual t ]);
            datatypeparams = None
          }) (typed_tokens grammar)
        )

    | Settings.CodeOnly m ->

        (* Type abbreviation. *)

        TAbbrev (TypApp (m ^ "." ^ tctoken, []))

  in
  [
    IIComment "The type of tokens.";
    IITypeDecls [{
      typename = tctoken;
      typeparams = [];
      typerhs;
      typeconstraint = None
    }]
  ]

(* This is the definition of the token GADT. Here, the data constructors have
   no value argument, but have a type index. *)

(* The token GADT is produced only when [Settings.inspection] is true. Thus,
   when [Settings.inspection] is false, we remain compatible with old versions
   of OCaml, without GADTs. *)

(* Although the [token] type does not include the [error] token (because this
   token is never produced by the lexer), the token GADT must include the
   [error] token (because this GADT must describe all of the tokens that are
   allowed to appear in a production). *)

(* It is defined as a generalized algebraic data type, unless
   [--external-tokens M] is set, in which case it is defined as an
   abbreviation for the type ['a M.tokengadt]. *)

let tokengadtdef grammar =
  assert Settings.inspection;
  let param, typerhs =
    match Settings.token_type_mode with
    | Settings.TokenTypeOnly
    | Settings.TokenTypeAndCode ->

        (* Generalized algebraic data type. *)

        let param = "_" in
        param,
        TDefSum (
          (* The ordering of this list matters. We want the data constructors
             to respect the internal ordering (as determined by [typed_tokens]
             in [BasicSyntax]) of the terminal symbols. This may be
             exploited in the table back-end to allow an unsafe conversion
             of a data constructor to an integer code. See [t2i] in
             [InspectionTableInterpreter]. *)
          {
            dataname = ttokengadtdata "error";
            datavalparams = [];
            datatypeparams = Some [ tunit ]
              (* the [error] token has a semantic value of type [unit] *)
          } ::
          List.map (fun (token, typo) -> {
            dataname = ttokengadtdata token;
            datavalparams = [];
            datatypeparams = Some [ match typo with None -> tunit | Some t -> TypTextual t ]
          }) (typed_tokens grammar)
        )

    | Settings.CodeOnly m ->

        (* Type abbreviation. *)

        let param = "a" in
        param,
        TAbbrev (TypApp (m ^ "." ^ tctokengadt, [ TypVar param ]))

  in
  [
    IIComment "The indexed type of terminal symbols.";
    IITypeDecls [{
      typename = tctokengadt;
      typeparams = [ param ];
      typerhs;
      typeconstraint = None
    }]
  ]

(* If we were asked to only produce a type definition, then
   do so and stop. *)

let produce_tokentypes grammar =
  match Settings.token_type_mode with
  | Settings.TokenTypeOnly ->

      (* Create both an .mli file and an .ml file. This is made
         necessary by the fact that the two can be different
         when there are functor parameters. *)

      let i =
        tokentypedef grammar @
        listiflazy Settings.inspection (fun () ->
          tokengadtdef grammar
        )
      in

      let module P =
        Printer.Make (struct
                        let f = open_out (Settings.base ^ ".mli")
                        let locate_stretches = None
                      end)
      in
      P.interface [
        IIFunctor (grammar.parameters, i)
      ];
      let module P =
        Printer.Make (struct
                        let f = open_out (Settings.base ^ ".ml")
                        let locate_stretches = None
                      end)
      in
      P.program [
        SIFunctor (grammar.parameters,
          interface_to_structure i
        )
      ];
      exit 0

  | Settings.CodeOnly _
  | Settings.TokenTypeAndCode ->
      ()

(* The token type and the token GADTs can be referred to via a short
   (unqualified) name, regardless of how they have been defined (either
   directly or as an abbreviation). However, their data constructors must
   be qualified if [--external-tokens] is set. *)

let tokenprefix id =
  match Settings.token_type_mode with
  | Settings.CodeOnly m ->
      m ^ "." ^ id
  | Settings.TokenTypeAndCode ->
      id
  | Settings.TokenTypeOnly ->
      id (* irrelevant, really *)

let tokendata =
  tokenprefix

let tokengadtdata token =
  tokenprefix (ttokengadtdata token)

