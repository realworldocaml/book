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

(* This is the conventional name of the nonterminal GADT, which describes the
   nonterminal symbols. *)

let tcnonterminalgadt =
  "nonterminal"

let tnonterminalgadt a =
  TypApp (tcnonterminalgadt, [ a ])

(* This is the conventional name of the data constructors of the nonterminal
   GADT. *)

let tnonterminalgadtdata nt =
  "N_" ^ Misc.normalize nt

(* This is the definition of the nonterminal GADT. Here, the data
   constructors have no value argument, but have a type index. *)

exception MissingOCamlType of string

let nonterminalgadtdef grammar =
  assert Settings.inspection;
  let comment, datadefs =
    try

      (* The ordering of this list matters. We want the data constructors
         to respect the internal ordering (as determined by [nonterminals]
         in [BasicSyntax]) of the nonterminal symbols. This may
         be exploited in the table back-end to allow an unsafe conversion
         of a data constructor to an integer code. See [n2i] in
         [InspectionTableInterpreter]. *)

      "The indexed type of nonterminal symbols.",
      List.map (fun nt ->
        let index =
          match ocamltype_of_symbol grammar nt with
          | Some t ->
              TypTextual t
          | None ->
              raise (MissingOCamlType nt)
        in
        {
          dataname = tnonterminalgadtdata nt;
          datavalparams = [];
          datatypeparams = Some [ index ]
        }
      ) (nonterminals grammar)

    with MissingOCamlType nt ->
      (* If the type of some nonterminal symbol is unknown, give up
         and define ['a nonterminal] as an abstract type. This is
         useful when we are in [--(raw)-depend] mode and we do not
         wish to fail. Instead, we produce a mock [.mli] file that
         is an approximation of the real [.mli] file. When we are
         not in [--(raw)-depend] mode, though, this is a problem.
         We display an error message and stop. *)
      Settings.(match infer with
      | IMDependRaw
      | IMDependPostprocess ->
          "The indexed type of nonterminal symbols (mock!).",
          []
      | IMNone ->
          Error.error [] "\
            the type of the nonterminal symbol %s is unknown.\n\
            When --inspection is set, the type of every nonterminal symbol must be known.\n\
            Please enable type inference (see --infer and --infer-read-reply)\n\
            or specify the type of every symbol via %%type declarations."
            nt
      | IMInfer
      | IMReadReply _ ->
          (* This should not happen: when [--infer] or [--infer-read-reply]
             is set, the types of all nonterminal symbols should be known. *)
          assert false
      | IMWriteQuery _ ->
          (* This should not happen: when [--infer-write-query] is set, we
             write a mock [.ml] file, but no [.mli] file, so this function
             should never be called. *)
          assert false)

  in
  [
    IIComment comment;
    IITypeDecls [{
      typename = tcnonterminalgadt;
      typeparams = [ "_" ];
      typerhs = TDefSum datadefs;
      typeconstraint = None
    }]
  ]
