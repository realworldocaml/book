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

(* This module provides some type and function definitions
   that help deal with the keywords that we recognize within
   semantic actions. *)

(* ------------------------------------------------------------------------- *)
(* Types. *)

(* The user can request position information either at type
   [int] (a simple offset) or at type [Lexing.position]. *)

type flavor =
  | FlavorOffset
  | FlavorPosition
  | FlavorLocation

(* The user can request position information about the $start or $end
   of a symbol. Also, $symbolstart requests the computation of the
   start position of the first nonempty element in a production. *)

type where =
| WhereSymbolStart
| WhereStart
| WhereEnd

(* The user can request position information about a production's
   left-hand side or about one of the symbols in its right-hand
   side, which he can refer to by position or by name. *)

type subject =
  | Before
  | Left
  | RightNamed of string

(* Keywords inside semantic actions. They allow access to semantic
   values or to position information. *)

type keyword =
  | Position of subject * where * flavor
  | SyntaxError

(* ------------------------------------------------------------------------- *)
(* These auxiliary functions help map a [Position] keyword to the
   name of the variable that the keyword is replaced with. *)

let where = function
  | WhereSymbolStart ->
      "symbolstart"
  | WhereStart ->
      "start"
  | WhereEnd ->
      "end"

let subject = function
  | Before ->
      "__0_"
  | Left ->
      ""
  | RightNamed id ->
      Printf.sprintf "_%s_" id

let flavor = function
  | FlavorPosition ->
      "pos"
  | FlavorOffset ->
      "ofs"
  | FlavorLocation ->
      "loc"

let posvar s w f =
  match w, f with
  | _, (FlavorOffset | FlavorPosition) ->
      Printf.sprintf "_%s%s%s" (where w) (flavor f) (subject s)
  | WhereSymbolStart, FlavorLocation ->
      "_sloc"
  | WhereStart, FlavorLocation ->
      Printf.sprintf "_loc%s" (subject s)
  | _ ->
      assert false

(* ------------------------------------------------------------------------- *)
(* Sets of keywords. *)

module KeywordSet = struct

  include Set.Make (struct
    type t = keyword
    let compare = compare
  end)

  let map f keywords =
    fold (fun keyword accu ->
      add (f keyword) accu
    ) keywords empty

end
