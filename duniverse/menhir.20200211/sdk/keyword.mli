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

(* The user can request position information either at several types:
   - a simple offset of type [int], e.g., via $startofs;
   - a position of type [Lexing.position], e.g., via $startpos;
   - a location, e.g., via $loc.
   A location is currently represented as a pair of positions, but
   this might change in the future; we may allow the user to choose
   a custom type of locations. *)

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
   side, which he must refer to by name. (Referring to its symbol
   by its position, using [$i], is permitted in the concrete
   syntax, but the lexer eliminates this form.)

   We add a new subject, [Before], which corresponds to [$endpos($0)]
   in concrete syntax. We adopt the (slightly awkward) convention that
   when the subject is [Before], the [where] component must be [WhereEnd].

   If [flavor] is [FlavorLocation], then [where] must be [WhereSymbolStart] or
   [WhereStart]. In the former case, [subject] must be [Left]; this
   corresponds to $sloc in concrete syntax. In the latter case, [subject] must
   be [Left] or [RightNamed _]; this corresponds to $loc and $loc(x) in
   concrete syntax. *)

type subject =
  | Before
  | Left
  | RightNamed of string

(* Keywords inside semantic actions. They allow access to semantic
   values or to position information. *)

type keyword =
  | Position of subject * where * flavor
  | SyntaxError

(* This maps a [Position] keyword to the name of the variable that the
   keyword is replaced with. *)

val posvar: subject -> where -> flavor -> string

(* Sets of keywords. *)

module KeywordSet : sig
  include Set.S with type elt = keyword
  val map: (keyword -> keyword) -> t -> t
end
