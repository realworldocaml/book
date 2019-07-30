(**************************************************************************)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

exception Syntax_not_found of string

type t = {
  keywords : (string * Approx_tokens.token) list;
  lexer : (Lexing.lexbuf -> Approx_tokens.token) option
}

(** Register lexer extension.*)
val register : string ->
  ?keywords:(string * Approx_tokens.token) list ->
  ?lexer:(Lexing.lexbuf -> Approx_tokens.token) ->
  unit -> unit

(** Get available extensions *)
val available : unit -> string list

(** Find an extension by its name *)
val find : string -> t
