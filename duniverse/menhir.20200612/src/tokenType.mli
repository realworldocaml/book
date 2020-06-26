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

(* This module deals with the definitions of the type(s) that describe
   the tokens and the terminal symbols. *)

(* By default, following [ocamlyacc], we produce just one type, [token],
   which describes the tokens. A token contains a tag (a terminal symbol)
   and possibly a semantic value. *)

(* In addition to that, in [--inspection] mode only, we produce a GADT which
   describes the terminal symbols. A terminal symbol is just a tag; it does
   not carry a semantic value. *)

(* In this module, we also deal with [--only-tokens] and [--external-tokens].
   If [--only-tokens] is specified on the command line, [produce_tokentypes]
   emits the type definition(s) and exit. If [--external-tokens M] is set,
   then the token type and the token GADT are defined as abbreviations for
   [M.token] and ['a M.terminal]. *)

(* The conventional name of the [token] type, for use by the code
   generators. *)

val ttoken: IL.typ

(* [tokendata] maps the name of a token to a data constructor of the [token]
   type. (If [--external-tokens] is set, then it prefixes its argument with an
   appropriate OCaml module name. Otherwise, it is the identity.) *)

val tokendata: string -> string

(* The conventional name of the [terminal] type, a.k.a. the token GADT. This
   is an indexed type (i.e., it has one type parameter). Its data constructors
   carry zero value arguments. *)

val tctokengadt: string
val ttokengadt: IL.typ -> IL.typ

(* [tokengadtdata] maps the name of a token to a data constructor of the token
   GADT. *)

val tokengadtdata: string -> string

(* The definitions of the token type and of the token GADT, for use by the
   code generators. Each of these lists defines zero or one type. *)

val tokentypedef: BasicSyntax.grammar -> IL.interface
val tokengadtdef: BasicSyntax.grammar -> IL.interface

(* If [--only-tokens] is set, then [produce_tokentypes] writes the type
   definitions to the [.ml] and [.mli] files and stops Menhir. Otherwise,
   it does nothing. *)

val produce_tokentypes: BasicSyntax.grammar -> unit

