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

(* This module deals with the definition of the type that describes the
   nonterminal symbols. *)

(* This is the conventional name of the [nonterminal] GADT. This is an indexed
   type (i.e., it has one type parameter). Its data constructors carry zero
   value arguments. *)

val tcnonterminalgadt: string
val tnonterminalgadt: IL.typ -> IL.typ

(* [tnonterminalgadtdata nt] is the conventional name of the data constructor
   associated with the non-terminal symbol [nt]. *)

val tnonterminalgadtdata: string -> string

(* This is the definition of the [nonterminal] GADT, for use by the code
   generators. This definition can be constructed only if the type of every
   nonterminal symbol is known, either because the user has provided this
   information, or because [--infer] has been set and inference has been
   performed already. This definition is produced only in [--inspection]
   mode. *)

val nonterminalgadtdef: BasicSyntax.grammar -> IL.interface

(* When in [--(raw-)depend] mode, we are asked to produce a mock [.mli] file
   before [--infer] has run, which means that we are usually not able to
   construct the definition of the [nonterminal] GADT. This implies that the
   mock [.mli] file is a subset of the final [.mli] file. I believe that, when
   working with [ocamlbuild], this is not a problem. In fact, the mock [.mli]
   file could just as well be empty or absent, and things would still work: in
   principle, it is enough for us to publish which files we need in order to
   be able to type-check the real [.ml] file used by [--infer].  However, when
   working with [make], which is unable to mix the production of targets and
   the computation of dependencies, we additionally need to predict which
   files one will need in order to compile the real [.mli] and [.ml] files.
   Here, the fact that the mock [.mli] file is incomplete could in theory be a
   problem, leading to incomplete dependencies. The problem does not lie in
   the line [parser.ml parser.mli: ...] that we add; it lies in the lines
   produced by [ocamldep] itself, where the line [parser.cmi: ...] is missing
   some dependencies. *)

