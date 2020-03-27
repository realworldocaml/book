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

(* This module drives the front-end. It opens and parses the input files,
   which yields a number of partial grammars. It joins these grammars, expands
   them to get rid of parameterized nonterminals, and performs reachability
   analysis. This yields a single unified grammar. It then performs type
   inference. This yields the grammar that the back-end works with (often
   through the interface provided by module [Grammar]). *)

val grammar: BasicSyntax.grammar

(* This flag tells whether the semantic actions have been type-checked. It is
   set if and only if either [--infer] or [--infer-read-reply] is in use. Note
   that the presence of a %type declaration for every nonterminal symbol is
   *not* sufficient for this flag to be set. Note also that, when
   [--infer-read-reply] is set, it could be the case that we have an
   out-of-date inferred [.mli] file, so the semantic actions could still be
   ill-typed. (The user is then at fault.) *)

val ocaml_types_have_been_checked: bool
