(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open BasicSyntax

(**Under the simplified strategy, [filter_grammar] warns when the [error]
   token used elsewhere than at the end of a production, and returns a grammar
   where the faulty productions have been removed. Under the legacy strategy,
   [filter_grammar] is an identity function. *)
val filter_grammar : grammar -> grammar

(* Why do we do this? The simplified strategy does not query the lexer for a
   new token after shifting the [error] token. This implies that the [error]
   token remains on the stream. If the [error] token is used at the end of a
   production, then this is not a problem; after we shift, a reduction must
   take place, whose semantic action will abort the parser. However, if the
   [error] token was used inside a production, then we could very well fall
   into an endless shift cycle. *)

(**Under the simplified strategy, the use of the $syntaxerror keyword is
   disallowed. This keyword is dubious, for several reasons. First, it means
   that some sentences that seem legal (according to the grammar) are in
   reality rejected. This causes confusion when generating tests, when
   generating syntax error messages, etc. Second, $syntaxerror is used to
   generate a syntax error during a reduction, so the error is behind us
   already, yet the next token is replaced with an error token, leading to a
   nonsensical error location. Better get rid of this mechanism, which is
   seldom used anyway. [check_grammar] reports an error if $syntaxerror is
   used. *)
val check_grammar : grammar -> unit
