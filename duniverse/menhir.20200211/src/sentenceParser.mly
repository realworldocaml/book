/******************************************************************************/
/*                                                                            */
/*                                   Menhir                                   */
/*                                                                            */
/*                       François Pottier, Inria Paris                        */
/*              Yann Régis-Gianas, PPS, Université Paris Diderot              */
/*                                                                            */
/*  Copyright Inria. All rights reserved. This file is distributed under the  */
/*  terms of the GNU General Public License version 2, as described in the    */
/*  file LICENSE.                                                             */
/*                                                                            */
/******************************************************************************/

/* This is two parsers in one. */

/* This parser is used to read the sentences provided on the standard input
   channel when [--interpret] is set. The entry point is [optional_sentence]. */

/* It is used also to read a [.messages] file. The entry point is [entry]. */

/* This parser must be compatible with both ocamlyacc and menhir, so we use
   $ notation, do not use Menhir's standard library, and collect positions
   manually. */

/* ------------------------------------------------------------------------ */
/* Tokens. */

%token COLON EOF EOL
%token<Grammar.Terminal.t * Lexing.position * Lexing.position> TERMINAL
%token<Grammar.Nonterminal.t * Lexing.position * Lexing.position> NONTERMINAL
%token<string> COMMENT
  /* only manually-written comments, beginning with a single # */

/* ------------------------------------------------------------------------ */
/* Types. */

%{

  open SentenceParserAux

  (* Removing the position information in a terminal or non-terminal symbol. *)

  let strip_symbol (x, _, _) = x

  (* Removing the position information in a sentence. *)

  let strip_sentence (nto, terminals) =
    Option.map strip_symbol nto,
    List.map strip_symbol terminals

  (* Computing the start and end positions of a sentence. *)

  let locate_sentence (nto, terminals) =
    let opening =
      match nto, terminals with
      | Some (_, opening, _), _
      | None, (_, opening, _) :: _ ->
          opening
      | None, [] ->
          Lexing.dummy_pos (* cannot happen *)
    and closing =
      match nto, List.rev terminals with
      | _, (_, _, closing) :: _
      | Some (_, _, closing), _ ->
          closing
      | None, [] ->
          Lexing.dummy_pos (* cannot happen *)
    in
    [Positions.import (opening, closing)],
    strip_sentence (nto, terminals)

%}

%type <located_sentence> located_sentence

%type <SentenceParserAux.sentence option> optional_sentence
%start optional_sentence

%type<SentenceParserAux.located_sentence SentenceParserAux.or_comment list> entry
%start entry

%%

/* ------------------------------------------------------------------------ */

/* An entry is a list of located sentences or comments. */
entry: located_sentences_or_comments EOF
  { $1 }

/* A list of located sentences or comments. */
located_sentences_or_comments:
  { [] }
| located_sentence located_sentences_or_comments { Thing   $1 :: $2 }
| COMMENT          located_sentences_or_comments { Comment $1 :: $2 }

/* A located sentence. */
located_sentence: sentence
  { locate_sentence $1 }

/* An optional sentence. */
optional_sentence:
| EOF
    { None }
| sentence
    { Some (strip_sentence $1) }

/* A sentence is a pair of an optional non-terminal start symbol and a list
   of terminal symbols. It is terminated by a newline. */
sentence:
| NONTERMINAL COLON terminals EOL
    { Some $1, $3 }
| terminals EOL
    { None, $1 }

/* A list of terminal symbols. */
terminals:
|
    { [] }
| TERMINAL terminals
    { $1 :: $2 }
