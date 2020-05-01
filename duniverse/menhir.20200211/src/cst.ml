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

open Grammar

(* Concrete syntax trees. *)

(* A concrete syntax tree is one of a leaf -- which corresponds to a
   terminal symbol; a node -- which corresponds to a non-terminal
   symbol, and whose immediate descendants form an expansion of that
   symbol; or an error leaf -- which corresponds to a point where the
   [error] pseudo-token was shifted. *)

type cst =
  | CstTerminal of Terminal.t
  | CstNonTerminal of Production.index * cst array
  | CstError

(* This is a (mostly) unambiguous printer for concrete syntax trees,
   in an sexp-like notation. *)

let rec pcst b = function
  | CstTerminal tok ->

      (* A leaf is denoted by a terminal symbol. *)

      Printf.bprintf b "%s" (Terminal.print tok)

  | CstNonTerminal (prod, csts) ->

      (* A node is denoted by a bracketed, whitespace-separated list,
         whose head is a non-terminal symbol (followed with a colon)
         and whose tail consists of the node's descendants. *)

      (* There is in fact some ambiguity in this notation, since we
         only print the non-terminal symbol that forms the left-hand
         side of production [prod], instead of the production itself.

         This abuse makes things much more readable, and should be
         acceptable for the moment. The cases where ambiguity actually
         arises should be rare. *)

      Printf.bprintf b "[%s:%a]"
        (Nonterminal.print false (Production.nt prod))
        pcsts csts

  | CstError ->

      (* An error leaf is denoted by [error]. *)

      Printf.bprintf b "error"

and pcsts b (csts : cst array) =
  Array.iter (fun cst ->
    Printf.bprintf b " %a" pcst cst
  ) csts

(* This is the public interface. *)

let wrap print f x =
  let b = Buffer.create 32768 in
  print b x;
  Buffer.output_buffer f b

let print =
  wrap pcst

(* This is a pretty-printer for concrete syntax trees. The notation is
   the same as that used by the above printer; the only difference is
   that the [Pprint] library is used to manage indentation. *)

open Pprint

let rec build : cst -> document = function
  | CstTerminal tok ->
      text (Terminal.print tok)
  | CstNonTerminal (prod, csts) ->
      brackets (
        group (
          text (Nonterminal.print false (Production.nt prod)) ^^
          colon ^^
          group (
            nest 2 (
              Array.fold_left (fun doc cst ->
                doc ^^ break1 ^^ build cst
              ) empty csts
            )
          ) ^^
          break0
        )
      )
  | CstError ->
      text "error"

let show f cst =
  Channel.pretty 0.8 80 f (build cst)

