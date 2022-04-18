(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let map = List.map
open PPrint
open Grammar
open Invariant
open StackLang

(* -------------------------------------------------------------------------- *)

(* Auxiliary functions. *)

let nl =
  hardline

let register r =
  string (Reg.export r)

let tag tag =
  string (Tag.print tag)

let label label =
  string (Label.export label)

(* Braces with spaces. *)

let braces doc =
  braces (space ^^ doc ^^ space)

let value v =
  match v with
  | VTag t ->
      tag t
  | VReg r ->
      register r
  | VUnit ->
      string "()"

let values vs =
  OCaml.tuple (map value vs)

let binding (r, v) =
  register r ^^ string " <- " ^^ value v

let bindings bs =
  let bs = Bindings.to_list bs in
  group (align (braces (
    separate (break 1 ^^ semi ^^ space) (map binding bs)
  )))

let bit0 =
  string "0"

let bit1 =
  string "1"

let bit b =
  if b then bit1 else bit0

let cell cell =
  (* The symbol. *)
  string (Symbol.print false (symbol cell)) ^^
  slash ^^
  (* This bit indicates whether a state is present. *)
  bit (holds_state cell)

let sharpcell c =
  space ^^ at ^^ space ^^ cell c

let final final =
  match final with
  | None ->
      string "<undetermined>"
  | Some nt ->
      string (Nonterminal.print false nt)

let cells cells =
  cells
  |> to_list
  |> map cell
  |> separate space

let block_type bty =
  nl ^^
  string "#### Stack: " ^^
  cells bty.stack ^^
  nl ^^
  string "#### Final: " ^^
  final bty.final

let states states =
  string "#### States:" ^^
  Tag.Map.fold (fun t bty doc ->
    nl ^^ string "| State " ^^ tag t ^^ nest 2 (block_type bty) ^^ doc
  ) states nl
  ^^ nl

let pattern p =
  match p with
  | PWildcard ->
      underscore
  | PReg r ->
      register r

let patterns ps =
  OCaml.tuple (map pattern ps)

let primitive prim =
  match prim with
  | PrimLexerCall [ lexer; lexbuf ] ->
      value lexer ^^ parens (value lexbuf)
  | PrimLexerCall _ ->
      assert false
  | PrimOCamlFieldAccess (v, f) ->
      value v ^^ dot ^^ string f
  | PrimOCamlAction (bs, _prod, _action) ->
      if Bindings.is_empty bs then
        string "<semantic action>"
      else
        align (bindings bs ^/^ string "<semantic action>")

let trace s =
  OCaml.string s

let tok_single tok r =
  utf8format "%s %s" (Terminal.print tok) (Reg.export r)

let tok_wildcard tok =
  utf8format "%s _" (Terminal.print tok)

let tokpat pat =
  match pat with
  | TokSingle (tok, r) ->
      tok_single tok r
  | TokMultiple toks ->
      toks
      |> TerminalSet.elements
      |> map tok_wildcard
      |> flow (break 1 ^^ bar ^^ space)

let tagpat (TagSingle t) =
  tag t

let branch (guard, body) =
  nl ^^ bar ^^ space ^^ guard ^^ string " ->" ^^ nest 4 body

let registers rs =
  let rs = Reg.Set.elements rs in
  align (flow (comma ^^ break 1) (map register rs))

let needed rs =
  nl ^^
  string "#### Needs: " ^^
  registers rs

(* -------------------------------------------------------------------------- *)

(* Instructions and blocks. *)

let rec typed_block tblock =
  (* The block's type. *)
  block_type tblock.block_type ^^
  (* The block's needed registers. *)
  needed tblock.needed ^^
  (* The block's code. *)
  block tblock.block

and instruction b =
  (* A single instruction. *)
  match b with
  | IPush (vs, c, _) ->
      string "PUSH " ^^ values vs ^^ sharpcell c
  | IPop (ps, c, _) ->
      string "POP  " ^^ patterns ps ^^ sharpcell c
  | IPeek (ps, c, _) ->
      string "PEEK " ^^ patterns ps ^^ sharpcell c
  | IDef (bs, _) ->
      string "DEF  " ^^ bindings bs
  | IPrim (p, prim, _) ->
      string "PRIM " ^^ pattern p ^^ string " = " ^^ primitive prim
  | ITrace (t, _) ->
      string "TRAC " ^^ trace t
  | IComment (s, _) ->
      string "#### " ^^ string s
  | IDead `Static ->
      string "DEAD (STATIC)"
  | IDead `Dynamic ->
      string "DEAD (DYNAMIC)"
  | IStop s ->
      string "STOP " ^^ (OCaml.int s)
  | IReturn (nt, v) ->
      string "RET  " ^^ value v ^^ space ^^ at ^^ space ^^ final (Some nt)
  | IJump l ->
      string "JUMP " ^^ label l
  | ICaseToken (r, _, _) ->
      string "CASE " ^^ register r ^^ string " OF"
  | ICaseTag (r, _) ->
      string "CASE " ^^ register r ^^ string " OF"

and block b =
  (* Print the first instruction, ... *)
  nl ^^
  instruction b ^^
  (* ... followed with its successor(s). *)
  match b with
  | IPush (_, _, b)
  | IPop (_, _, b)
  | IPeek (_, _, b)
  | IDef (_, b)
  | IPrim (_, _, b)
  | ITrace (_, b)
  | IComment (_, b) ->
      block b
  | IDead _ | IStop _ | IReturn _ | IJump _ ->
      empty
  | ICaseToken (_, branches, default) ->
      concat_map branch (
        map casetok_branch branches @
        casetok_default default
      )
  | ICaseTag (_, branches) ->
      concat_map branch (
        map casetag_branch branches
      )

and casetok_branch (pat, b) =
  tokpat pat, block b

and casetok_default default =
  match default with
  | Some body ->
      [ (underscore, block body) ]
  | None ->
      []

and casetag_branch (pat, b) =
  tagpat pat, block b

(* [entry_comment] and [routine] assume that [entry] is a map of labels to
   start symbols, that is, the reverse of [program.entry]. This is used to
   determine whether a label is an entry point. *)

let entry_comment entry l =
  match Label.Map.find l entry with
  | nt ->
      utf8format "#### Entry point for the start symbol %s" nt ^^ nl
  | exception Not_found ->
      empty

let routine entry (l, tblock) =
  entry_comment entry l ^^
  label l ^^
  colon ^^
  nest 2 (typed_block tblock) ^^
  nl ^^
  nl

let reverse (m : label StringMap.t) : string Label.Map.t =
  StringMap.fold (fun k x accu ->
    Label.Map.add x k accu
  ) m Label.Map.empty

let cfg program =
  (* Print every block. *)
  program.cfg
  |> Label.Map.bindings
  |> map (routine (reverse program.entry))
  |> concat

let program program =
  (* Print the data type [state]. *)
  states program.states ^^
  (* Print the code. *)
  cfg program

(* -------------------------------------------------------------------------- *)

(* Public functions. *)

let rfrac = 0.8
let width = 80

module ToChannel = struct

  type 'a printer =
    out_channel -> 'a -> unit

  let to_channel f channel x =
    let doc = f x in
    ToChannel.pretty rfrac width channel doc

  let instruction = to_channel instruction
  let block = to_channel block
  let typed_block = to_channel typed_block
  let states = to_channel states
  let program = to_channel program

end

module ToString = struct

  type 'a printer =
    'a -> string

  let to_string f x =
    let doc = f x in
    let buffer = Buffer.create 32 in
    ToBuffer.pretty rfrac width buffer doc;
    Buffer.contents buffer

  let final = to_string final
  let values = to_string values
  let patterns = to_string patterns

end
