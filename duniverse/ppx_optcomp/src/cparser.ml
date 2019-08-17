(* Parser for directives in C-like syntax, rewriting them into extensions,
   like ones we would get from parsing OCaml file.
*)

open Ppxlib

module Parsing  = Caml.Parsing

type lexer = Lexing.lexbuf -> Parser.token

(* +---------------------------------------------------------------+
   | Parsing of directives                                         |
   +---------------------------------------------------------------+ *)

let located x lexbuf =
  { Location.
    txt = x
  ; loc = Location.of_lexbuf lexbuf
  }
;;

let parse parsing_fun lexer lexbuf =
  try
    parsing_fun lexer lexbuf
  with Parsing.Parse_error | Syntaxerr.Escape_error ->
    let loc = Location.of_lexbuf lexbuf in
    raise (Syntaxerr.Error(Syntaxerr.Other loc))
;;

let fetch_directive_argument (lexer : lexer) lexbuf =
  let rec loop acc (brackets : Parser.token list) =
    match lexer lexbuf, brackets with
    | EOF, _ | EOL, [] -> located Parser.EOF lexbuf :: acc
    | (EOL | COMMENT _), _ -> loop acc brackets
    | token, _ ->
      let acc = located token lexbuf :: acc in
      match token, brackets with
      | BEGIN        , _ -> loop acc (END             :: brackets)
      | DO           , _ -> loop acc (DONE            :: brackets)
      | LPAREN       , _ -> loop acc (RPAREN          :: brackets)
      | LBRACE       , _ -> loop acc (RBRACE          :: brackets)
      | LBRACELESS   , _ -> loop acc (GREATERRBRACE   :: brackets)
      | LBRACKETLESS , _ -> loop acc (GREATERRBRACKET :: brackets)
      | LBRACKETBAR  , _ -> loop acc (BARRBRACKET     :: brackets)
      | (LBRACKET
        | LBRACKETGREATER
        | LBRACKETPERCENT
        | LBRACKETPERCENTPERCENT
        | LBRACKETAT
        | LBRACKETATAT
        | LBRACKETATATAT), _ -> loop acc (RBRACKET :: brackets)
      | _, closing :: brackets when token = closing ->
        loop acc brackets
      | _ -> loop acc brackets
  in
  let start_pos = Lexing.lexeme_end_p lexbuf in
  match loop [] [] |> List.rev with
  | []     -> None
  | tokens ->
    let tokens = ref tokens in
    let fake_lexer (lexbuf : Lexing.lexbuf) : Parser.token =
      match !tokens with
      | [] -> EOF
      | token :: rest ->
        tokens := rest;
        lexbuf.lex_start_p <- token.loc.loc_start;
        lexbuf.lex_curr_p  <- token.loc.loc_end;
        token.txt
    in
    let fake_lexbuf = Lexing.from_function (fun _ _ -> assert false) in
    fake_lexbuf.lex_curr_p <- start_pos;
    match parse Parser.implementation fake_lexer fake_lexbuf with
    | []   -> None
    | [st] ->
      assert_no_attributes_in#structure_item st;
      Some st
    | _ :: st :: _ ->
      Location.raise_errorf ~loc:st.pstr_loc "optcomp: too many structure items"
;;

let parse_directive (lexer : lexer) lexbuf : ('a Token.t) =
  let token = located (lexer lexbuf) lexbuf in
  let arg = fetch_directive_argument lexer lexbuf in
  let loc = { token.loc with loc_end = Lexing.lexeme_end_p lexbuf } in
  let payload = match arg with
    | Some st_item -> PStr [st_item]
    | None -> PStr []
  in
  match token.txt with
    | IF                -> Token.make_directive "if" loc payload
    | ELSE              -> Token.make_directive "else" loc payload
    | LIDENT s          -> Token.make_directive s loc payload
    | _ -> Location.raise_errorf ~loc "optcomp: unknown token"

let parse_loop lexbuf =
  let is_beginning_of_line lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    pos.pos_cnum = pos.pos_bol
  in
  let rec parse_loop_aux acc =
    match Lexer.token_with_comments lexbuf with
    | HASH when is_beginning_of_line lexbuf ->
      let acc = parse_directive Lexer.token_with_comments lexbuf :: acc in
      parse_loop_aux acc
    | EOF -> acc
    | _ -> parse_loop_aux acc
  in List.rev (parse_loop_aux [])
