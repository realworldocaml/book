
{
  open Import
  open Lexing
  open Parser

  let lexing_error lexbuf msg =
    let loc = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
    Ast.error (Ast.string_of_loc loc ^ "\n" ^ msg)

  type accu = { mutable depth : int;
                buf : Buffer.t }

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
                             pos_lnum = pos.pos_lnum + 1;
                             pos_bol = pos.pos_cnum }

  let int_of_dec c =
    match c with
        '0'..'9' -> Char.code c - 48
      | _ -> invalid_arg "int_of_dec"

  let int_of_hex c =
    match c with
        '0'..'9' -> Char.code c - 48
      | 'a'..'f' -> Char.code c - 87
      | 'A'..'F' -> Char.code c - 55
      | _ -> invalid_arg "int_of_hex"

  let byte_of_hex a b =
    Char.chr (int_of_hex a lsl 4 + int_of_hex b)

  let byte_of_dec a b c =
    let x = int_of_dec a * 100 + int_of_dec b * 10 + int_of_dec c in
    if x > 255 then
      invalid_arg "byte_of_dec"
    else
      Char.chr x

  let utf8_of_hex4 buf b1 b2 b3 b4 =
    (* covers only U+0000-U+FFFF *)
    let a = int_of_hex b1 lsl 4 + int_of_hex b2 in
    let b = int_of_hex b3 lsl 4 + int_of_hex b4 in
    let u = a lsl 8 + b in
    let add buf i = Buffer.add_char buf (Char.chr (i land 0xff)) in
    if u <= 0x007f then
      add buf u
    else if u <= 0x07ff then (
      add buf (0b11000000 lor (a lsl 2) lor (b lsr 6));
      add buf (0b10000000 lor (b land 0b00111111))
    )
    else if u <= 0xffff then (
      add buf (0b11100000 lor (a lsr 4));
      add buf (0b10000000 lor ((a lsl 2) land 0b00111100) lor (b lsr 6));
      add buf (0b10000000 lor (b land 0b00111111))
    )
    else invalid_arg "utf8_of_hex4"
(*
  let test_utf8_of_hex s =
    assert (String.length s = 4);
    let buf = Buffer.create 10 in
    utf8_of_hex4 buf s.[0] s.[1] s.[2] s.[3];
    let file = Filename.temp_file "debug" "" in
    let oc = open_out file in
    output_string oc (Buffer.contents buf);
    close_out oc;
    assert (Sys.command ("xxd -b " ^ file) = 0);
    Sys.remove file
*)
  ;;
}

let upper = ['A'-'Z']
let lower = ['a'-'z']
let digit = ['0'-'9']
let identchar = upper | lower | digit | ['_' '\'']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let lident = (lower | '_' identchar) identchar*
let uident = upper identchar*

let blank = [ ' ' '\t' ]
let newline = '\r'? '\n'
let space = [ ' ' '\t' '\r' '\n' ]

rule token = parse
  | "("      { OP_PAREN }
  | ")"      { CL_PAREN }
  | "["      { OP_BRACK }
  | "]"      { CL_BRACK }
  | "{"      { OP_CURL }
  | "}"      { CL_CURL }
  | "<"      { LT }
  | ">"      { GT }
  | ";"      { SEMICOLON }
  | ","      { COMMA }
  | ":"      { COLON }
  | "*"      { STAR }
  | "|"      { BAR }
  | "="      { EQ }
  | "?"      { QUESTION }
  | "~"      { TILDE }
  | "."      { DOT }
  | "type"   { TYPE }
  | "of"     { OF }
  | "inherit"   { INHERIT }
  | lident as s { LIDENT s }
  | uident as s { UIDENT s }
  | "'" (lident as s) { TIDENT s }
  | newline  { newline lexbuf; token lexbuf }
  | blank+   { token lexbuf }
  | eof      { EOF }
  | '"'      { STRING (string (Buffer.create 200) lexbuf) }
  | "(*"     { comment 1 lexbuf; token lexbuf }
  | _ as c   { lexing_error lexbuf
                 (sprintf "Illegal character %S" (String.make 1 c)) }

and string buf = parse
  | '"'       { Buffer.contents buf }
  | '\\' (['\\' '"'] as c)
              { Buffer.add_char buf c;
                string buf lexbuf }
  | "\\x" (hex as a) (hex as b)
              { Buffer.add_char buf (byte_of_hex a b);
                string buf lexbuf }
  | '\\' (digit as a) (digit as b) (digit as c)
              { Buffer.add_char buf (byte_of_dec a b c);
                string buf lexbuf }
  | "\\n"     { Buffer.add_char buf '\n'; string buf lexbuf }
  | "\\r"     { Buffer.add_char buf '\r'; string buf lexbuf }
  | "\\t"     { Buffer.add_char buf '\t'; string buf lexbuf }
  | "\\b"     { Buffer.add_char buf '\b'; string buf lexbuf }
  | '\n'      { newline lexbuf;
                Buffer.add_char buf '\n';
                string buf lexbuf }
  | '\\' newline blank*
              { newline lexbuf; string buf lexbuf }
  | '\\'      { lexing_error lexbuf "Invalid escape sequence" }
  | _ as c    { Buffer.add_char buf c; string buf lexbuf }
  | eof       { lexing_error lexbuf "Unterminated string" }

and comment depth = parse
  | "*)"      { if depth > 1 then
                  comment (depth - 1) lexbuf
              }
  | "(*"      { comment (depth + 1) lexbuf }
  | '"'       { ignore (string (Buffer.create 200) lexbuf);
                comment depth lexbuf }
  | newline   { newline lexbuf; comment depth lexbuf }
  | _         { comment depth lexbuf }
  | eof       { lexing_error lexbuf "Unterminated comment" }

{
  let init_fname lexbuf fname lnum =
    lexbuf.lex_start_p <- { lexbuf.lex_start_p
                            with
                              pos_fname = fname;
                              pos_lnum = lnum };
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p
                           with
                             pos_fname = fname;
                             pos_lnum = lnum }
}
