open Lexing

let process (line : string) =
  let linebuf = from_string line in
  linebuf.lex_curr_p <- { pos_fname = "-"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try
    (* Run the parser on this line of input. Print the resulting tree.
       We construct a tree and print it afterwards (instead of printing
       within the semantic actions) because %inline reorders side effects,
       and that would prevent us from comparing the positions. *)
    Aux.Print.main (Parser.main Lexer.token linebuf)
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parsing.Parse_error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (lexeme_start linebuf)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat lexbuf =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line lexbuf in
  process optional_line;
  if continue then
    repeat lexbuf

let ()  =
  repeat (from_channel stdin)

