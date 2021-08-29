open Ppxlib

let set_filename (lexbuf : Lexing.lexbuf) ~filename =
  { lexbuf with lex_curr_p = { lexbuf.lex_curr_p with pos_fname = filename } }

let _ =
  Lexing.from_channel stdin
  |> set_filename ~filename:"lexbuf_pos_fname"
  |> Parse.implementation |> Driver.map_structure
