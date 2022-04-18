open Error

let next_line_and f lexbuf =
  Lexing.new_line lexbuf;
  f lexbuf

let unqote s = String.sub s 1 (String.length s - 2)

(* let error lexbuf =
  error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p) *)
let char_of_atom lexbuf atom =
  match atom with
  | {|\n|} -> '\n'
  | {|\t|} -> '\t'
  | {|\b|} -> '\b'
  | {|\r|} -> '\r'
  | {|\\|} -> '\\'
  | {|\'|} -> '\''
  | {|\"|} -> '"'
  | _ when String.length atom = 1 -> atom.[0]
  | _ when atom.[0] = {|\|}.[0] -> (
      try Char.chr (int_of_string (String.sub atom 1 (String.length atom - 1)))
      with Invalid_argument _ ->
        error "during lexing" (Position.cpos lexbuf) "" )
  | _ -> failwith "Should never happen"
