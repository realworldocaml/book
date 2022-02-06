{
let newline lexbuf = Lexing.new_line lexbuf
}

let eol = '\n' | eof
let ws = ' ' | '\t'

rule token = parse
 | eof           { [] }
 | "..." ws* eol { newline lexbuf; `Ellipsis :: token lexbuf }
 | '\n'          { newline lexbuf; `Output "" :: token lexbuf }
 | "# "          { let loc = Location.curr lexbuf in
                   let c = phrase [] (Buffer.create 8) lexbuf in
                   `Command (c, loc) :: token lexbuf }
 | ([^'#' '\n'] [^'\n']* as str) eol
                 { newline lexbuf; `Output str :: token lexbuf }
 | _ as c        { failwith (Printf.sprintf "unexpected character '%c'. Did you forget a space after the '#' at the start of the line?" c) }

and phrase acc buf = parse
  | ("\n"* as nl) "\n" ("  " | "\t")
      { newline lexbuf;
        for _ = 1 to (String.length nl) do
          newline lexbuf
        done;
        let nl = List.init (String.length nl) (fun _ -> "") in
        phrase (nl @ Buffer.contents buf :: acc) (Buffer.create 8) lexbuf }
  | eol      { newline lexbuf; List.rev (Buffer.contents buf :: acc) }
  | ";;" eol { newline lexbuf; List.rev ((Buffer.contents buf ^ ";;") :: acc) }
  | _ as c   { Buffer.add_char buf c; phrase acc buf lexbuf }

{
let token lexbuf =
  try newline lexbuf; token lexbuf
  with Failure e -> Misc.err lexbuf "incomplete toplevel entry: %s" e
}
