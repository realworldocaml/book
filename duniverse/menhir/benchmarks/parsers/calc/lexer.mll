{ (* -*- tuareg -*- *)
  open Parser

  let next_line_and f lexbuf =
    Lexing.new_line lexbuf;
    f lexbuf

  exception ExnEOF
}

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

rule token = parse
  (** Layout *)
  | "main:" { token lexbuf }
  | newline { next_line_and token lexbuf }
  | blank+  { token lexbuf               }
  (** With payload *)
  | "INT" { INT (Random.int 1000000000) }
  (* Atomic lexemes *)
  | "LPAREN" { LPAREN }
  | "RPAREN" { RPAREN }
  | "PLUS"   { PLUS   }
  | "MINUS"  { MINUS  }
  | "TIMES"  { TIMES  }
  | "DIV"    { DIV    }
  | "EOL"    { EOL    }
  (* Error *)
  | eof { raise ExnEOF  }
  | _   { assert false }
