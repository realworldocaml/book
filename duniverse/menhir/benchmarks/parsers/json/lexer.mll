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
  | "INT"    { INT 1000                      }
  | "FLOAT"  { FLOAT 0.756467                }
  | "ID"     { ID "identifier"               }
  | "STRING" { STRING "string string string" }
  (* Atomic lexemes *)
  | "TRUE"        { TRUE        }
  | "FALSE"       { FALSE       }
  | "NULL"        { NULL        }
  | "LEFT_BRACE"  { LEFT_BRACE  }
  | "RIGHT_BRACE" { RIGHT_BRACE }
  | "LEFT_BRACK"  { LEFT_BRACK  }
  | "RIGHT_BRACK" { RIGHT_BRACK }
  | "COLON"       { COLON       }
  | "COMMA"       { COMMA       }
  | "EOF"         { EOF         }
  (* Error *)
  | eof { raise ExnEOF }
  | _   { assert false }
