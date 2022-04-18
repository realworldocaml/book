{ (* -*- tuareg -*- *)
  open Error
  open Parser
  open LexerHelper

  exception ExnEOF

(*


  let get_char_quoted s =
    let s' = String.sub s 1 (String.length (s - 2))


  let char_of_string_atom atom =
   ' '*)
}

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

rule token = parse
  (** Layout *)
  | "main:" { token lexbuf }
  | newline { next_line_and token lexbuf }
  | blank+  { token lexbuf               }
  (* char *)
  | "CHAR"   { CHAR ('c')                  }
  | "STRING" { STRING ("example string\n") }
  | "INT"    { INT(Int64.of_int 123456)    }
  (* atomic lexemes *)
  | "LET"                 { LET                 }
  | "TYPE"                { TYPE                }
  | "FUN"                 { FUN                 }
  | "FOR"                 { FOR                 }
  | "IN"                  { IN                  }
  | "REF"                 { REF                 }
  | "IF"                  { IF                  }
  | "ELSE"                { ELSE                }
  | "WHILE"               { WHILE               }
  | "EXTERN"              { EXTERN              }
  | "AND"                 { AND                 }
  | "SWITCH"              { SWITCH              }
  | "DO"                  { DO                  }
  | "TO"                  { TO                  }
  | "LCBRACK"             { LCBRACK             }
  | "RCBRACK"             { RCBRACK             }
  | "BACKSLASH"           { BACKSLASH           }
  | "SEMICOLON"           { SEMICOLON           }
  | "COMMA"               { COMMA               }
  | "ARROW"               { ARROW               }
  | "LPAR"                { LPAR                }
  | "RPAR"                { RPAR                }
  | "LBRACK"              { LBRACK              }
  | "RBRACK"              { RBRACK              }
  | "PIPEPIPE"            { PIPEPIPE            }
  | "EQUALQUESTION"       { EQUALQUESTION       }
  | "LANGLEEQUALQUESTION" { LANGLEEQUALQUESTION }
  | "RANGLEEQUALQUESTION" { RANGLEEQUALQUESTION }
  | "LANGLEQUESTION"      { LANGLEQUESTION      }
  | "RANGLEQUESTION"      { RANGLEQUESTION      }
  | "DOT"                 { DOT                 }
  | "EXCLAMATION"         { EXCLAMATION         }
  | "PIPE"                { PIPE                }
  | "COLON"               { COLON               }
  | "EQUAL"               { EQUAL               }
  | "PLUS"                { PLUS                }
  | "MINUS"               { MINUS               }
  | "STAR"                { STAR                }
  | "SLASH"               { SLASH               }
  | "LANGLE"              { LANGLE              }
  | "RANGLE"              { RANGLE              }
  | "DOUBLEAMPERSAND"     { DOUBLEAMPERSAND     }
  | "AMPERSAND"           { AMPERSAND           }
  | "COLONEQUAL"          { COLONEQUAL          }
  | "EOF"                 { EOF                 }
  (* identifiers *)
  | "LOWERCASE_ID"  { LOWERCASE_ID("lowercase_identifier") }
  | "UPPERCASE_ID"  { UPPERCASE_ID("Uppercase_identifier") }
  | "TYPE_VARIABLE" { TYPE_VARIABLE("`type_variable")      }
  | "UNDERSCORE"    { UNDERSCORE                           }
  (** Lexing error. *)
  | eof { raise ExnEOF }
  | _   { error "during lexing" (Position.cpos lexbuf) "unexpected character." }
