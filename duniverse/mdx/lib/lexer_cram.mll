{
open Astring
let commands s = String.cuts ~sep:"\\\n> " s
}

let eol = '\n' | eof
let ws = ' ' | '\t'
let digit = ['0' - '9']

rule token = parse
 | eof               { [] }
 | "[" (digit+ as str) "]" ws* eol
                     { `Exit (int_of_string str) :: token lexbuf }
 | ws* "..." ws* eol { `Ellipsis :: token lexbuf }
 | "$ "              {
      let buf = Buffer.create 8 in
      let line, cont = command_line buf lexbuf in
      if cont then `Command_first line :: token lexbuf
      else `Command line :: token lexbuf
    }
 | "> "              {
      let buf = Buffer.create 8 in
      let line, cont = command_line buf lexbuf in
      if cont then `Command_cont line :: token lexbuf
      else `Command_last line :: token lexbuf
    }
 | eol               { `Output "" :: token lexbuf }
 | _ as c            {
     let buf = Buffer.create 8 in
     Buffer.add_char buf c;
     let line = line buf lexbuf in
     `Output line :: token lexbuf
   }

and command_line buf = parse
 | '\\' ws* eol { Buffer.contents buf, true }
 | eol          { Buffer.contents buf, false }
 | '\"'   {
     let xbuf = Buffer.create 8 in
     Buffer.add_char xbuf '\"';
     Buffer.add_string buf (string xbuf lexbuf);
     command_line buf lexbuf }
 | _ as c { Buffer.add_char buf c; command_line buf lexbuf }

and string buf = parse
 | "\\\"" as str { Buffer.add_string buf str; string buf lexbuf }
 | "\""          { Buffer.add_char buf '\"'; Buffer.contents buf }
 | _ as c        { Buffer.add_char buf c; string buf lexbuf }

and line buf = parse
 | eol    { Buffer.contents buf }
 | _ as c { Buffer.add_char buf c; line buf lexbuf }

{
let token lexbuf =
  try token lexbuf
  with Failure _ -> Misc.err lexbuf "incomplete cram test"
}
