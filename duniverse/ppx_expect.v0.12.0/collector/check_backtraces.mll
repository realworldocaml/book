let forbidden = "Raised at " | "Called from " | "Raised by primitive operation "

rule check = parse
  | forbidden { true }
  | "" { not_at_bos lexbuf }

and not_at_bos = parse
  | [^'a'-'z' 'A'-'Z' '0'-'9' '_'] forbidden { true }
  | _ { not_at_bos lexbuf }
  | eof { false }

{
  let contains_backtraces s = check (Lexing.from_string s)
}
