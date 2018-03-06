(* Mini implementation of cram tests *)

{

type item = [
  | `Output  of string
  | `Command of string
  | `Comment of string
  | `Part    of string
]

}

let eol = '\n' | eof

rule file = parse
 | eof { [] }
 | "### " ([^'\n']* as str) eol { `Part str    :: file lexbuf }
 | "  $ " ([^'\n']* as str) eol { `Command str :: file lexbuf }
 | "  " ([^'\n']* as str) eol   { `Output  str :: file lexbuf }
 | ([^'\n']* as str) eol        { `Comment str :: file lexbuf }
