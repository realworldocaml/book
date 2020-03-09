{
open Result
open Astring

let line_ref = ref 1

let newline lexbuf =
  Lexing.new_line lexbuf;
  incr line_ref
}

let eol = '\n' | eof
let ws = ' ' | '\t'

rule text section = parse
  | eof { [] }
  | ("#"+ as n) " " ([^'\n']* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: text (Some section) lexbuf }
  | "```" ([^' ' '\n']* as h) ws* ([^'\n']* as l) eol
      { let header = if h = "" then None else Some h in
        let contents = block lexbuf in
        let labels = match Label.of_string l with
          | Ok labels -> labels
          | Error msgs ->
            let msgs = List.map (fun (`Msg (x : string)) -> x) msgs in
            let msg = String.concat ~sep:" " msgs in
            failwith msg
        in
        let value = Block.Raw in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        newline lexbuf;
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        newline lexbuf;
        `Block { Block.file; line; section; header; contents; labels; value }
        :: text section lexbuf }
  | ([^'\n']* as str) eol
      { newline lexbuf;
        `Text str :: text section lexbuf }

and block = parse
  | eof | "```" ws* eol    { [] }
  | ([^'\n'] * as str) eol { str :: block lexbuf }


and cram_text section = parse
  | eof { [] }
  | ("#"+ as n) " " ([^'\n']* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: cram_text (Some section) lexbuf }
  | "  " ([^'\n']* as first_line) eol
      { let header = Syntax.cram_default_header in
        let requires_empty_line, contents = cram_block lexbuf in
        let contents = first_line :: contents in
        let labels = [] in
        let value = Block.Raw in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        let rest = cram_text section lexbuf in
        `Block { Block.file; line; section; header; contents; labels; value }
        :: (if requires_empty_line then `Text "" :: rest else rest) }
  | "<-- non-deterministic" ws* ([^'\n']* as choice) eol
      { let header = Syntax.cram_default_header in
        let requires_empty_line, contents = cram_block lexbuf in
        let labels =
          match Label.interpret "non-deterministic" (Some (Eq, choice)) with
          | Ok label -> [label]
          | Error (`Msg msg) -> failwith msg
        in
        let value = Block.Raw in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        newline lexbuf;
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        let rest = cram_text section lexbuf in
        `Block { Block.file; line; section; header; contents; labels; value }
        :: (if requires_empty_line then `Text "" :: rest else rest) }
  | ([^'\n']* as str) eol
      { newline lexbuf;
        `Text str :: cram_text section lexbuf }

and cram_block = parse
  | eof { false, [] }
  | eol { newline lexbuf; true, [] }
  | "  " ([^'\n'] * as str) eol
      { let requires_empty_line, lst = cram_block lexbuf in
        requires_empty_line, str :: lst }

{
let token syntax lexbuf =
  try
    match syntax with
    | Syntax.Normal -> text      None lexbuf
    | Syntax.Cram   -> cram_text None lexbuf
  with Failure e -> Misc.err lexbuf "invalid code block: %s" e
}
