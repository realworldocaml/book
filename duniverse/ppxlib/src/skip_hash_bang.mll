{
open Lexing

let update_loc lexbuf lines_to_skip =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + lines_to_skip;
    pos_bol = pos.pos_cnum;
  }
}

rule skip_hash_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
      { update_loc lexbuf 3 }
  | "#!" [^ '\n']* '\n'
      { update_loc lexbuf 1 }
  | "" { () }
