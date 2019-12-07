{
  (** Lexer: Lexer Specification for S-expressions *)

  open Printf
  open Lexing

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c -> c

  let lf = '\010'

  let dec_code c1 c2 c3 =
    100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

  let hex_code c1 c2 =
    let d1 = Char.code c1 in
    let val1 =
      if d1 >= 97 then d1 - 87
      else if d1 >= 65 then d1 - 55
      else d1 - 48 in
    let d2 = Char.code c2 in
    let val2 =
      if d2 >= 97 then d2 - 87
      else if d2 >= 65 then d2 - 55
      else d2 - 48 in
    val1 * 16 + val2

  let found_newline ({ lex_curr_p; _ } as lexbuf) diff =
    lexbuf.lex_curr_p <-
      {
        lex_curr_p with
        pos_lnum = lex_curr_p.pos_lnum + 1;
        pos_bol = lex_curr_p.pos_cnum - diff;
      }

  (* same length computation as in [Lexing.lexeme] *)
  let lexeme_len { lex_start_pos; lex_curr_pos; _ } = lex_curr_pos - lex_start_pos

  let main_failure lexbuf msg =
    let { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } = lexeme_start_p lexbuf in
    let msg =
      sprintf
        "Sexplib.Lexer.main: %s at line %d char %d"
        msg pos_lnum (pos_cnum - pos_bol)
    in
    failwith msg

  module type T = sig
    module Quoted_string_buffer : sig
      type t
      val create : int -> t
      val add_char : t -> char -> unit
      val add_subbytes : t -> bytes -> int -> int -> unit
      val add_lexeme : t -> lexbuf -> unit
      val clear : t -> unit
      val of_buffer : Buffer.t -> t
    end
    module Token : sig
      type t
      val lparen : t
      val rparen : t
      val eof : t
      val simple_string : string -> t
      val hash_semi : t
      val quoted_string : Lexing.position -> Quoted_string_buffer.t -> t
      type s = Quoted_string_buffer.t -> Lexing.lexbuf -> t
      val comment : string -> main:s -> s
      val block_comment : Lexing.position -> main:s -> s
    end
  end

  module Make (X : T) : sig
    val main : ?buf:Buffer.t -> Lexing.lexbuf -> X.Token.t
  end = struct (* BEGIN FUNCTOR BODY CONTAINING GENERATED CODE *)
    open X
}

let lf = '\010'
let lf_cr = ['\010' '\013']
let dos_newline = "\013\010"
let blank = [' ' '\009' '\012']
let unquoted = [^ ';' '(' ')' '"'] # blank # lf_cr
let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f' 'A'-'F']

let unquoted_start =
  unquoted # ['#' '|'] | '#' unquoted # ['|'] | '|' unquoted # ['#']

rule main buf = parse
  | lf | dos_newline { found_newline lexbuf 0;
                       main buf lexbuf }
  | blank+ { main buf lexbuf }
  | (';' (_ # lf_cr)*) as text { Token.comment text ~main buf lexbuf }
  | '(' { Token.lparen }
  | ')' { Token.rparen }
  | '"'
      {
        let pos = Lexing.lexeme_start_p lexbuf in
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf pos lexbuf;
        let tok = Token.quoted_string pos buf in
        Quoted_string_buffer.clear buf;
        tok
      }
  | "#;" { Token.hash_semi }
  | "#|"
      {
        let pos = Lexing.lexeme_start_p lexbuf in
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_block_comment buf [pos] lexbuf;
        let tok = Token.block_comment pos ~main buf lexbuf in
        Quoted_string_buffer.clear buf;
        tok
      }
  | "|#" { main_failure lexbuf "illegal end of comment" }
  | "#" "#"+ "|" unquoted* (* unquoted_start can match ##, so ##| (which should be
                              refused) would not not be parsed by this case if the regexp
                              on the left was not there *)
  | "|" "|"+ "#" unquoted*
  | unquoted_start unquoted* ("#|" | "|#") unquoted*
      { main_failure lexbuf "comment tokens in unquoted atom" }
  | "#" | "|" | unquoted_start unquoted* as str { Token.simple_string str }
  | eof { Token.eof }

and scan_string buf start = parse
  | '"' { Quoted_string_buffer.add_lexeme buf lexbuf; () }
  | '\\' lf [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 2 in
        found_newline lexbuf len;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' dos_newline [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 3 in
        found_newline lexbuf len;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
      {
        Quoted_string_buffer.add_char buf (char_for_backslash c);
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' (digit as c1) (digit as c2) (digit as c3)
      {
        let v = dec_code c1 c2 c3 in
        if v > 255 then (
          let { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } = lexeme_end_p lexbuf in
          let msg =
            sprintf
              "Sexplib.Lexer.scan_string: \
               illegal escape at line %d char %d: `\\%c%c%c'"
              pos_lnum (pos_cnum - pos_bol - 3)
              c1 c2 c3 in
          failwith msg);
        Quoted_string_buffer.add_char buf (Char.chr v);
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' 'x' (hexdigit as c1) (hexdigit as c2)
      {
        let v = hex_code c1 c2 in
        Quoted_string_buffer.add_char buf (Char.chr v);
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' (_ as c)
      {
        Quoted_string_buffer.add_char buf '\\';
        Quoted_string_buffer.add_char buf c;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | lf
      {
        found_newline lexbuf 0;
        Quoted_string_buffer.add_char buf lf;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | ([^ '\\' '"'] # lf)+
      {
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Quoted_string_buffer.add_subbytes buf lexbuf.lex_buffer ofs len;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | eof
      {
        let msg =
          sprintf
            "Sexplib.Lexer.scan_string: unterminated string at line %d char %d"
            start.pos_lnum (start.pos_cnum - start.pos_bol)
        in
        failwith msg
      }

and scan_block_comment buf locs = parse
  | ('#'* | '|'*) lf
      { Quoted_string_buffer.add_lexeme buf lexbuf;
        found_newline lexbuf 0; scan_block_comment buf locs lexbuf }
  | (('#'* | '|'*) [^ '"' '#' '|'] # lf)+
      { Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_block_comment buf locs lexbuf }
  | ('#'* | '|'*) '"'
      {
        Quoted_string_buffer.add_lexeme buf lexbuf;
        let cur = lexeme_end_p lexbuf in
        let start = { cur with pos_cnum = cur.pos_cnum - 1 } in
        scan_string buf start lexbuf;
        scan_block_comment buf locs lexbuf
      }
  | '#'+ '|'
    {
      Quoted_string_buffer.add_lexeme buf lexbuf;
      let cur = lexeme_end_p lexbuf in
      let start = { cur with pos_cnum = cur.pos_cnum - 2 } in
      scan_block_comment buf (start :: locs) lexbuf
    }
  | '|'+ '#'
      {
        Quoted_string_buffer.add_lexeme buf lexbuf;
        match locs with
        | [_] -> () (* the comment is finished *)
        | _ :: (_ :: _ as t) -> scan_block_comment buf t lexbuf
        | [] -> assert false  (* impossible *)
      }
  | eof
      {
        match locs with
        | [] -> assert false
        | { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } :: _ ->
            let msg =
              sprintf "Sexplib.Lexer.scan_block_comment: \
                unterminated block comment at line %d char %d"
                pos_lnum (pos_cnum - pos_bol)
            in
            failwith msg
      }

{ (* RESUME FUNCTOR BODY CONTAINING GENERATED CODE *)

    let main ?buf =
      let buf =
        match buf with
        | None -> Quoted_string_buffer.create 64
        | Some buf ->
          Buffer.clear buf;
          Quoted_string_buffer.of_buffer buf
      in
      main buf

  end (* END FUNCTOR BODY CONTAINING GENERATED CODE *)

  module Vanilla =
    Make (struct
      module Quoted_string_buffer = struct
        include Buffer
        let add_lexeme _ _ = ()
        let of_buffer b = b
      end
      module Token = struct
        open Parser
        type t = token
        type s = Quoted_string_buffer.t -> Lexing.lexbuf -> t
        let eof = EOF
        let lparen = LPAREN
        let rparen = RPAREN
        let hash_semi = HASH_SEMI
        let simple_string x = STRING x
        let quoted_string _ buf = STRING (Buffer.contents buf)
        let block_comment _pos ~main buf lexbuf =
          main buf lexbuf
        let comment _text ~main buf lexbuf =
          main buf lexbuf (* skip and continue lexing *)
      end
    end)

  module With_layout =
    Make (struct
      module Quoted_string_buffer = struct
        type t = {
          contents : Buffer.t;
          lexeme : Buffer.t;
        }
        let create n = {contents = Buffer.create n; lexeme = Buffer.create n}
        let of_buffer contents = { contents; lexeme = Buffer.create 64 }
        let add_char t ch = Buffer.add_char t.contents ch
        let add_subbytes t str ofs len = Buffer.add_subbytes t.contents str ofs len
        let add_lexeme t lexbuf = Buffer.add_string t.lexeme (Lexing.lexeme lexbuf)
        let clear t = Buffer.clear t.lexeme; Buffer.clear t.contents
      end
      module Token = struct
        open Parser_with_layout
        type t = token
        type s = Quoted_string_buffer.t -> Lexing.lexbuf -> t
        let eof = EOF
        let lparen = LPAREN
        let rparen = RPAREN
        let hash_semi = HASH_SEMI
        let simple_string x = STRING (x, None)
        let quoted_string pos {Quoted_string_buffer.contents; lexeme} =
          STRING (Buffer.contents contents, Some (pos, Buffer.contents lexeme))
        let block_comment pos ~main:_ {Quoted_string_buffer.contents = _; lexeme} _lexbuf =
          COMMENT (Buffer.contents lexeme, Some pos)
        let comment text ~main:_ _buf _lexbuf =
          COMMENT (text, None)
      end
    end)

  let main = Vanilla.main
  let main_with_layout = With_layout.main

}
