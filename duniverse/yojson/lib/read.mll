{
  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  open Printf
  open Lexing

  (* see description in common.mli *)
  type lexer_state = Lexer_state.t = {
    buf : Bi_outbuf.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }

  let dec c =
    Char.code c - 48

  let hex c =
    match c with
        '0'..'9' -> int_of_char c - int_of_char '0'
      | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
      | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
      | _ -> assert false

  let custom_error descr v lexbuf =
    let offs = lexbuf.lex_abs_pos - 1 in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol - 1 in
    let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol) in
    let file_line =
      match v.fname with
          None -> "Line"
        | Some s ->
            sprintf "File %s, line" s
    in
    let bytes =
      if pos1 = pos2 then
        sprintf "byte %i" (pos1+1)
      else
        sprintf "bytes %i-%i" (pos1+1) (pos2+1)
    in
    let msg = sprintf "%s %i, %s:\n%s" file_line v.lnum bytes descr in
    json_error msg


  let lexer_error descr v lexbuf =
    custom_error
      (sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
      v lexbuf

  let read_junk = ref (fun _ -> assert false)

  let long_error descr v lexbuf =
    let junk = Lexing.lexeme lexbuf in
    let extra_junk = !read_junk lexbuf in
    custom_error
      (sprintf "%s '%s%s'" descr junk extra_junk)
      v lexbuf

  let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
  let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

  exception Int_overflow

  let extract_positive_int lexbuf =
    let start = lexbuf.lex_start_pos in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n >= max10 then
        raise Int_overflow
      else
        n := 10 * !n + dec (Bytes.get s i)
    done;
    if !n < 0 then
      raise Int_overflow
    else
      !n

  let make_positive_int v lexbuf =
    #ifdef INT
      try `Int (extract_positive_int lexbuf)
      with Int_overflow ->
    #endif
      #ifdef INTLIT
        `Intlit (lexeme lexbuf)
      #else
        lexer_error "Int overflow" v lexbuf
      #endif

  let extract_negative_int lexbuf =
    let start = lexbuf.lex_start_pos + 1 in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n <= min10 then
        raise Int_overflow
      else
        n := 10 * !n - dec (Bytes.get s i)
    done;
    if !n > 0 then
      raise Int_overflow
    else
      !n

  let make_negative_int v lexbuf =
    #ifdef INT
      try `Int (extract_negative_int lexbuf)
      with Int_overflow ->
    #endif
      #ifdef INTLIT
        `Intlit (lexeme lexbuf)
      #else
        lexer_error "Int overflow" v lexbuf
      #endif


  let set_file_name v fname =
    v.fname <- fname

  let newline v lexbuf =
    v.lnum <- v.lnum + 1;
    v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  let add_lexeme buf lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Bi_outbuf.add_subbytes buf lexbuf.lex_buffer lexbuf.lex_start_pos len

  let map_lexeme f lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    f (Bytes.to_string lexbuf.lex_buffer) lexbuf.lex_start_pos len

  type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
  type tuple_kind = [ `Parenthesis | `Square_bracket ]
}

let space = [' ' '\t' '\r']+

let digit = ['0'-'9']
let nonzero = ['1'-'9']
let digits = digit+
let frac = '.' digits
let e = ['e' 'E']['+' '-']?
let exp = e digits

let positive_int = (digit | nonzero digits)
let float = '-'? positive_int (frac | exp | frac exp)
let number = '-'? positive_int (frac | exp | frac exp)?

let hex = [ '0'-'9' 'a'-'f' 'A'-'F' ]

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

let optjunk4 = (eof | _ (eof | _ (eof | _ (eof | _))))
let optjunk8 = (eof | _ (eof | _ (eof | _ (eof | _ (eof | optjunk4)))))
let optjunk12 = (eof | _ (eof | _ (eof | _ (eof | _ (eof | optjunk8)))))
let optjunk16 = (eof | _ (eof | _ (eof | _ (eof | _ (eof | optjunk12)))))
let optjunk20 = (eof | _ (eof | _ (eof | _ (eof | _ (eof | optjunk16)))))
let optjunk24 = (eof | _ (eof | _ (eof | _ (eof | _ (eof | optjunk20)))))
let optjunk28 = (eof | _ (eof | _ (eof | _ (eof | _ (eof | optjunk24)))))
let optjunk32 = (eof | _ (eof | _ (eof | _ (eof | _ (eof | optjunk28)))))
let junk = optjunk32

rule read_json v = parse
  | "true"      { `Bool true }
  | "false"     { `Bool false }
  | "null"      { `Null }
  | "NaN"       {
                  #ifdef FLOAT
                    `Float nan
                  #elif defined FLOATLIT
                    `Floatlit "NaN"
                  #endif
                }
  | "Infinity"  {
                  #ifdef FLOAT
                    `Float infinity
                  #elif defined FLOATLIT
                    `Floatlit "Infinity"
                  #endif
                }
  | "-Infinity" {
                  #ifdef FLOAT
                    `Float neg_infinity
                  #elif defined FLOATLIT
                    `Floatlit "-Infinity"
                  #endif
                }
  | '"'         {
                  #ifdef STRING
                    Bi_outbuf.clear v.buf;
                    `String (finish_string v lexbuf)
                  #elif defined STRINGLIT
                    `Stringlit (finish_stringlit v lexbuf)
                  #endif
                }
  | positive_int         { make_positive_int v lexbuf }
  | '-' positive_int     { make_negative_int v lexbuf }
  | float       {
                  #ifdef FLOAT
                    `Float (float_of_string (lexeme lexbuf))
                  #elif defined FLOATLIT
                    `Floatlit (lexeme lexbuf)
                  #endif
                 }

  | '{'          { let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     let field_name = read_ident v lexbuf in
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     acc := (field_name, read_json v lexbuf) :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       let field_name = read_ident v lexbuf in
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       acc := (field_name, read_json v lexbuf) :: !acc;
                     done;
                     assert false
                   with End_of_object ->
                     `Assoc (List.rev !acc)
                 }

  | '['          { let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     acc := read_json v lexbuf :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       acc := read_json v lexbuf :: !acc;
                     done;
                     assert false
                   with End_of_array ->
                     `List (List.rev !acc)
                 }

  | '('          {
                   #ifdef TUPLE
                     let acc = ref [] in
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       acc := read_json v lexbuf :: !acc;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         acc := read_json v lexbuf :: !acc;
                       done;
                       assert false
                     with End_of_tuple ->
                       `Tuple (List.rev !acc)
                   #else
                     long_error "Invalid token" v lexbuf
                   #endif
                 }

  | '<'          {
                   #ifdef VARIANT
                     read_space v lexbuf;
                     let cons = read_ident v lexbuf in
                     read_space v lexbuf;
                     `Variant (cons, finish_variant v lexbuf)
                   #else
                     long_error "Invalid token" v lexbuf
                   #endif
                 }

  | "//"[^'\n']* { read_json v lexbuf }
  | "/*"         { finish_comment v lexbuf; read_json v lexbuf }
  | "\n"         { newline v lexbuf; read_json v lexbuf }
  | space        { read_json v lexbuf }
  | eof          { custom_error "Unexpected end of input" v lexbuf }
  | _            { long_error "Invalid token" v lexbuf }


and finish_string v = parse
    '"'           { Bi_outbuf.contents v.buf }
  | '\\'          { finish_escaped_char v lexbuf;
                    finish_string v lexbuf }
  | [^ '"' '\\']+ { add_lexeme v.buf lexbuf;
                    finish_string v lexbuf }
  | eof           { custom_error "Unexpected end of input" v lexbuf }

and map_string v f = parse
    '"'           { let b = v.buf in
                    f (Bytes.to_string b.Bi_outbuf.o_s) 0 b.Bi_outbuf.o_len }
  | '\\'          { finish_escaped_char v lexbuf;
                    map_string v f lexbuf }
  | [^ '"' '\\']+ { add_lexeme v.buf lexbuf;
                    map_string v f lexbuf }
  | eof           { custom_error "Unexpected end of input" v lexbuf }

and finish_escaped_char v = parse
    '"'
  | '\\'
  | '/' as c { Bi_outbuf.add_char v.buf c }
  | 'b'  { Bi_outbuf.add_char v.buf '\b' }
  | 'f'  { Bi_outbuf.add_char v.buf '\012' }
  | 'n'  { Bi_outbuf.add_char v.buf '\n' }
  | 'r'  { Bi_outbuf.add_char v.buf '\r' }
  | 't'  { Bi_outbuf.add_char v.buf '\t' }
  | 'u' (hex as a) (hex as b) (hex as c) (hex as d)
         { let x =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if x >= 0xD800 && x <= 0xDBFF then
             finish_surrogate_pair v x lexbuf
           else
             utf8_of_code v.buf x
         }
  | _    { long_error "Invalid escape sequence" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and finish_surrogate_pair v x = parse
  | "\\u" (hex as a) (hex as b) (hex as c) (hex as d)
         { let y =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if y >= 0xDC00 && y <= 0xDFFF then
             utf8_of_surrogate_pair v.buf x y
           else
             long_error "Invalid low surrogate for code point beyond U+FFFF"
               v lexbuf
         }
  | _    { long_error "Missing escape sequence representing low surrogate \
                       for code point beyond U+FFFF" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and finish_stringlit v = parse
    ( '\\' (['"' '\\' '/' 'b' 'f' 'n' 'r' 't'] | 'u' hex hex hex hex)
    | [^'"' '\\'] )* '"'
         { let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
           let s = Bytes.create (len+1) in
           Bytes.set s 0 '"';
           Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos s 1 len;
           Bytes.to_string s
         }
  | _    { long_error "Invalid string literal" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and finish_variant v = parse
    ':'  { let x = read_json v lexbuf in
           read_space v lexbuf;
           read_gt v lexbuf;
           Some x }
  | '>'  { None }
  | _    { long_error "Expected ':' or '>' but found" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and read_lt v = parse
    '<'      { () }
  | _        { long_error "Expected '<' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_gt v = parse
    '>'  { () }
  | _    { long_error "Expected '>' but found" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and read_comma v = parse
    ','  { () }
  | _    { long_error "Expected ',' but found" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and start_any_variant v = parse
    '<'      { `Edgy_bracket }
  | '"'      { Bi_outbuf.clear v.buf;
               `Double_quote }
  | '['      { `Square_bracket }
  | _        { long_error "Expected '<', '\"' or '[' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and finish_comment v = parse
  | "*/" { () }
  | eof  { long_error "Unterminated comment" v lexbuf }
  | '\n' { newline v lexbuf; finish_comment v lexbuf }
  | _    { finish_comment v lexbuf }




(* Readers expecting a particular JSON construct *)

and read_eof = parse
    eof       { true }
  | ""        { false }

and read_space v = parse
  | "//"[^'\n']* ('\n'|eof)  { newline v lexbuf; read_space v lexbuf }
  | "/*"                     { finish_comment v lexbuf; read_space v lexbuf }
  | '\n'                     { newline v lexbuf; read_space v lexbuf }
  | [' ' '\t' '\r']+         { read_space v lexbuf }
  | ""                       { () }

and read_null v = parse
    "null"    { () }
  | _         { long_error "Expected 'null' but found" v lexbuf }
  | eof       { custom_error "Unexpected end of input" v lexbuf }

and read_null_if_possible v = parse
    "null"    { true }
  | ""        { false }

and read_bool v = parse
    "true"      { true }
  | "false"     { false }

  (* tolerate booleans passed as strings without \u obfuscation *)
  | "\"true\""  { true }
  | "\"false\"" { false }

  | _           { long_error "Expected 'true' or 'false' but found" v lexbuf }
  | eof         { custom_error "Unexpected end of input" v lexbuf }

and read_int v = parse
    positive_int         { try extract_positive_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf }
  | '-' positive_int     { try extract_negative_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf }
  | '"'                  { (* Support for double-quoted "ints" *)
                           Bi_outbuf.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             int_of_string s
                           with _ ->
                             custom_error
                               "Expected an integer but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         }
  | _                    { long_error "Expected integer but found" v lexbuf }
  | eof                  { custom_error "Unexpected end of input" v lexbuf }

and read_int32 v = parse
    '-'? positive_int    { try Int32.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf }
  | '"'                  { (* Support for double-quoted "ints" *)
                           Bi_outbuf.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int32.of_string s
                           with _ ->
                             custom_error
                               "Expected an int32 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         }
  | _                    { long_error "Expected int32 but found" v lexbuf }
  | eof                  { custom_error "Unexpected end of input" v lexbuf }

and read_int64 v = parse
    '-'? positive_int    { try Int64.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf }
  | '"'                  { (* Support for double-quoted "ints" *)
                           Bi_outbuf.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int64.of_string s
                           with _ ->
                             custom_error
                               "Expected an int64 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         }
  | _                    { long_error "Expected int64 but found" v lexbuf }
  | eof                  { custom_error "Unexpected end of input" v lexbuf }

and read_number v = parse
  | "NaN"       { nan }
  | "Infinity"  { infinity }
  | "-Infinity" { neg_infinity }
  | number      { float_of_string (lexeme lexbuf) }
  | '"'         { Bi_outbuf.clear v.buf;
                  let s = finish_string v lexbuf in
                  try
                    (* Any OCaml-compliant float will pass,
                       including hexadecimal and octal notations,
                       and embedded underscores. *)
                    float_of_string s
                  with _ ->
                    match s with
                        "NaN" -> nan
                      | "Infinity" -> infinity
                      | "-Infinity" -> neg_infinity
                      | _ ->
                          custom_error
                            "Expected a number but found a string that \
                             doesn't even represent a number"
                            v lexbuf
                }
  | _           { long_error "Expected number but found" v lexbuf }
  | eof         { custom_error "Unexpected end of input" v lexbuf }

and read_string v = parse
    '"'      { Bi_outbuf.clear v.buf;
               finish_string v lexbuf }
  | _        { long_error "Expected '\"' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_ident v = parse
    '"'      { Bi_outbuf.clear v.buf;
               finish_string v lexbuf }
  | ident as s
             { s }
  | _        { long_error "Expected string or identifier but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and map_ident v f = parse
    '"'      { Bi_outbuf.clear v.buf;
               map_string v f lexbuf }
  | ident
             { map_lexeme f lexbuf }
  | _        { long_error "Expected string or identifier but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_sequence read_cell init_acc v = parse
    '['      { let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell !acc v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell !acc v lexbuf;
                 done;
                 assert false
               with End_of_array ->
                 !acc
             }
  | _        { long_error "Expected '[' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_list_rev read_cell v = parse
    '['      { let acc = ref [] in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell v lexbuf :: !acc;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell v lexbuf :: !acc;
                 done;
                 assert false
               with End_of_array ->
                 !acc
             }
  | _        { long_error "Expected '[' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_array_end = parse
    ']'      { raise End_of_array }
  | ""       { () }

and read_array_sep v = parse
    ','      { () }
  | ']'      { raise End_of_array }
  | _        { long_error "Expected ',' or ']' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }


and read_tuple read_cell init_acc v = parse
    '('          {
                   #ifdef TUPLE
                     let pos = ref 0 in
                     let acc = ref init_acc in
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       acc := read_cell !pos !acc v lexbuf;
                       incr pos;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         acc := read_cell !pos !acc v lexbuf;
                         incr pos;
                       done;
                       assert false
                     with End_of_tuple ->
                       !acc
                   #else
                     long_error "Invalid token" v lexbuf
                   #endif
                 }
  | _        { long_error "Expected ')' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_tuple_end = parse
    ')'      { raise End_of_tuple }
  | ""       { () }

and read_tuple_end2 v std = parse
    ')'      { if std then
                 long_error "Expected ')' or '' but found" v lexbuf
               else
                 raise End_of_tuple }
  | ']'      { if std then
                 raise End_of_tuple
               else
                 long_error "Expected ']' or '' but found" v lexbuf }
  | ""       { () }

and read_tuple_sep v = parse
    ','      { () }
  | ')'      { raise End_of_tuple }
  | _        { long_error "Expected ',' or ')' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_tuple_sep2 v std = parse
    ','      { () }
  | ')'      { if std then
                 long_error "Expected ',' or ']' but found" v lexbuf
               else
                 raise End_of_tuple }
  | ']'      { if std then
                 raise End_of_tuple
               else
                 long_error "Expected ',' or ')' but found" v lexbuf }
  | _        { long_error "Expected ',' or ')' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

(* Read a JSON object, reading the keys using a custom parser *)
and read_abstract_fields read_key read_field init_acc v = parse
    '{'      { let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_object_end lexbuf;
                 let field_name = read_key v lexbuf in
                 read_space v lexbuf;
                 read_colon v lexbuf;
                 read_space v lexbuf;
                 acc := read_field !acc field_name v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_object_sep v lexbuf;
                   read_space v lexbuf;
                   let field_name = read_key v lexbuf in
                   read_space v lexbuf;
                   read_colon v lexbuf;
                   read_space v lexbuf;
                   acc := read_field !acc field_name v lexbuf;
                 done;
                 assert false
               with End_of_object ->
                 !acc
             }
  | _        { long_error "Expected '{' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_lcurl v = parse
    '{'      { () }
  | _        { long_error "Expected '{' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_object_end = parse
    '}'      { raise End_of_object }
  | ""       { () }

and read_object_sep v = parse
    ','      { () }
  | '}'      { raise End_of_object }
  | _        { long_error "Expected ',' or '}' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_colon v = parse
    ':'      { () }
  | _        { long_error "Expected ':' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and start_any_tuple v = parse
    '('      { false }
  | '['      { true }
  | _        { long_error "Expected '(' or '[' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_lpar v = parse
    '('      { () }
  | _        { long_error "Expected '(' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_rpar v = parse
    ')'      { () }
  | _        { long_error "Expected ')' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_lbr v = parse
    '['      { () }
  | _        { long_error "Expected '[' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_rbr v = parse
    ']'      { () }
  | _        { long_error "Expected ']' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

(*** And now pretty much the same thing repeated,
     only for the purpose of skipping ignored field values ***)

and skip_json v = parse
  | "true"      { () }
  | "false"     { () }
  | "null"      { () }
  | "NaN"       { () }
  | "Infinity"  { () }
  | "-Infinity" { () }
  | '"'         { finish_skip_stringlit v lexbuf }
  | '-'? positive_int     { () }
  | float       { () }

  | '{'          { try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       skip_ident v lexbuf;
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with End_of_object ->
                     ()
                 }

  | '['          { try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with End_of_array ->
                     ()
                 }

  | '('          {
                   #ifdef TUPLE
                     try
                       read_space v lexbuf;
                       read_tuple_end lexbuf;
                       skip_json v lexbuf;
                       while true do
                         read_space v lexbuf;
                         read_tuple_sep v lexbuf;
                         read_space v lexbuf;
                         skip_json v lexbuf;
                       done;
                       assert false
                     with End_of_tuple ->
                       ()
                   #else
                     long_error "Invalid token" v lexbuf
                   #endif
                 }

  | '<'          {
                   #ifdef VARIANT
                     read_space v lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     finish_skip_variant v lexbuf
                   #else
                     long_error "Invalid token" v lexbuf
                   #endif
                 }

  | "//"[^'\n']* { skip_json v lexbuf }
  | "/*"         { finish_comment v lexbuf; skip_json v lexbuf }
  | "\n"         { newline v lexbuf; skip_json v lexbuf }
  | space        { skip_json v lexbuf }
  | eof          { custom_error "Unexpected end of input" v lexbuf }
  | _            { long_error "Invalid token" v lexbuf }


and finish_skip_stringlit v = parse
    ( '\\' (['"' '\\' '/' 'b' 'f' 'n' 'r' 't'] | 'u' hex hex hex hex)
    | [^'"' '\\'] )* '"'
         { () }
  | _    { long_error "Invalid string literal" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and finish_skip_variant v = parse
    ':'  { skip_json v lexbuf;
           read_space v lexbuf;
           read_gt v lexbuf }
  | '>'  { () }
  | _    { long_error "Expected ':' or '>' but found" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and skip_ident v = parse
    '"'      { finish_skip_stringlit v lexbuf }
  | ident    { () }
  | _        { long_error "Expected string or identifier but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

(*** And now pretty much the same thing repeated,
     only for the purpose of buffering deferred field values ***)

and buffer_json v = parse
  | "true"
  | "false"
  | "null"
  | "NaN"
  | "Infinity"
  | "-Infinity"
  | '-'? positive_int
  | float       { add_lexeme v.buf lexbuf }

  | '"'         { finish_buffer_stringlit v lexbuf }
  | '{'          { try
                     Bi_outbuf.add_char v.buf '{';
                     buffer_space v lexbuf;
                     buffer_object_end v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     buffer_colon v lexbuf;
                     buffer_space v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_object_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_ident v lexbuf;
                       buffer_space v lexbuf;
                       buffer_colon v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with End_of_object ->
                     ()
                 }

  | '['          { try
                     Bi_outbuf.add_char v.buf '[';
                     buffer_space v lexbuf;
                     buffer_array_end v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_array_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with End_of_array ->
                     ()
                 }

  | '('          {
                   #ifdef TUPLE
                     try
                       Bi_outbuf.add_char v.buf '(';
                       buffer_space v lexbuf;
                       buffer_tuple_end v lexbuf;
                       buffer_json v lexbuf;
                       while true do
                         buffer_space v lexbuf;
                         buffer_tuple_sep v lexbuf;
                         buffer_space v lexbuf;
                         buffer_json v lexbuf;
                       done;
                       assert false
                     with End_of_tuple ->
                       ()
                   #else
                     long_error "Invalid token" v lexbuf
                   #endif
                 }

  | '<'          {
                   #ifdef VARIANT
                     Bi_outbuf.add_char v.buf '<';
                     buffer_space v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     finish_buffer_variant v lexbuf
                   #else
                     long_error "Invalid token" v lexbuf
                   #endif
                 }

  | "//"[^'\n']* { add_lexeme v.buf lexbuf; buffer_json v lexbuf }
  | "/*"         { Bi_outbuf.add_string v.buf "/*";
                   finish_buffer_comment v lexbuf;
                   buffer_json v lexbuf }
  | "\n"         { Bi_outbuf.add_char v.buf '\n';
                   newline v lexbuf;
                   buffer_json v lexbuf }
  | space        { add_lexeme v.buf lexbuf; buffer_json v lexbuf }
  | eof          { custom_error "Unexpected end of input" v lexbuf }
  | _            { long_error "Invalid token" v lexbuf }


and finish_buffer_stringlit v = parse
    ( '\\' (['"' '\\' '/' 'b' 'f' 'n' 'r' 't'] | 'u' hex hex hex hex)
    | [^'"' '\\'] )* '"'
         { Bi_outbuf.add_char v.buf '"';
           add_lexeme v.buf lexbuf
         }
  | _    { long_error "Invalid string literal" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and finish_buffer_variant v = parse
    ':'  { Bi_outbuf.add_char v.buf ':';
           buffer_json v lexbuf;
           buffer_space v lexbuf;
           buffer_gt v lexbuf }
  | '>'  { Bi_outbuf.add_char v.buf '>' }
  | _    { long_error "Expected ':' or '>' but found" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and buffer_ident v = parse
    '"'      { finish_buffer_stringlit v lexbuf }
  | ident    { add_lexeme v.buf lexbuf }
  | _        { long_error "Expected string or identifier but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and buffer_space v = parse
  | "//"[^'\n']* ('\n'|eof)  {
    add_lexeme v.buf lexbuf;
    newline v lexbuf;
    buffer_space v lexbuf }
  | "/*"                     {
    Bi_outbuf.add_string v.buf "/*";
    finish_buffer_comment v lexbuf;
    buffer_space v lexbuf }
  | '\n'                     {
    Bi_outbuf.add_char v.buf '\n';
    newline v lexbuf;
    buffer_space v lexbuf }
  | [' ' '\t' '\r']+         {
    add_lexeme v.buf lexbuf;
    buffer_space v lexbuf }
  | ""                       { () }

and buffer_object_end v = parse
    '}'      {
      Bi_outbuf.add_char v.buf '}';
      raise End_of_object }
  | ""       { () }

and buffer_object_sep v = parse
    ','      { Bi_outbuf.add_char v.buf ',' }
  | '}'      { Bi_outbuf.add_char v.buf '}'; raise End_of_object }
  | _        { long_error "Expected ',' or '}' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and buffer_array_end v = parse
    ']'      { Bi_outbuf.add_char v.buf ']'; raise End_of_array }
  | ""       { () }

and buffer_array_sep v = parse
    ','      { Bi_outbuf.add_char v.buf ',' }
  | ']'      { Bi_outbuf.add_char v.buf ']'; raise End_of_array }
  | _        { long_error "Expected ',' or ']' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and buffer_tuple_end v = parse
    ')'      {
      Bi_outbuf.add_char v.buf ')';
      raise End_of_tuple }
  | ""       { () }

and buffer_tuple_sep v = parse
    ','      { Bi_outbuf.add_char v.buf ',' }
  | ')'      { Bi_outbuf.add_char v.buf ')'; raise End_of_tuple }
  | _        { long_error "Expected ',' or ')' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and buffer_colon v = parse
    ':'      { Bi_outbuf.add_char v.buf ':' }
  | _        { long_error "Expected ':' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and buffer_gt v = parse
    '>'  { Bi_outbuf.add_char v.buf '>' }
  | _    { long_error "Expected '>' but found" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }

and finish_buffer_comment v = parse
  | "*/" { Bi_outbuf.add_string v.buf "*/" }
  | eof  { long_error "Unterminated comment" v lexbuf }
  | '\n' { Bi_outbuf.add_char v.buf '\n';
           newline v lexbuf;
           finish_buffer_comment v lexbuf }
  | _    { add_lexeme v.buf lexbuf; finish_buffer_comment v lexbuf }

and junk = parse
    junk     { Lexing.lexeme lexbuf }

{
  let _ = (read_json : lexer_state -> Lexing.lexbuf -> t)

  let read_t = read_json

  let () =
    read_junk := junk

  let read_int8 v lexbuf =
    let n = read_int v lexbuf in
    if n < 0 || n > 255 then
      lexer_error "Int8 overflow" v lexbuf
    else
      char_of_int n

  let read_list read_cell v lexbuf =
    List.rev (read_list_rev read_cell v lexbuf)

  let array_of_rev_list l =
    match l with
        [] -> [| |]
      | x :: tl ->
          let len = List.length l in
          let a = Array.make len x in
          let r = ref tl in
          for i = len - 2 downto 0 do
            a.(i) <- List.hd !r;
            r := List.tl !r
          done;
          a

  let read_array read_cell v lexbuf =
    let l = read_list_rev read_cell v lexbuf in
    array_of_rev_list l

  (* Read a JSON object, reading the keys into OCaml strings
     (provided for backward compatibility) *)
  let read_fields read_field init_acc v =
    read_abstract_fields read_ident read_field init_acc v

  let finish v lexbuf =
    read_space v lexbuf;
    if not (read_eof lexbuf) then
      long_error "Junk after end of JSON value:" v lexbuf

  let init_lexer = init_lexer

  let from_lexbuf v ?(stream = false) lexbuf =
    read_space v lexbuf;

    let x =
      if read_eof lexbuf then
        raise End_of_input
      else
        read_json v lexbuf
    in

    if not stream then
      finish v lexbuf;

    x


  let from_string ?buf ?fname ?lnum s =
    try
      let lexbuf = Lexing.from_string s in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with End_of_input ->
      json_error "Blank input data"

  let from_channel ?buf ?fname ?lnum ic =
    try
      let lexbuf = Lexing.from_channel ic in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with End_of_input ->
      json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  let stream_from_lexbuf v ?(fin = fun () -> ()) lexbuf =
    let stream = Some true in
    let f i =
      try Some (from_lexbuf v ?stream lexbuf)
      with
          End_of_input ->
            fin ();
            None
        | e ->
            (try fin () with fin_e -> raise (Finally (e, fin_e)));
            raise e
    in
    Stream.from f

  let stream_from_string ?buf ?fname ?lnum s =
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v (Lexing.from_string s)

  let stream_from_channel ?buf ?fin ?fname ?lnum ic =
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v ?fin lexbuf

  let stream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf v ~fin lexbuf

  type json_line = [ `Json of t | `Exn of exn ]

  let linestream_from_channel
      ?buf ?(fin = fun () -> ()) ?fname ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with
          None -> Some (Bi_outbuf.create 256)
        | Some _ -> buf
    in
    let f i =
      try
        let line = input_line ic in
        let lnum = lnum0 + i in
        Some (`Json (from_string ?buf ?fname ~lnum line))
      with
          End_of_file -> fin (); None
        | e -> Some (`Exn e)
    in
    Stream.from f

  let linestream_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    linestream_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s =
    pretty_to_string ?std (from_string s)

  let compact ?std s =
    to_string (from_string s)

  let validate_json _path _value = None
}
