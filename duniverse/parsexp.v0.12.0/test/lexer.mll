rule assoc sexp_parser = parse
  | eof               { [] }
  | ' '* '\n'         { assoc sexp_parser lexbuf }
  | '#' [^'\n']* '\n' { assoc sexp_parser lexbuf }
  | (['a'-'z']+ as key) ' '* '=' ' '*
    { let value = Parsexp.Eager.Lexbuf_consumer.parse sexp_parser lexbuf in
      eol lexbuf;
      (key, value) :: assoc sexp_parser lexbuf
    }

and eol = parse
 | ' '* '\n' { () }
