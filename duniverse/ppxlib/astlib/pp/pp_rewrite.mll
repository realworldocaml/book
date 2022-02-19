rule rewrite is_current ocaml_version = parse
  |          "(*IF_CURRENT " ([^'*']* as s) "*)"
    { let chunk = if is_current
        then "             " ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version lexbuf
    }
  |          "(*IF_AT_LEAST " ([^'*' ' ']* as v) " " ([^'*']* as s) "*)"
    { let chunk = if (v <= ocaml_version)
        then "              " ^ String.make (String.length v + 1) ' ' ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version lexbuf
    }
  |          "(*IF_NOT_AT_LEAST " ([^'*' ' ']* as v) " " ([^'*']* as s) "*)"
    { let chunk = if not (v <= ocaml_version)
        then "                  " ^ String.make (String.length v + 1) ' ' ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version lexbuf
    }
  | _ as c
    { print_char c;
      rewrite is_current ocaml_version lexbuf
    }
  | eof { () }


