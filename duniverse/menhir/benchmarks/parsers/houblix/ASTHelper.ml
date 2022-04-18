open AST

(*int -> (int -> int)*)

let fresh_identifier =
  let count = ref (-1) in
  fun () ->
    incr count;
    Id ("id" ^ string_of_int !count)

let expr_of_apply_list l =
  let rec aux l =
    match l with
    | [] -> failwith "should never happen"
    | [ x ] -> x
    | x :: xs ->
        let located_xs = aux xs in
        Position.
          {
            value = Apply (located_xs, x);
            position = join located_xs.position x.position;
          }
  in
  Position.value (aux (List.rev l))

let define_of_list l e =
  let rec aux l =
    match l with
    | [] -> failwith "should never happen"
    | [ x ] -> Position.{ value = Define (x, e); position = e.position }
    | x :: xs ->
        let located_xs = aux xs in
        Position.
          { value = Define (x, located_xs); position = located_xs.position }
  in
  Position.value (aux l)

(*let string_of_token token =
  Parser.(
    match token with
    | LOWERCASE_ID s -> sprintf "LOWERCASE_ID(%s)" s
    | UPPERCASE_ID s -> sprintf "UPPERCASE_ID(%s)" s
    | TYPE_VARIABLE s -> sprintf "TYPE_VARIABLE(%s)" s
    | INT i -> sprintf "INT(%d)" (Int64.to_int i)
    | CHAR c -> sprintf "INT(%c)" c
    | STRING s -> sprintf "STRING(%s)" s
    | TYPE -> "TYPE"
    | LET -> "LET"
    | BACKSLASH -> "BACKSLASH"
    | UNDERSCORE -> "UNDERSCORE"
    | AND -> "AND"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | WHILE -> "WHILE"
    | DO -> "DO"
    | FOR -> "FOR"
    | TO -> "TO"
    | SWITCH -> "SWITCH"
    | REF -> "REF"
    | SEMICOLON -> "SEMICOLON"
    | DOUBLEAMPERSAND -> "DOUBLEAMPERSAND"
    | PIPEPIPE -> "PIPEPIPE"
    | EQUALQUESTION -> "EQUALQUESTION"
    | LANGLEEQUALQUESTION -> "LANGLEEQUALQUESTION"
    | RANGLEEQUALQUESTION -> "RANGLEEQUALQUESTION"
    | LANGLEQUESTION -> "LANGLEQUESTION"
    | RANGLEQUESTION -> "RANGLEQUESTION"
    | DOT -> "DOT"
    | EXCLAMATION -> "EXCLAMATION"
    | PIPE -> "PIPE"
    | COLON -> "COLON"
    | EQUAL -> "EQUAL"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | STAR -> "STAR"
    | SLASH -> "SLASH"
    | LANGLE -> "LANGLE"
    | RANGLE -> "RANGLE"
    | ARROW -> "ARROW"
    | COMMA -> "COMMA"
    | LPAR -> "LPAR"
    | RPAR -> "RPAR"
    | LBRACK -> "LBRACK"
    | RBRACK -> "RBRACK"
    | IN -> "IN"
    | FUN -> "FUN"
    | EXTERN -> "EXTERN"
    | COLONEQUAL -> "COLONEQUAL"
    | AMPERSAND -> "AMPERSAND"
    | LCBRACK -> "LCBRACK"
    | RCBRACK -> "RCBRACK"
    | EOF -> "EOF")*)
