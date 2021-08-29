{
  type inline_element = [
    | `Text of string
    | `Code of string
  ]

  type block = [
    | `Paragraph of inline_element list
    | `Pre of string
  ]

  type document = block list

  let close_paragraph a1 a2 a3 =
    let a2 =
      match String.concat "" (List.rev a3) with
          "" -> a2
        | s -> `Text s :: a2
    in
    match List.rev a2 with
        [] -> a1
      | l -> `Paragraph l :: a1
}

let space = [' ' '\t' '\r' '\n']
let space' = space#['\n']

let par_special = ['\\' '{' '}']
let par_not_special = [^ '\\' '{' '}' ' ' '\t' '\r' '\n']
let verb_not_special = [^ '\\' ' ' '\t' '\r' '\n' '}']


(*
  Paragraph mode
*)
rule paragraph a1 a2 a3 = parse
    '\\' ('\\' | "{{" | "{{{" as s)
                        { paragraph a1 a2 (s :: a3) lexbuf }
  | "{{"
                        { let code = inline_verbatim [] lexbuf in
                          let a2 =
                            match String.concat "" (List.rev a3) with
                                "" -> a2
                              | s -> `Text s :: a2
                          in
                          let a2 = `Code code :: a2 in
                          paragraph a1 a2 [] lexbuf
                        }
  | space* "{{{" (("\r"?) "\n")?
                        { let pre = verbatim [] lexbuf in
                          let a1 = close_paragraph a1 a2 a3 in
                          let a1 = `Pre pre :: a1 in
                          paragraph a1 [] [] lexbuf
                        }
  | par_not_special+ as s
                        { paragraph a1 a2 (s :: a3) lexbuf }
  | space'* "\n"? space'*
                        { paragraph a1 a2 (" " :: a3) lexbuf }
  | space'* "\n" (space'* "\n")+ space'*
                        { let a1 = close_paragraph a1 a2 a3 in
                          paragraph a1 [] [] lexbuf
                        }
  | space* eof          { let a1 = close_paragraph a1 a2 a3 in
                          List.rev a1 }

  | _ as c              { paragraph a1 a2 (String.make 1 c :: a3) lexbuf }



(*
  Inline verbatim mode:
  Only "}}" need to be escaped.
  Backslashes can be escaped but single backslashes are tolerated.
*)
and inline_verbatim accu = parse
    "\\\\"              { inline_verbatim ("\\" :: accu) lexbuf }
  | "\\}}"              { inline_verbatim ("}}" :: accu) lexbuf }
  | space+              { inline_verbatim (" " :: accu) lexbuf }
  | verb_not_special+ as s
                        { inline_verbatim (s :: accu) lexbuf }
  | _ as c              { inline_verbatim (String.make 1 c :: accu) lexbuf }

  | space* "}}"         { String.concat "" (List.rev accu) }

  | eof                 { failwith "Missing `}}'" }


(*
  Verbatim paragraph mode:
  Only "}}}" need to be escaped.
  Backslashes can be escaped but single backslashes are tolerated.
*)
and verbatim accu = parse
    "\\\\"              { verbatim ("\\" :: accu) lexbuf }
  | "\\}}}"             { verbatim ("}}}" :: accu) lexbuf }
  | '\t'                { verbatim ("        " :: accu) lexbuf }
  | "\r\n"              { verbatim ("\n" :: accu) lexbuf }
  | verb_not_special+ as s
                        { verbatim (s :: accu) lexbuf }
  | _ as c              { verbatim (String.make 1 c :: accu) lexbuf }

  | ('\r'? '\n')? "}}}" { String.concat "" (List.rev accu) }

  | eof                 { failwith "Missing `}}}'" }

{
  let parse_string s =
    let lexbuf = Lexing.from_string s in
    paragraph [] [] [] lexbuf
}
