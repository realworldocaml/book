open Base

let choose ~default body =
  let terminators = Lexer.extract_quoted_string_terminators body in
  let rec loop tag =
    if List.mem terminators tag ~equal:String.equal then loop (tag ^ "x") else tag
  in
  if List.mem terminators default ~equal:String.equal
  then loop (if String.is_empty default then "xxx" else default ^ "_xxx")
  else default
;;
