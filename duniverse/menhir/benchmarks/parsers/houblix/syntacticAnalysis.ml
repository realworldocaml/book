let parsing_step = "during parsing"

let process ~lexer_init ~lexer_fun ~parser_fun ~input =
  parser_fun lexer_fun (lexer_init input)

let process ~lexer_init ~lexer_fun ~parser_fun ~input =
  try process ~lexer_init ~lexer_fun ~parser_fun ~input with
  | Sys_error msg -> Error.global_error parsing_step msg
  (*| _ -> Error.global_error parsing_step "Syntax error."*)
