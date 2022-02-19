include Ocaml_common.Longident

let parse s =
  (*IF_NOT_AT_LEAST 411 parse s *)
  (*IF_AT_LEAST 411 Ocaml_common.Parse.longident @@ Lexing.from_string @@ s *)
