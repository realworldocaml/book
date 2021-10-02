(* Internal module, not exposed *)

val parse :
  Warning.t list ref ->
  Token.t Loc.with_location Stream.t ->
  Ast.t * Warning.t list
