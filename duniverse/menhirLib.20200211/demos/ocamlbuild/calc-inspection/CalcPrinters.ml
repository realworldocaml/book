open Parser.MenhirInterpreter

(* In order to print syntax error messages and/or debugging information, we
   need a symbol printer. *)

let print_symbol symbol : string =
  match symbol with
  | X (T T_TIMES) ->
      "*"
  | X (T T_RPAREN) ->
      ")"
  | X (T T_PLUS) ->
      "+"
  | X (T T_MINUS) ->
      "-"
  | X (T T_LPAREN) ->
      "("
  | X (T T_INT) ->
      "INT"
  | X (N N_expr) ->
      "expr"
  | X (N N_main) ->
      "main"
  | X (T T_EOL) ->
      "EOL"
  | X (T T_DIV) ->
      "/"
  | X (T T_error) ->
      "error"

(* In order to print a view of the stack that includes semantic values,
   we need an element printer. (If we don't need this feature, then
   [print_symbol] above suffices.) *)

let print_element e : string =
  match e with
  | Element (s, v, _, _) ->
      match incoming_symbol s with
      | T T_TIMES ->
          "*"
      | T T_RPAREN ->
          ")"
      | T T_PLUS ->
          "+"
      | T T_MINUS ->
          "-"
      | T T_LPAREN ->
          "("
      | T T_INT ->
          string_of_int v
      | N N_expr ->
          string_of_int v
      | N N_main ->
          string_of_int v
      | T T_EOL ->
          ""
      | T T_DIV ->
          "/"
      | T T_error ->
          "error"

(* The public functions. *)

let print = prerr_string
let print_symbol s = print (print_symbol s)
let print_element = Some (fun s -> print (print_element s))

