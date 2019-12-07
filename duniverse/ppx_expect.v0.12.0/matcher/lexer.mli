open Expect_test_common.Std

(** Strip all surrounding whitespace and return the result as a list of lines *)
val strip_surrounding_whitespaces : string -> unit Cst.t


val parse_pretty_line
  :  allow_output_patterns:bool
  -> string
  -> Fmt.t
val parse_pretty
  :  allow_output_patterns:bool
  -> string
  -> Fmt.t Cst.t
val parse_body
  :  allow_output_patterns:bool
  -> string Expectation.Body.t
  -> Fmt.t Cst.t Expectation.Body.t

val extract_quoted_string_terminators : string -> string list
