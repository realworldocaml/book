open! Base
open Ppxlib

(** This module constructs a generic type-directed map function.

    Given
    - a map from type names [n] to functions [t_n : n -> r_n]
    - an input type [tau]

    Define the output type [tau'] to be the same as [tau] but with all (most) instances of
    [n] replaced by [r_n].

    This module constructs a function that maps values of type [tau] to values of type
    [tau']
*)

type replace_result =
  | Unchanged
  | Replaced

val build
  :  loc:location
  -> map:(string, expression, 'a) Base.Map.t
  -> core_type
  -> expression
  -> replace_result * expression
