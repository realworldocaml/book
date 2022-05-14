open! Core
open! Import

module Trailing_newline : sig
  type t =
    [ `Missing_trailing_newline
    | `With_trailing_newline
    ]
  [@@deriving sexp_of]
end

val lines_of_contents : string -> string array * Trailing_newline.t

val warn_if_no_trailing_newline
  :  warn_if_no_trailing_newline_in_both:bool
  -> warn:(string -> unit)
  -> prev:Trailing_newline.t * string
  -> next:Trailing_newline.t * string
  -> unit

val binary_different_message
  :  config:Configuration.t
  -> prev_file:File_name.t
  -> prev_is_binary:bool
  -> next_file:File_name.t
  -> next_is_binary:bool
  -> string
