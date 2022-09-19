open! Base

module Quoted_string = struct
  type t =
    | Normal
    | After_backslash
    | After_backslash_cr
    | After_backslash_digit
    | After_backslash_2digits
    | After_backslash_x
    | After_backslash_x_hex
    | Ignoring_blanks
  [@@deriving enumerate, compare, sexp_of]
end

module Block_comment = struct
  type t =
    | Normal
    | After_pipe
    | After_hash
    | Quoted_string of Quoted_string.t
  [@@deriving enumerate, compare, sexp_of]
end

module Unquoted_string = struct
  type t =
    | Normal
    | After_hash
    | After_pipe
  [@@deriving enumerate, compare, sexp_of]
end

module type State = sig
  module Block_comment = Block_comment
  module Quoted_string = Quoted_string
  module Unquoted_string = Unquoted_string

  type t =
    | Whitespace
    | Error
    | After_cr (** After '\r' *)
    | Unquoted_string of Unquoted_string.t
    | Line_comment
    | After_hash
    | Quoted_string of Quoted_string.t
    | Block_comment of Block_comment.t
  [@@deriving enumerate, compare, sexp_of]

  include Comparator.S with type t := t

  val count : int
  val initial : t
  val old_parser_approx_cont_state : t -> string
  val of_int : int -> t
  val to_int : t -> int
end
