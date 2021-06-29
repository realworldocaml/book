(** Parsing of s-expression *)

open! Import

module type Parsexp = sig
  module Conv_error = Conv_error
  module Of_sexp_error = Of_sexp_error
  module Old_parser_cont_state = Old_parser_cont_state
  module Parse_error = Parse_error
  module Positions = Positions
  module Cst = Cst

  module type Conv = Conv.S
  module type Parser = Parser.S
  module type Eager_parser = Parser.S_eager

  (** Exception raised in case of a conversion error *)
  exception Of_sexp_error of Of_sexp_error.t

  (** Exception raised in case of a parsing error *)
  exception Parse_error of Parse_error.t

  module Single : Parser with type parsed_value = Sexp.t
  module Many : Parser with type parsed_value = Sexp.t list
  module Eager : Eager_parser with type parsed_value = Sexp.t
  module Single_and_positions : Parser with type parsed_value = Sexp.t * Positions.t
  module Many_and_positions : Parser with type parsed_value = Sexp.t list * Positions.t
  module Eager_and_positions : Eager_parser with type parsed_value = Sexp.t * Positions.t
  module Single_just_positions : Parser with type parsed_value = Positions.t
  module Many_just_positions : Parser with type parsed_value = Positions.t
  module Eager_just_positions : Eager_parser with type parsed_value = Positions.t
  module Many_cst : Parser with type parsed_value = Cst.t_or_comment list
  module Eager_cst : Eager_parser with type parsed_value = Cst.t_or_comment

  (*_ These type synonyms are introduced because ocaml <4.06
    do not support destructive substitutions with `type 'a t1 = t2`
    or `type t1 = 'a t2`. *)
  type 'a id = 'a
  type sexp_list = Sexp.t list

  module Conv_single :
    Conv
    with type 'a res := 'a id
     and type parsed_sexp := Sexp.t
     and type chunk_to_conv := Sexp.t

  module Conv_many :
    Conv
    with type 'a res := 'a list
     and type parsed_sexp := sexp_list
     and type chunk_to_conv := Sexp.t

  module Conv_many_at_once :
    Conv
    with type 'a res := 'a id
     and type parsed_sexp := sexp_list
     and type chunk_to_conv := sexp_list

  (*_ For tests *)
  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    module Automaton_stack = Automaton_stack
    module Parser_automaton = Parser_automaton
  end
end
