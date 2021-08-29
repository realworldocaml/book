open! Import

module type Conv = Conv.S
module type Parser = Parser.S
module type Eager_parser = Parser.S_eager

module Conv_error = Conv_error
module Of_sexp_error = Of_sexp_error
module Old_parser_cont_state = Old_parser_cont_state
module Parse_error = Parse_error
module Positions = Positions
module Cst = Cst
module A = Parser_automaton

exception Parse_error = Parse_error.Parse_error
exception Of_sexp_error = Of_sexp_error.Of_sexp_error

module Single =
  Parser.Make
    (Kind.Sexp)
    (struct
      type parsed_value = Sexp.t

      let mode = A.Single
      let make_value _ stack = Automaton_stack.get_single stack
    end)

module Many =
  Parser.Make
    (Kind.Sexp)
    (struct
      type parsed_value = Sexp.t list

      let mode = A.Many
      let make_value _ stack = Automaton_stack.get_many stack
    end)

module Eager =
  Parser.Make_eager
    (Kind.Sexp)
    (struct
      type parsed_value = Sexp.t

      let make_value _ stack = Automaton_stack.get_single stack
    end)

module Single_and_positions =
  Parser.Make
    (Kind.Sexp_with_positions)
    (struct
      type parsed_value = Sexp.t * Positions.t

      let mode = A.Single
      let make_value state stack = Automaton_stack.get_single stack, A.positions state
    end)

module Many_and_positions =
  Parser.Make
    (Kind.Sexp_with_positions)
    (struct
      type parsed_value = Sexp.t list * Positions.t

      let mode = A.Many
      let make_value state stack = Automaton_stack.get_many stack, A.positions state
    end)

module Eager_and_positions =
  Parser.Make_eager
    (Kind.Sexp_with_positions)
    (struct
      type parsed_value = Sexp.t * Positions.t

      let make_value state stack = Automaton_stack.get_single stack, A.positions state
    end)

module Single_just_positions =
  Parser.Make
    (Kind.Positions)
    (struct
      type parsed_value = Positions.t

      let mode = A.Single
      let make_value state () = A.positions state
    end)

module Many_just_positions =
  Parser.Make
    (Kind.Positions)
    (struct
      type parsed_value = Positions.t

      let mode = A.Many
      let make_value state () = A.positions state
    end)

module Eager_just_positions =
  Parser.Make_eager
    (Kind.Positions)
    (struct
      type parsed_value = Positions.t

      let make_value state () = A.positions state
    end)

module Many_cst =
  Parser.Make
    (Kind.Cst)
    (struct
      type parsed_value = Cst.t_or_comment list

      let mode = A.Many
      let make_value _ stack = Automaton_stack.For_cst.get_many stack
    end)

module Eager_cst =
  Parser.Make_eager
    (Kind.Cst)
    (struct
      type parsed_value = Cst.t_or_comment

      let make_value _ stack =
        match Automaton_stack.For_cst.get_many stack with
        | [ sexp ] -> sexp
        | _ -> assert false
      ;;
    end)

type 'a id = 'a
type sexp_list = Sexp.t list

module Conv_single =
  Conv.Make
    (struct
      type 'a res = 'a
      type parsed_sexp = Sexp.t
      type chunk_to_conv = Sexp.t

      let apply_f x ~f = f x
      let find = Positions.find_sub_sexp_phys
    end)
    (Single)
    (Single_just_positions)

module Conv_many =
  Conv.Make
    (struct
      type 'a res = 'a list
      type parsed_sexp = Sexp.t list
      type chunk_to_conv = Sexp.t

      let apply_f x ~f = List.rev (List.rev_map x ~f)
      let find = Positions.find_sub_sexp_in_list_phys
    end)
    (Many)
    (Many_just_positions)

module Conv_many_at_once =
  Conv.Make
    (struct
      type 'a res = 'a
      type parsed_sexp = Sexp.t list
      type chunk_to_conv = Sexp.t list

      let apply_f x ~f = f x
      let find = Positions.find_sub_sexp_in_list_phys
    end)
    (Many)
    (Many_just_positions)

module Private = struct
  module Automaton_stack = Automaton_stack
  module Parser_automaton = Parser_automaton
end
