open! Core
include Composition_infix
include Expect_test_helpers_core
include Parsexp_prefix
module Atom_string = Sexp_string_quickcheck.Atom_string
module Automaton = Parsexp.Private.Automaton
module Positions = Parsexp.Private.Positions
module Sexp_string = Sexp_string_quickcheck.Sexp_string

let state_after_every_prefix mode s =
  let len = String.length s in
  let state, stack = Automaton.of_substring mode Sexp_with_positions s ~pos:0 ~len in
  ignore (Automaton.feed_eoi state stack : Automaton.Stack.t);
  List.init (len + 1) ~f:(fun len ->
    let state, stack = Automaton.of_substring mode Sexp_with_positions s ~pos:0 ~len in
    len, state, stack)
  @ [ len + 1, state, stack ]
;;
