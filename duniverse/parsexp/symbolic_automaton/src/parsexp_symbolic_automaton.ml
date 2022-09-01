(** Abstract version of the parsing automaton.

    It is used in two places:

    - to define the automaton and generate parser code.
    - for tests

    At parser runtime, we instead use an integer for states and a table of
    functions for transitions
*)

(** It is possible that a modern and good parser generator can simplify the parser
    definition further.

    We didn't use an existing parser generator like ocamlyacc or menhir because we wanted
    to have a very precise control over performance and a good API for incremental
    parsing.

    At the time [Parsexp] was written, sexp parsers written using parser generators ran
    more slowly, allocated more, and were difficult to integrate with monadic IOs.

    [Sexplib] had both a lex/yacc generated parser and a handwritten one for performance.
    [Parsexp] had to match the performance of the handwritten parser (which we did, with
    less allocation).
*)

open! Base
module Automaton = Automaton
module Parse_error_reason = Parse_error_reason
module Table = Table

let table = Table.compile (module Automaton)
