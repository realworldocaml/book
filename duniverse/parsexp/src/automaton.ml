open! Import
module Stack = Automaton_stack
include Automaton_state

let feed (type u s) (state : (u, s) Automaton_state.t) char (stack : s) : s =
  let idx = (automaton_state state lsl 8) lor Char.code char in
  Automaton_tables.transitions.(idx).f state char stack
[@@inline always]
;;

let feed_eoi (type u s) (state : (u, s) Automaton_state.t) (stack : s) : s =
  let stack = Automaton_tables.transitions_eoi.(automaton_state state).f state stack in
  set_error_state state;
  stack
;;

let old_parser_cont_state state : Old_parser_cont_state.t =
  match context state with
  | Sexp_comment -> Parsing_sexp_comment
  | Sexp ->
    (match
       ( Automaton_tables.old_parser_approx_cont_states.(automaton_state state)
       , has_unclosed_paren state )
     with
     | Parsing_toplevel_whitespace, true -> Parsing_list
     | s, _ -> s)
;;

let rec feed_substring_unsafe str state stack i stop =
  if i < stop
  then (
    let c = String.unsafe_get str i in
    let stack = feed state c stack in
    feed_substring_unsafe str state stack (i + 1) stop)
  else stack
;;

let rec feed_subbytes_unsafe str state stack i stop =
  if i < stop
  then (
    let c = Bytes.unsafe_get str i in
    let stack = feed state c stack in
    feed_subbytes_unsafe str state stack (i + 1) stop)
  else stack
;;

let feed_substring state str ~pos ~len stack =
  let str_len = String.length str in
  if pos < 0 || len < 0 || pos > str_len - len then invalid_arg "Parsexp.feed_substring";
  feed_substring_unsafe str state stack pos (pos + len)
;;

let feed_subbytes state str ~pos ~len stack =
  let str_len = Bytes.length str in
  if pos < 0 || len < 0 || pos > str_len - len then invalid_arg "Parsexp.feed_subbytes";
  feed_subbytes_unsafe str state stack pos (pos + len)
;;

let feed_string state str stack =
  feed_substring_unsafe str state stack 0 (String.length str)
;;

let feed_bytes state str stack = feed_subbytes_unsafe str state stack 0 (Bytes.length str)

let empty_stack : type u s. (u, s) Kind.t -> s = function
  | Sexp -> Stack.empty
  | Sexp_with_positions -> Stack.empty
  | Positions -> Stack.Just_positions.empty
  | Cst -> Stack.For_cst.empty
;;

let of_substring (type u s) (mode : (u, s) Mode.t) (kind : (u, s) Kind.t) s ~pos ~len =
  let state = create mode kind in
  let stack = feed_substring state s ~pos ~len (empty_stack kind) in
  state, stack
;;
