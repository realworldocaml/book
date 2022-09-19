open! Import
include Parser_intf
module A = Automaton

let kind_to_stack
  : type stack.
    ('state, stack) Automaton_state.Kind.t -> (module Stack with type t = stack)
  =
  fun (type state) (kind : (state, stack) Automaton_state.Kind.t) ->
  match kind with
  | Sexp -> (module Automaton_stack : Stack with type t = stack)
  | Positions -> (module Automaton_stack.Just_positions : Stack with type t = stack)
  | Sexp_with_positions -> (module Automaton_stack : Stack with type t = stack)
  | Cst -> (module Automaton_stack.For_cst : Stack with type t = stack)
;;

let make (type stack state parsed_value) kind mode make_value
  : (module S with type parsed_value = parsed_value)
  =
  (module struct
    type nonrec parsed_value = parsed_value

    module Stack = (val kind_to_stack kind : Stack with type t = stack)

    module State = struct
      type t = (state, Stack.t) Automaton_state.t

      let create ?pos () = A.create ?initial_pos:pos mode kind
      let reset = A.reset
      let offset = A.offset
      let line = A.line
      let column = A.column

      let position t : Positions.pos =
        { offset = offset t; line = line t; col = column t }
      ;;

      let stop state = A.set_error_state state
    end

    let feed = A.feed
    let feed_eoi state stack = make_value state (A.feed_eoi state stack)
    let feed_substring = Automaton.feed_substring
    let feed_string = Automaton.feed_string
    let feed_subbytes = Automaton.feed_subbytes
    let feed_bytes = Automaton.feed_bytes

    let parse_string_exn str =
      let state = State.create () in
      feed_eoi state (feed_string state str Stack.empty)
    ;;

    let parse_string str =
      match parse_string_exn str with
      | x -> Ok x
      | exception Parse_error.Parse_error e -> Error e
    ;;
  end)
;;

let make_eager (type stack state parsed_value) kind make_value
  : (module S_eager with type parsed_value = parsed_value)
  =
  (module struct
    type nonrec parsed_value = parsed_value

    module Stack = (val kind_to_stack kind : Stack with type t = stack)

    module State = struct
      module Read_only = struct
        type t = (state, Stack.t) Automaton_state.t

        let offset = A.offset
        let line = A.line
        let column = A.column

        let position t : Positions.pos =
          { offset = offset t; line = line t; col = column t }
        ;;
      end

      include Read_only

      let create ?pos ?(no_sexp_is_error = false) f =
        let got_sexp state stack =
          let parsed_value = make_value state stack in
          f state parsed_value;
          Stack.empty
        in
        A.create ?initial_pos:pos (Eager { got_sexp; no_sexp_is_error }) kind
      ;;

      let reset = A.reset
      let stop t = A.set_error_state t
      let old_parser_cont_state t = Automaton.old_parser_cont_state t
    end

    let feed = A.feed
    let feed_eoi state stack = ignore (A.feed_eoi state stack : Stack.t)
    let feed_substring = Automaton.feed_substring
    let feed_string = Automaton.feed_string
    let feed_subbytes = Automaton.feed_subbytes
    let feed_bytes = Automaton.feed_bytes

    module Lexbuf_consumer = struct
      type t = State.t

      exception Got_sexp of parsed_value * Positions.pos

      let got_sexp state parsed_value =
        raise_notrace (Got_sexp (parsed_value, State.position state))
      ;;

      let create () = State.create got_sexp

      let pos_of_lexbuf lexbuf =
        let p = lexbuf.Lexing.lex_curr_p in
        { Positions.line = p.pos_lnum; col = p.pos_cnum - p.pos_bol; offset = p.pos_cnum }
      ;;

      let update_lexbuf (lexbuf : Lexing.lexbuf) (pos : Positions.pos) =
        let p = pos.offset - lexbuf.lex_abs_pos in
        lexbuf.lex_curr_pos <- p;
        lexbuf.lex_start_pos <- p;
        lexbuf.lex_curr_p
        <- { lexbuf.lex_curr_p with
             pos_lnum = pos.line
           ; pos_cnum = pos.offset
           ; pos_bol = pos.offset - pos.col
           }
      ;;

      let rec feed_lexbuf t (lexbuf : Lexing.lexbuf) stack =
        let stack =
          feed_subbytes
            t
            lexbuf.lex_buffer
            stack
            ~pos:lexbuf.lex_curr_pos
            ~len:(lexbuf.lex_buffer_len - lexbuf.lex_curr_pos)
        in
        lexbuf.lex_curr_pos <- lexbuf.lex_buffer_len;
        lexbuf.lex_start_pos <- lexbuf.lex_buffer_len;
        if not lexbuf.lex_eof_reached
        then (
          lexbuf.refill_buff lexbuf;
          feed_lexbuf t lexbuf stack)
        else feed_eoi t stack
      ;;

      let parse_gen t (lexbuf : Lexing.lexbuf) =
        A.reset t ~pos:(pos_of_lexbuf lexbuf);
        match feed_lexbuf t lexbuf Stack.empty with
        | () ->
          update_lexbuf lexbuf (State.position t);
          None
        | exception Got_sexp (parsed_value, pos) ->
          update_lexbuf lexbuf pos;
          Some parsed_value
        | exception exn ->
          update_lexbuf lexbuf (State.position t);
          raise exn
      ;;

      let set_no_sexp_is_error t x =
        match A.mode t with
        | Eager e -> e.no_sexp_is_error <- x
        | _ -> assert false
      ;;

      let parse t lexbuf =
        set_no_sexp_is_error t true;
        match parse_gen t lexbuf with
        | Some x -> x
        | None -> failwith "Parsexp.parse_gen: None"
      ;;

      let parse_opt t lexbuf =
        set_no_sexp_is_error t false;
        parse_gen t lexbuf
      ;;
    end
  end)
;;
