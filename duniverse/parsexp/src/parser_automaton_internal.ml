open! Import

module Public = struct
  type state_cst =
    { token_buffer : Buffer.t
    ; (* Starting positions of the current token *)
      mutable token_start_pos : Positions.pos
    }

  type ('u, 's) kind =
    | Positions : (Positions.Builder.t, unit) kind
    | Sexp : (unit, Automaton_stack.t) kind
    | Sexp_with_positions : (Positions.Builder.t, Automaton_stack.t) kind
    | Cst : (state_cst, Automaton_stack.For_cst.t) kind

  type ('u, 's) state =
    { mutable automaton_state : int
    ; kind : ('u, 's) kind
    ; mutable depth : int
    ; (* Number of opened #| when parsing a block comment *)
      mutable block_comment_depth : int
    ; (* Stack of ignoring depths; the current depth is pushed
         each time a #; comment is entered. *)
      mutable ignoring_stack : int list
    ; (* When parsing an escape sequence of the form "\\NNN" or "\\XX", this accumulates
         the computed number *)
      mutable escaped_value : int
    ; (* Buffer for accumulating atoms *)
      atom_buffer : Buffer.t
    ; user_state : 'u
    ; mode : ('u, 's) mode
    ; mutable full_sexps : int
    ; mutable offset : int (* global offset *)
    ; mutable line_number : int
    ; mutable bol_offset : int (* offset of beginning of line *)
    }

  and ('u, 's) mode =
    | Single
    | Many
    | Eager of
        { got_sexp : ('u, 's) state -> 's -> 's
        ; mutable no_sexp_is_error : bool
        }

  let initial_user_state : type u s. (u, s) kind -> Positions.pos -> u =
    fun kind initial_pos ->
    match kind with
    | Positions -> Positions.Builder.create ~initial_pos ()
    | Sexp -> ()
    | Sexp_with_positions -> Positions.Builder.create ~initial_pos ()
    | Cst ->
      (* [token_start_pos] is set to a dummy location here. It is properly set when we
         start to capture a token from the input *)
      { token_buffer = Buffer.create 128; token_start_pos = Positions.beginning_of_file }
  ;;

  (* these magic numbers are checked in gen_parser_automaton.ml:
     let () = assert (initial = 0)
     let () = assert (to_int Error = 1) *)
  let initial_state = 0
  let error_state = 1

  let new_state ?(initial_pos = Positions.beginning_of_file) mode kind =
    { kind
    ; depth = 0
    ; automaton_state = initial_state
    ; block_comment_depth = 0
    ; ignoring_stack = []
    ; escaped_value = 0
    ; atom_buffer = Buffer.create 128
    ; user_state = initial_user_state kind initial_pos
    ; mode
    ; full_sexps = 0
    ; offset = initial_pos.offset
    ; line_number = initial_pos.line
    ; bol_offset = initial_pos.offset - initial_pos.col
    }
  ;;

  let mode t = t.mode
  let positions t = Positions.Builder.contents t.user_state
  let atom_buffer t = t.atom_buffer
  let offset state = state.offset
  let line state = state.line_number
  let column state = state.offset - state.bol_offset
  let position t = { Positions.col = column t; line = line t; offset = offset t }

  let reset_user_state : type u s. (u, s) state -> unit =
    fun t ->
    match t.kind with
    | Positions -> Positions.Builder.reset t.user_state (position t)
    | Sexp -> ()
    | Sexp_with_positions -> Positions.Builder.reset t.user_state (position t)
    | Cst -> Buffer.clear t.user_state.token_buffer
  ;;

  let reset ?(pos = Positions.beginning_of_file) t =
    t.depth <- 0;
    t.automaton_state <- initial_state;
    t.block_comment_depth <- 0;
    t.ignoring_stack <- [];
    t.escaped_value <- 0;
    t.full_sexps <- 0;
    t.offset <- pos.offset;
    t.line_number <- pos.line;
    t.bol_offset <- pos.offset - pos.col;
    reset_user_state t;
    Buffer.clear t.atom_buffer
  ;;

  type context =
    | Sexp_comment
    | Sexp

  let is_ignoring state =
    match state.ignoring_stack with
    | _ :: _ -> true
    | [] -> false
  ;;

  let is_not_ignoring state = not (is_ignoring state)
  let context state = if is_not_ignoring state then Sexp else Sexp_comment
  let has_unclosed_paren state = state.depth > 0
  let set_error_state state = state.automaton_state <- error_state

  module Error = Parse_error

  let automaton_state state = state.automaton_state
end

open Public

let raise_error : type a b. (a, b) state -> _ =
  fun state ~at_eof reason ->
  set_error_state state;
  Parse_error.Private.raise
    reason
    { line = state.line_number
    ; col = state.offset - state.bol_offset
    ; offset = state.offset
    }
    ~at_eof
    ~atom_buffer:state.atom_buffer
;;

type nonrec context = context =
  | Sexp_comment
  | Sexp

let context = context

type ('u, 's) action = ('u, 's) state -> char -> 's -> 's
type ('u, 's) epsilon_action = ('u, 's) state -> 's -> 's

let current_pos ?(delta = 0) state : Positions.pos =
  let offset = state.offset + delta in
  { line = state.line_number; col = offset - state.bol_offset; offset }
;;

let set_automaton_state state x = state.automaton_state <- x
let advance state = state.offset <- state.offset + 1

let advance_eol : type u s. (u, s) state -> unit =
  fun state ->
  let newline_offset = state.offset in
  state.offset <- newline_offset + 1;
  state.bol_offset <- state.offset;
  state.line_number <- state.line_number + 1;
  match state.kind with
  | Positions -> Positions.Builder.add_newline state.user_state ~offset:newline_offset
  | Sexp_with_positions ->
    Positions.Builder.add_newline state.user_state ~offset:newline_offset
  | _ -> ()
;;

let block_comment_depth state = state.block_comment_depth

let add_token_char : type u s. (u, s) action =
  fun state char stack ->
  match state.kind with
  | Cst ->
    Buffer.add_char state.user_state.token_buffer char;
    stack
  | _ -> stack
;;

let add_atom_char state c stack =
  Buffer.add_char state.atom_buffer c;
  stack
;;

let add_quoted_atom_char state c stack =
  Buffer.add_char state.atom_buffer c;
  add_token_char state c stack
;;

let check_new_sexp_allowed state =
  let is_single =
    match state.mode with
    | Single -> true
    | _ -> false
  in
  if is_single && state.full_sexps > 0 && is_not_ignoring state
  then raise_error state ~at_eof:false Too_many_sexps
;;

let add_pos state ~delta =
  Positions.Builder.add state.user_state ~offset:(state.offset + delta)
;;

let add_first_char : type u s. (u, s) action =
  fun state char stack ->
  check_new_sexp_allowed state;
  Buffer.add_char state.atom_buffer char;
  (* For non-quoted atoms, we save both positions at the end. We can always determine the
     start position from the end position and the atom length for non-quoted atoms.

     Doing it this way allows us to detect single characater atoms for which we need to
     save the position twice. *)
  stack
;;

let eps_add_first_char_hash : type u s. (u, s) epsilon_action =
  fun state stack ->
  check_new_sexp_allowed state;
  Buffer.add_char state.atom_buffer '#';
  stack
;;

let start_quoted_string : type u s. (u, s) action =
  fun state _char stack ->
  check_new_sexp_allowed state;
  match state.kind with
  | Positions ->
    if is_not_ignoring state then add_pos state ~delta:0;
    stack
  | Sexp_with_positions ->
    if is_not_ignoring state then add_pos state ~delta:0;
    stack
  | Cst ->
    state.user_state.token_start_pos <- current_pos state;
    Buffer.add_char state.user_state.token_buffer '"';
    stack
  | Sexp -> stack
;;

let add_escaped state c stack =
  let c' =
    match c with
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 'b' -> '\b'
    | 't' -> '\t'
    | '\\' | '\'' | '"' -> c
    | _ ->
      Buffer.add_char state.atom_buffer '\\';
      c
  in
  Buffer.add_char state.atom_buffer c';
  add_token_char state c stack
;;

let eps_add_escaped_cr state stack =
  Buffer.add_char state.atom_buffer '\r';
  stack
;;

let dec_val c = Char.code c - Char.code '0'

let hex_val c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | _ -> Char.code c - Char.code 'A' + 10
;;

let add_dec_escape_char state c stack =
  state.escaped_value <- (state.escaped_value * 10) + dec_val c;
  add_token_char state c stack
;;

let add_last_dec_escape_char state c stack =
  let value = (state.escaped_value * 10) + dec_val c in
  state.escaped_value <- 0;
  if value > 255 then raise_error state ~at_eof:false Escape_sequence_out_of_range;
  Buffer.add_char state.atom_buffer (Char.chr value);
  add_token_char state c stack
;;

let comment_add_last_dec_escape_char state c stack =
  let value = (state.escaped_value * 10) + dec_val c in
  state.escaped_value <- 0;
  if value > 255 then raise_error state ~at_eof:false Escape_sequence_out_of_range;
  add_token_char state c stack
;;

let add_hex_escape_char state c stack =
  state.escaped_value <- (state.escaped_value lsl 4) lor hex_val c;
  add_token_char state c stack
;;

let add_last_hex_escape_char state c stack =
  let value = (state.escaped_value lsl 4) lor hex_val c in
  state.escaped_value <- 0;
  Buffer.add_char state.atom_buffer (Char.chr value);
  add_token_char state c stack
;;

let opening : type u s. (u, s) state -> char -> s -> s =
  fun state _char stack ->
  check_new_sexp_allowed state;
  state.depth <- state.depth + 1;
  match state.kind with
  | Positions ->
    if is_not_ignoring state then add_pos state ~delta:0;
    stack
  | Sexp -> if is_not_ignoring state then Open stack else stack
  | Sexp_with_positions ->
    if is_not_ignoring state
    then (
      add_pos state ~delta:0;
      Open stack)
    else stack
  | Cst -> Open (current_pos state, stack)
;;

let do_reset_positions state =
  Positions.Builder.reset
    state.user_state
    { line = state.line_number
    ; col = state.offset - state.bol_offset
    ; offset = state.offset
    }
;;

let reset_positions : type u s. (u, s) state -> unit =
  fun state ->
  match state.kind with
  | Positions -> do_reset_positions state
  | Sexp_with_positions -> do_reset_positions state
  | Sexp -> ()
  | Cst -> ()
;;

let toplevel_sexp_or_comment_added state stack ~delta =
  match state.mode with
  | Single | Many -> stack
  | Eager { got_sexp = f; _ } ->
    (* Modify the offset so that [f] get a state pointing to the end of the current
       s-expression *)
    let saved_offset = state.offset in
    state.offset <- state.offset + delta;
    let saved_full_sexps = state.full_sexps in
    (match f state stack with
     | exception e ->
       set_error_state state;
       raise e
     | stack ->
       (* This assert is not a full protection against the user mutating the state but
          it should catch most cases. *)
       assert (state.offset = saved_offset + delta && state.full_sexps = saved_full_sexps);
       state.offset <- saved_offset;
       reset_positions state;
       stack)
;;

let is_top_level state = is_not_ignoring state && state.depth = 0

let comment_added_assuming_cst state stack ~delta =
  if is_top_level state then toplevel_sexp_or_comment_added state stack ~delta else stack
;;

let maybe_pop_ignoring_stack state =
  match state.ignoring_stack with
  | inner_comment_depth :: _tl when inner_comment_depth > state.depth ->
    raise_error state ~at_eof:false Sexp_comment_without_sexp
  | inner_comment_depth :: tl when inner_comment_depth = state.depth ->
    state.ignoring_stack <- tl;
    true
  | _ -> false
;;

let sexp_added : type u s. (u, s) state -> s -> delta:int -> s =
  fun state stack ~delta ->
  let is_comment = maybe_pop_ignoring_stack state in
  if is_top_level state
  then (
    if not is_comment then state.full_sexps <- state.full_sexps + 1;
    if (not is_comment)
       ||
       match state.kind with
       | Cst -> true
       | _ -> false
    then toplevel_sexp_or_comment_added state stack ~delta
    else stack)
  else stack
;;

let rec make_list acc : Automaton_stack.t -> Automaton_stack.t = function
  | Empty -> assert false
  | Open stack -> Sexp (List acc, stack)
  | Sexp (sexp, stack) -> make_list (sexp :: acc) stack
;;

let add_comment_to_stack_cst comment (stack : Automaton_stack.For_cst.t)
  : Automaton_stack.For_cst.t
  =
  match stack with
  | In_sexp_comment r ->
    In_sexp_comment { r with rev_comments = comment :: r.rev_comments }
  | _ -> T_or_comment (Comment comment, stack)
;;

let add_sexp_to_stack_cst sexp : Automaton_stack.For_cst.t -> Automaton_stack.For_cst.t
  = function
    | In_sexp_comment { hash_semi_pos; rev_comments; stack } ->
      let comment : Cst.comment =
        Sexp_comment { hash_semi_pos; comments = List.rev rev_comments; sexp }
      in
      add_comment_to_stack_cst comment stack
    | stack -> T_or_comment (Sexp sexp, stack)
;;

let rec make_list_cst end_pos acc
  : Automaton_stack.For_cst.t -> Automaton_stack.For_cst.t
  = function
    | T_or_comment (t, stack) -> make_list_cst end_pos (t :: acc) stack
    | Open (start_pos, stack) ->
      let sexp : Cst.t = List { loc = { start_pos; end_pos }; elements = acc } in
      add_sexp_to_stack_cst sexp stack
    | Empty | In_sexp_comment _ -> assert false
;;

let closing : type u s. (u, s) state -> char -> s -> s =
  fun state _char stack ->
  if state.depth > 0
  then (
    let stack : s =
      match state.kind with
      | Positions ->
        (* Note we store end positions as inclusive in [Positions.t], so we use [delta:0],
           while in the [Cst] case we save directly the final ranges, so we use
           [delta:1]. *)
        if is_not_ignoring state then add_pos state ~delta:0;
        stack
      | Sexp -> if is_not_ignoring state then make_list [] stack else stack
      | Sexp_with_positions ->
        if is_not_ignoring state
        then (
          add_pos state ~delta:0;
          make_list [] stack)
        else stack
      | Cst -> make_list_cst (current_pos state ~delta:1) [] stack
    in
    state.depth <- state.depth - 1;
    sexp_added state stack ~delta:1)
  else raise_error state ~at_eof:false Closed_paren_without_opened
;;

let make_loc ?(delta = 0) state : Positions.range =
  { start_pos = state.user_state.token_start_pos; end_pos = current_pos state ~delta }
;;

(* This is always called on the position exactly following the last character of a
   non-quoted atom *)
let add_non_quoted_atom_pos state ~atom =
  let len = String.length atom in
  if len = 1
  then Positions.Builder.add_twice state.user_state ~offset:(state.offset - 1)
  else (
    add_pos state ~delta:(-len);
    add_pos state ~delta:(-1))
;;

let eps_push_atom : type u s. (u, s) epsilon_action =
  fun state stack ->
  let str = Buffer.contents state.atom_buffer in
  Buffer.clear state.atom_buffer;
  let stack : s =
    match state.kind with
    | Positions ->
      if is_not_ignoring state then add_non_quoted_atom_pos state ~atom:str;
      stack
    | Sexp -> if is_not_ignoring state then Sexp (Atom str, stack) else stack
    | Sexp_with_positions ->
      if is_not_ignoring state
      then (
        add_non_quoted_atom_pos state ~atom:str;
        Sexp (Atom str, stack))
      else stack
    | Cst ->
      let loc : Positions.range =
        { start_pos = current_pos state ~delta:(-String.length str)
        ; end_pos = current_pos state ~delta:0
        }
      in
      let sexp : Cst.t = Atom { loc; atom = str; unescaped = Some str } in
      add_sexp_to_stack_cst sexp stack
  in
  sexp_added state stack ~delta:0
;;

let push_quoted_atom : type u s. (u, s) action =
  fun state _char stack ->
  let str = Buffer.contents state.atom_buffer in
  Buffer.clear state.atom_buffer;
  let stack : s =
    match state.kind with
    | Positions ->
      if is_not_ignoring state then add_pos state ~delta:0;
      stack
    | Sexp -> if is_not_ignoring state then Sexp (Atom str, stack) else stack
    | Sexp_with_positions ->
      if is_not_ignoring state
      then (
        add_pos state ~delta:0;
        Sexp (Atom str, stack))
      else stack
    | Cst ->
      let buf = state.user_state.token_buffer in
      Buffer.add_char buf '"';
      let s = Buffer.contents buf in
      Buffer.clear buf;
      let sexp : Cst.t =
        Atom { loc = make_loc state ~delta:1; atom = str; unescaped = Some s }
      in
      add_sexp_to_stack_cst sexp stack
  in
  sexp_added state stack ~delta:1
;;

let start_sexp_comment : type u s. (u, s) action =
  fun state _char stack ->
  state.ignoring_stack <- state.depth :: state.ignoring_stack;
  match state.kind with
  | Cst ->
    In_sexp_comment
      { hash_semi_pos = current_pos state ~delta:(-1); rev_comments = []; stack }
  | _ -> stack
;;

let start_block_comment : type u s. (u, s) state -> char -> s -> s =
  fun state char stack ->
  state.block_comment_depth <- state.block_comment_depth + 1;
  match state.kind with
  | Positions -> stack
  | Sexp -> stack
  | Sexp_with_positions -> stack
  | Cst ->
    if state.block_comment_depth = 1
    then (
      state.user_state.token_start_pos <- current_pos state ~delta:(-1);
      Buffer.add_char state.user_state.token_buffer '#');
    Buffer.add_char state.user_state.token_buffer char;
    stack
;;

let end_block_comment : type u s. (u, s) state -> char -> s -> s =
  fun state char stack ->
  state.block_comment_depth <- state.block_comment_depth - 1;
  match state.kind with
  | Positions -> stack
  | Sexp -> stack
  | Sexp_with_positions -> stack
  | Cst ->
    let buf = state.user_state.token_buffer in
    Buffer.add_char buf char;
    if state.block_comment_depth = 0
    then (
      let s = Buffer.contents buf in
      Buffer.clear buf;
      let comment : Cst.comment =
        Plain_comment { loc = make_loc state ~delta:1; comment = s }
      in
      let stack = add_comment_to_stack_cst comment stack in
      comment_added_assuming_cst state stack ~delta:1)
    else stack
;;

let start_line_comment : type u s. (u, s) action =
  fun state char stack ->
  match state.kind with
  | Cst ->
    state.user_state.token_start_pos <- current_pos state;
    Buffer.add_char state.user_state.token_buffer char;
    stack
  | _ -> stack
;;

let end_line_comment : type u s. (u, s) epsilon_action =
  fun state stack ->
  match state.kind with
  | Positions -> stack
  | Sexp -> stack
  | Sexp_with_positions -> stack
  | Cst ->
    let buf = state.user_state.token_buffer in
    let s = Buffer.contents buf in
    Buffer.clear buf;
    let comment : Cst.comment = Plain_comment { loc = make_loc state; comment = s } in
    let stack = add_comment_to_stack_cst comment stack in
    comment_added_assuming_cst state stack ~delta:0
;;

let eps_eoi_check : type u s. (u, s) epsilon_action =
  fun state stack ->
  if state.depth > 0 then raise_error state ~at_eof:true Unclosed_paren;
  if is_ignoring state then raise_error state ~at_eof:true Sexp_comment_without_sexp;
  if state.full_sexps = 0
  then (
    match state.mode with
    | Many | Eager { no_sexp_is_error = false; _ } -> ()
    | Single | Eager { no_sexp_is_error = true; _ } ->
      raise_error state ~at_eof:true No_sexp_found_in_input);
  stack
;;
