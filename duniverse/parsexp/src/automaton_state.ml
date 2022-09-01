open! Import
include Automaton_state_intf

type ('u, 's) t = ('u, 's) State.t

open State

let initial_user_state : type u s. (u, s) Kind.t -> Positions.pos -> u =
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

(*$ Parsexp_cinaps_helpers.Gen_automaton_state.print_constants () *)
let initial_state = 0
let error_state = 1

(*$*)

let create ?(initial_pos = Positions.beginning_of_file) mode kind =
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

let reset_user_state : type u s. (u, s) t -> unit =
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

let context state : Context.t =
  match state.ignoring_stack with
  | _ :: _ -> Sexp_comment
  | [] -> Sexp
;;

let has_unclosed_paren state = state.depth > 0
let set_error_state state = state.automaton_state <- error_state
let automaton_state state = state.automaton_state
