(** Internal bits used by the generated automaton, not part of the public API *)

open! Import

(*_ Interface exposed in [Parser_automaton] *)
module Public : sig
  (** Internal state of the automaton *)
  type ('user_state, 'stack) state

  type ('u, 's) mode =
    | Single (** Parse a single s-expression *)
    | Many (** Parse a list of s-expressions *)
    | Eager of
        { got_sexp : ('u, 's) state -> 's -> 's
        (** Whether to consider no s-expression in the input as an error or not.

            The mutability is used in [Parsexp.Eager*.Lexbuf_consumer].
        *)
        ; mutable no_sexp_is_error : bool
        } (** Gives back s-expressions as soon as they are found. *)

  type state_cst

  type ('u, 's) kind =
    (*_ [Positions] case needs no stack because the [state] type keeps track of [depth]. *)
    | Positions : (Positions.Builder.t, unit) kind
    | Sexp : (unit, Automaton_stack.t) kind
    | Sexp_with_positions : (Positions.Builder.t, Automaton_stack.t) kind
    | Cst : (state_cst, Automaton_stack.For_cst.t) kind

  val new_state
    :  ?initial_pos:Positions.pos
    -> ('u, 's) mode
    -> ('u, 's) kind
    -> ('u, 's) state

  val reset : ?pos:Positions.pos -> _ state -> unit
  val positions : (Positions.Builder.t, _) state -> Positions.t
  val mode : ('u, 's) state -> ('u, 's) mode

  (** Number of characters fed to the parser *)
  val offset : _ state -> int

  (** Position in the text *)
  val line : _ state -> int

  val column : _ state -> int

  (** Whether there are some unclosed parentheses *)
  val has_unclosed_paren : ('u, 's) state -> bool

  val set_error_state : _ state -> unit

  (**/**)

  (*_ Only for converting errors to the old parser errors *)
  val atom_buffer : _ state -> Buffer.t

  (*_ For coverate tests *)
  val automaton_state : ('u, 's) state -> int
end

open Public

val raise_error : _ state -> at_eof:bool -> Parse_error.Private.Reason.t -> _

type context =
  | Sexp_comment
  | Sexp

val context : _ state -> context
val set_automaton_state : ('u, 's) state -> int -> unit

(** Advance the position counters. [advance_eol] is for when we read a newline
    character. *)
val advance : ('u, 's) state -> unit

val advance_eol : ('u, 's) state -> unit

(** Number of opened #| *)
val block_comment_depth : ('u, 's) state -> int

type ('u, 's) action = ('u, 's) state -> char -> 's -> 's
type ('u, 's) epsilon_action = ('u, 's) state -> 's -> 's

(** Add a character to the atom buffer. [add_quoted_atom_char] does the same for quoted
    atoms *)
val add_atom_char : _ action

val add_quoted_atom_char : _ action

(** Add a character that just follows a '\\' and the '\\' itself if necessary. *)
val add_escaped : _ action

(** [escaped_value <- escaped_value * 10 + (char - '0')]

    These functions make the assumption that [char] is between '0' and '9'.
    [add_dec_escape_char] also assumes the result doesn't overflow. The automaton
    definition must make sure this is the case.

    [add_last_dec_escape_char] also adds the resulting character to the atom buffer.
*)
val add_dec_escape_char : _ action

val add_last_dec_escape_char : _ action

(** Same but for quoted strings inside comments. Useful because it can fail. *)
val comment_add_last_dec_escape_char : _ action

(** Same as [add_dec_escape_char] but for hexadicemal escape sequences *)
val add_hex_escape_char : _ action

val add_last_hex_escape_char : _ action

(** Ignore one more full sexp to come *)
val start_sexp_comment : _ action

(** Add the first char of an unquoted atom. *)
val add_first_char : _ action

val start_quoted_string : _ action

(** Takes note of a control character in quoted atoms or the uninterpreted characters of
    comments, for which there is no corresponding [add_*] call (a backslash and the x in
    "\xff" or any character in a line comment).  This does not get called for the opening
    ([start_quoted_string]) or closing ([push_quoted_atom]) quotes themselves.
*)
val add_token_char : _ action

val opening : _ action
val closing : _ action
val push_quoted_atom : _ action
val start_block_comment : _ action
val end_block_comment : _ action
val start_line_comment : _ action
val end_line_comment : _ epsilon_action
val eps_push_atom : _ epsilon_action
val eps_add_first_char_hash : _ epsilon_action
val eps_eoi_check : _ epsilon_action
val eps_add_escaped_cr : _ epsilon_action
