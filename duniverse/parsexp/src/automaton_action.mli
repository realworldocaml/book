(** Internal bits used by the generated automaton, not part of the public API *)

open! Import

val raise_error : _ Automaton_state.t -> at_eof:bool -> Parse_error.Private.Reason.t -> _

type context = Automaton_state.Context.t =
  | Sexp_comment
  | Sexp

val context : _ Automaton_state.t -> context
val set_automaton_state : ('u, 's) Automaton_state.t -> int -> unit

(** Advance the position counters. [advance_eol] is for when we read a newline
    character. *)
val advance : ('u, 's) Automaton_state.t -> unit

val advance_eol : ('u, 's) Automaton_state.t -> unit

(** Number of opened #| *)
val block_comment_depth : ('u, 's) Automaton_state.t -> int

type ('u, 's) t = ('u, 's) Automaton_state.t -> char -> 's -> 's

module Poly : sig
  type nonrec t = { f : 'u 's. ('u, 's) t } [@@unboxed]
end

module Epsilon : sig
  type ('u, 's) t = ('u, 's) Automaton_state.t -> 's -> 's

  module Poly : sig
    type nonrec t = { f : 'u 's. ('u, 's) t } [@@unboxed]
  end
end

(** Add a character to the atom buffer. [add_quoted_atom_char] does the same for quoted
    atoms *)
val add_atom_char : _ t

val add_quoted_atom_char : _ t

(** Add a character that just follows a '\\' and the '\\' itself if necessary. *)
val add_escaped : _ t

(** [escaped_value <- escaped_value * 10 + (char - '0')]

    These functions make the assumption that [char] is between '0' and '9'.
    [add_dec_escape_char] also assumes the result doesn't overflow. The automaton
    definition must make sure this is the case.

    [add_last_dec_escape_char] also adds the resulting character to the atom buffer.
*)
val add_dec_escape_char : _ t

val add_last_dec_escape_char : _ t

(** Same but for quoted strings inside comments. Useful because it can fail. *)
val comment_add_last_dec_escape_char : _ t

(** Same as [add_dec_escape_char] but for hexadicemal escape sequences *)
val add_hex_escape_char : _ t

val add_last_hex_escape_char : _ t

(** Ignore one more full sexp to come *)
val start_sexp_comment : _ t

(** Add the first char of an unquoted atom. *)
val add_first_char : _ t

val start_quoted_string : _ t

(** Takes note of a control character in quoted atoms or the uninterpreted characters of
    comments, for which there is no corresponding [add_*] call (a backslash and the x in
    "\xff" or any character in a line comment).  This does not get called for the opening
    ([start_quoted_string]) or closing ([push_quoted_atom]) quotes themselves.
*)
val add_token_char : _ t

val opening : _ t
val closing : _ t
val push_quoted_atom : _ t
val start_block_comment : _ t
val end_block_comment : _ t
val start_line_comment : _ t
val end_line_comment : _ Epsilon.t
val eps_push_atom : _ Epsilon.t
val eps_add_first_char_hash : _ Epsilon.t
val eps_eoi_check : _ Epsilon.t
val eps_add_escaped_cr : _ Epsilon.t
