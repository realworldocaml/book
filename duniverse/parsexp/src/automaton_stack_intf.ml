open! Import

(** Automaton stack represents a prefix of a sexp (or of a sexp sequence) while it's being
    parsed.  We have three different types of the stack depending on how much detail about
    the sexp structure is being recorded (just the sexp, sexp with comments and positions
    (CST), or just positions. *)

module type Automaton_stack = sig
  module For_cst : sig
    type t =
      | Empty (** at top-level *)
      | T_or_comment of Cst.t_or_comment * t (** after the given sexp or comment *)
      | Open of Positions.pos * t (** after the opening paren *)
      | In_sexp_comment of
          { hash_semi_pos : Positions.pos
          ; rev_comments : Cst.comment list
          ; stack : t
          }
      (** [In_sexp_comment] only indicates if the next s-expression is to be commented
          out, but if we are nested below parens below an sexp comment, the stack would
          look like [Open (.., In_sexp_comment ..)]. *)

    val empty : t

    (** Raises if [t] contains a partial sexp. *)
    val get_many : t -> Cst.t_or_comment list
  end

  (** The recorded positions are stored elsewhere *)
  module Just_positions : sig
    type t = unit

    val empty : t
  end

  type t =
    | Empty
    | Open of t
    | Sexp of Sexp.t * t

  val empty : t

  (** Raises if [t] is not exactly one complete sexp. *)
  val get_single : t -> Sexp.t

  (** Raises if [t] contains a partial sexp. *)
  val get_many : t -> Sexp.t list
end
