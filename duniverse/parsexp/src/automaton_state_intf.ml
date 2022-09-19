open! Import

module Context = struct
  type t =
    | Sexp_comment
    | Sexp
end

module For_cst = struct
  type t =
    { token_buffer : Buffer.t (** Starting positions of the current token **)
    ; mutable token_start_pos : Positions.pos
    }
  [@@deriving sexp_of]
end

module Kind = struct
  type ('u, 's) t =
    | Positions : (Positions.Builder.t, unit) t
    | Sexp : (unit, Automaton_stack.t) t
    | Sexp_with_positions : (Positions.Builder.t, Automaton_stack.t) t
    | Cst : (For_cst.t, Automaton_stack.For_cst.t) t
end

module rec State : sig
  type ('u, 's) t =
    { mutable automaton_state : int
    ; kind : ('u, 's) Kind.t
    ; mutable depth : int (** Number of opened #| when parsing a block comment *)
    ; mutable block_comment_depth : int
    (** Stack of ignoring depths; the current depth is pushed
        each time a #; comment is entered. *)
    ; mutable ignoring_stack : int list
    (** When parsing an escape sequence of the form "\\NNN" or "\\XX", this accumulates
        the computed number *)
    ; mutable escaped_value : int (** Buffer for accumulating atoms *)
    ; atom_buffer : Buffer.t
    ; user_state : 'u
    ; mode : ('u, 's) Mode.t
    ; mutable full_sexps : int
    ; mutable offset : int (** global offset **)
    ; mutable line_number : int
    ; mutable bol_offset : int (** offset of beginning of line **)
    }
end =
  State

and Mode : sig
  type ('u, 's) t =
    | Single
    | Many
    | Eager of
        { got_sexp : ('u, 's) State.t -> 's -> 's
        ; mutable no_sexp_is_error : bool
        }
end =
  Mode

module type Automaton_state = sig
  module Context = Context
  module For_cst = For_cst
  module Kind = Kind
  module Mode = Mode
  module State = State

  type ('u, 's) t = ('u, 's) State.t

  val create
    :  ?initial_pos:Positions.pos
    -> ('u, 's) Mode.t
    -> ('u, 's) Kind.t
    -> ('u, 's) t

  val reset : ?pos:Positions.pos -> _ t -> unit
  val positions : (Positions.Builder.t, _) t -> Positions.t
  val mode : ('u, 's) t -> ('u, 's) Mode.t

  (** Number of characters fed to the parser *)
  val offset : _ t -> int

  (** Position in the text *)
  val line : _ t -> int

  val column : _ t -> int

  (** Whether there are some unclosed parentheses *)
  val has_unclosed_paren : ('u, 's) t -> bool

  val set_error_state : _ t -> unit

  (**/**)

  (*_ Only for converting errors to the old parser errors *)
  val atom_buffer : _ t -> Buffer.t

  (*_ For coverate tests *)
  val automaton_state : ('u, 's) t -> int
  val context : _ t -> Context.t
end
