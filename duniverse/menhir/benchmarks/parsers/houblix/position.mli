(** Extension of standard library's positions. *)

(** {2 Extended lexing positions} *)

type t
(** Abstract type for pairs of positions in the lexing stream. *)

type position = t

type 'a located = { value : 'a; position : t } [@@deriving sexp]
(** Decoration of a value with a position. *)

val value : 'a located -> 'a
(** [value dv] returns the raw value that underlies the
    decorated value [dv]. *)

val position : 'a located -> t
(** [position dv] returns the position that decorates the
    decorated value [dv]. *)

val destruct : 'a located -> 'a * t
(** [destruct dv] returns the couple of position and value
    of a decorated value [dv]. *)

val located : ('a -> 'b) -> 'a located -> 'b
(** [located f x] applies [f] to the value of [x]. *)

val with_val : 'b -> 'a located -> 'b located

val with_pos : t -> 'a -> 'a located
(** [with_pos p v] decorates [v] with a position [p]. *)

val with_cpos : Lexing.lexbuf -> 'a -> 'a located
(** [with_cpos p v] decorates [v] with a lexical position [p]. *)

val with_poss : Lexing.position -> Lexing.position -> 'a -> 'a located
(** [with_poss start stop v] decorates [v] with a position [(start, stop)]. *)

val unknown_pos : 'a -> 'a located
(** [unknown_pos x] decorates [v] with an unknown position. *)

val dummy : t
(** This value is used when an object does not come from a particular
    input location. *)

val map : ('a -> 'b) -> 'a located -> 'b located
(** [map f v] extends the decoration from [v] to [f v]. *)

val iter : ('a -> unit) -> 'a located -> unit
(** [iter f dv] applies [f] to the value inside [dv]. *)

val mapd : ('a -> 'b1 * 'b2) -> 'a located -> 'b1 located * 'b2 located
(** [mapd f v] extends the decoration from [v] to both members of the pair
    [f v]. *)

(** {2 Accessors} *)

val column : Lexing.position -> int
(** [column p] returns the number of characters from the
    beginning of the line of the Lexing.position [p]. *)

val line : Lexing.position -> int
(** [column p] returns the line number of to the Lexing.position [p]. *)

val characters : Lexing.position -> Lexing.position -> int * int
(** [characters p1 p2] returns the character interval
    between [p1] and [p2] assuming they are located in the same
    line. *)

val start_of_position : t -> Lexing.position
(** [start_of_position p] returns the beginning of a position [p]. *)

val end_of_position : t -> Lexing.position
(** [end_of_position p] returns the end of a position [p]. *)

val filename_of_position : t -> string
(** [filename_of_position p] returns the filename of a position [p]. *)

(** {2 Position handling} *)

val join : t -> t -> t
(** [join p1 p2] returns a position that starts where [p1]
    starts and stops where [p2] stops. *)

val lex_join : Lexing.position -> Lexing.position -> t
(** [lex_join l1 l2] returns a position that starts at [l1] and stops
    at [l2]. *)

val string_of_lex_pos : Lexing.position -> string
(** [string_of_lex_pos p] returns a string representation for
    the lexing position [p]. *)

val string_of_pos : t -> string
(** [string_of_pos p] returns the standard (Emacs-like) representation
    of the position [p]. *)

val pos_or_undef : t option -> t
(** [pos_or_undef po] is the identity function except if po = None,
    in that case, it returns [undefined_position]. *)

(** {2 Interaction with the lexer runtime} *)

val cpos : Lexing.lexbuf -> t
(** [cpos lexbuf] returns the current position of the lexer. *)

val string_of_cpos : Lexing.lexbuf -> string
(** [string_of_cpos p] returns a string representation of
    the lexer's current position. *)
