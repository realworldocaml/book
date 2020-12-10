type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
[@@deriving sexp]

(** Datatypes of various thresholds *)
type atom_threshold = Atom_threshold of int [@@deriving sexp]

type char_threshold = Character_threshold of int [@@deriving sexp]

(** Depth is the depth of an atom. For example, in (a (b (c) d)), the depth of a is 1, the
    depth of b and d is 2, and depth of c is 3.
    Depth_threshold usually refers to the maximum depth of any atom in a list for it to be
    considered for certain heuristic, e.g. data alignment.
*)
type depth_threshold = Depth_threshold of int [@@deriving sexp]

(** Whether or not should closing parentheses be aligned. *)
type aligned_parens = Parens_alignment of bool [@@deriving sexp]

type data_alignment =
  | Data_not_aligned
  | Data_aligned of aligned_parens * atom_threshold * char_threshold * depth_threshold
  (** Character threshold is excluding spaces and parentheses, the maximum depth can't exceed
      the depth threshold.
  *)
[@@deriving sexp]

type atom_coloring =
  | Color_first of int
  (** Color the first one, only if the number of atoms that follow it at most the value of
      the constructor's argument.
  *)
  | Color_all
  | Color_none
[@@deriving sexp]

(** This currently relates only to block comments. [Auto_indent] tries to infer the
    indentation from the original formatting, [Indent_comment n] indents new lines in a
    block comment by n spaces.
*)
type comment_indent =
  | Auto_indent_comment
  | Indent_comment of int
[@@deriving sexp]

type comment_print_style =
  | Pretty_print (** Auto aligns multi-line block comments. *)
  | Conservative_print
  (** Leaves block comments as they are, only adjusts indentation. *)
[@@deriving sexp]

(** Comment treatment. *)
type comments =
  | Drop
  | Print of comment_indent * color option * comment_print_style
[@@deriving sexp]

type atom_printing =
  | Escaped (** Can be parsed again. Atoms are printed out as loaded, with escaping. *)
  | Minimal_escaping (** As [Escaped], but applies escaping to fewer characters. *)
  | Interpreted (** Try to interpret atoms as sexps. *)
[@@deriving sexp]

(** Singleton_lists are lists of the following format

    (ATOM_1 .. ATOM_N (....))

    and are printed in the following way if they are too big to fit on a line/force a
    breakline for other reasons:

    (ATOM_1 .. ATOM_N (
    ....
    ))

    Thresholds correspond to what's an acceptable number/size of the leading atoms ATOM_1
    through ATOM_N.

    Character threshold is excluding spaces.

*)
type singleton_limit = Singleton_limit of atom_threshold * char_threshold
[@@deriving sexp]

(** Should parentheses be colored? *)
type paren_coloring = bool [@@deriving sexp]

(** Separator between individual sexps. *)
type separator =
  | No_separator
  | Empty_line
[@@deriving sexp]

(** Should closing parentheses be on the same line as the last sexp in the list (modulo
    comments), or should they be on new lines?
    Should opening parentheses always be on the same line as what follows them, or should
    they sometimes (when the first item in the list is a list followed by some other sexp)
    be on a separate line?
*)
type parens =
  | Same_line
  | New_line
[@@deriving sexp]

(** Where to put line comments relative to an associated sexp. *)
type sticky_comments =
  | Before
  | Same_line
  | After
[@@deriving sexp]

type t =
  { indent : int
  ; data_alignment : data_alignment
  ; color_scheme : color array
  ; atom_coloring : atom_coloring
  ; atom_printing : atom_printing
  ; paren_coloring : paren_coloring
  ; opening_parens : parens
  ; closing_parens : parens
  ; comments : comments
  ; singleton_limit : singleton_limit
  ; leading_threshold : atom_threshold * char_threshold
  ; separator : separator
  ; sticky_comments : sticky_comments
  }
[@@deriving sexp]

val default : t

val create
  :  ?color:bool
  -> ?interpret_atom_as_sexp:bool
  -> ?drop_comments:bool
  -> ?new_line_separator:bool
  -> ?custom_data_alignment:data_alignment
  -> unit
  -> t

val update
  :  ?color:bool
  -> ?interpret_atom_as_sexp:bool
  -> ?drop_comments:bool
  -> ?new_line_separator:bool
  -> ?custom_data_alignment:data_alignment
  -> t
  -> t
