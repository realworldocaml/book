open Base

let of_sexp_error = Sexplib.Conv.of_sexp_error


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

(* Datatypes of various thresholds *)
type atom_threshold = Atom_threshold of int [@@deriving sexp]

let atom_threshold_of_sexp sexp =
  match atom_threshold_of_sexp sexp with
  | Atom_threshold n when n < 0 ->
    of_sexp_error "Atom threshold must be non_negative." sexp
  | threshold -> threshold
;;

type char_threshold = Character_threshold of int [@@deriving sexp]

let char_threshold_of_sexp sexp =
  match char_threshold_of_sexp sexp with
  | Character_threshold n when n < 0 ->
    of_sexp_error "Character threshold must be non_negative." sexp
  | threshold -> threshold
;;

(* Depth is the depth of an atom. For example, in (a (b (c) d)), the depth of a is 1, the
   depth of b and d is 2, and depth of c is 3.
   Depth_threshold usually refers to the maximum depth of any atom in a list for it to be
   considered for certain heuristic, e.g. data alignment.
*)
type depth_threshold = Depth_threshold of int [@@deriving sexp]

let depth_threshold_of_sexp sexp =
  match depth_threshold_of_sexp sexp with
  | Depth_threshold n when n < 1 ->
    of_sexp_error "Depth threshold must be greater than 0." sexp
  | threshold -> threshold
;;

(* Whether or not should closing parentheses be aligned. *)
type aligned_parens = Parens_alignment of bool [@@deriving sexp]

type data_alignment =
  | Data_not_aligned
  (* Character threshold is excluding spaces and parentheses, the maximum depth can't exceed
     the depth threshold.
  *)
  | Data_aligned of aligned_parens * atom_threshold * char_threshold * depth_threshold
[@@deriving sexp]

type atom_coloring =
  (* Color the first one, only if the number of atoms that follow it at most the value of
     the constructor's argument.
  *)
  | Color_first of int
  | Color_all
  | Color_none
[@@deriving sexp]

let atom_coloring_of_sexp sexp =
  match atom_coloring_of_sexp sexp with
  | Color_first n when n < 0 ->
    of_sexp_error "The limit to color atoms must be non-negative." sexp
  | coloring -> coloring
;;

(* This currently relates only to block comments. [Auto_indent] tries to infer the
   indentation from the original formatting, [Indent_comment n] indents new lines in a
   block comment by n spaces.
*)
type comment_indent =
  | Auto_indent_comment
  | Indent_comment of int
[@@deriving sexp]

let comment_indent_of_sexp sexp =
  match comment_indent_of_sexp sexp with
  | Indent_comment n when n < 0 -> of_sexp_error "Indentation must be non-negative." sexp
  | indent -> indent
;;

type comment_print_style =
  (* Auto aligns multi-line block comments. *)
  | Pretty_print
  (* Leaves block comments as they are, only adjusts indentation. *)
  | Conservative_print
[@@deriving sexp]

(* Comment treatment. *)
type comments =
  | Drop
  | Print of comment_indent * color option * comment_print_style
[@@deriving sexp]

type atom_printing =
  | Escaped (* Can be parsed again. Atoms are printed out as loaded, with escaping. *)
  | Minimal_escaping (* As [Escaped], but applies escaping to fewer characters. *)
  | Interpreted (* Try to interpret atoms as sexps. *)
[@@deriving sexp]

(* Singleton_lists are lists of the following format

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

(* Should parentheses be colored? *)
type paren_coloring = bool [@@deriving sexp]

(* Separator between individual sexps. *)
type separator =
  | No_separator
  | Empty_line
[@@deriving sexp]

(* Should closing parentheses be on the same line as the last sexp in the list (modulo
   comments), or should they be on new lines?
   Should opening parentheses always be on the same line as what follows them, or should
   they sometimes (when the first item in the list is a list followed by some other sexp)
   be on a separate line?
*)
type parens =
  | Same_line
  | New_line
[@@deriving sexp]

(* Where to put the line comments corresponding to some sexp?
   For example, if the original input is

   {v
      SEXP ;comment1
           ;comment2
   v}

   If [Before], put comments in lines before the sexp:

   {v
      ;comment1
      ;comment2
      SEXP
   v}

   If [Same_line], put the first comment right after the sexp, on the same line,
   and align the rest of the comments:

   {v
      SEXP ;comment1
           ;comment2
   v}

   If [After], put comments in lines after the sexp:

   {v
      SEXP
      ;comment1
      ;comment2
   v}
*)
type sticky_comments =
  | Before
  | Same_line
  | After
[@@deriving sexp]

type t =
  { (* The size of indentation in number of spaces. *)
    indent : int [@default 2]
  ; (* Alignment of sexp list into columns. *)
    data_alignment : data_alignment
                     [@default
                       Data_aligned
                         ( Parens_alignment false
                         , Atom_threshold 6
                         , Character_threshold 60
                         , Depth_threshold 3 )]
  ; color_scheme : color array
  ; atom_coloring : atom_coloring [@default Color_first 3]
  ; atom_printing : atom_printing [@default Escaped]
  ; paren_coloring : paren_coloring [@default true]
  ; opening_parens : parens [@default Same_line]
  ; closing_parens : parens [@default Same_line]
  ; comments : comments [@default Print (Indent_comment 3, Some Green, Pretty_print)]
  ; singleton_limit : singleton_limit
                      [@default Singleton_limit (Atom_threshold 3, Character_threshold 15)]
  ; (* Number of atoms that will be marked as leading in regular lists. They will be put on
       a single line, if they fit.
    *)
    leading_threshold : atom_threshold * char_threshold
                        [@default Atom_threshold 3, Character_threshold 20]
  ; separator : separator [@default Empty_line]
  ; sticky_comments : sticky_comments [@default After]
  }
[@@deriving sexp]

let t_of_sexp sexp =
  let t = t_of_sexp sexp in
  if t.indent < 0 then of_sexp_error "Indentation must be non-negative." sexp else t
;;


let default_color_scheme = [| Magenta; Yellow; Cyan; White |]

let default =
  { indent = 2
  ; data_alignment =
      Data_aligned
        ( Parens_alignment false
        , Atom_threshold 6
        , Character_threshold 50
        , Depth_threshold 3 )
  ; color_scheme = default_color_scheme
  ; atom_coloring = Color_first 3
  ; atom_printing = Escaped
  ; paren_coloring = true
  ; closing_parens = Same_line
  ; opening_parens = Same_line
  ; comments = Print (Indent_comment 3, Some Green, Pretty_print)
  ; singleton_limit = Singleton_limit (Atom_threshold 3, Character_threshold 40)
  ; leading_threshold = Atom_threshold 3, Character_threshold 40
  ; separator = Empty_line
  ; sticky_comments = After
  }
;;

let update
      ?color
      ?interpret_atom_as_sexp
      ?drop_comments
      ?new_line_separator
      ?custom_data_alignment
      conf
  =
  let conf =
    match color with
    | None -> conf
    | Some color ->
      if color
      then conf
      else (
        match conf.comments with
        | Print (indent, Some _, style) ->
          { conf with
            atom_coloring = Color_none
          ; paren_coloring = false
          ; comments = Print (indent, None, style)
          }
        | _ -> { conf with atom_coloring = Color_none; paren_coloring = false })
  in
  let conf =
    match interpret_atom_as_sexp with
    | None -> conf
    | Some interpret_atom_as_sexp ->
      if interpret_atom_as_sexp then { conf with atom_printing = Interpreted } else conf
  in
  let conf =
    match drop_comments with
    | None -> conf
    | Some drop_comments -> if drop_comments then { conf with comments = Drop } else conf
  in
  let conf =
    match new_line_separator with
    | None -> conf
    | Some true -> { conf with separator = Empty_line }
    | Some false -> { conf with separator = No_separator }
  in
  let conf =
    match custom_data_alignment with
    | None -> conf
    | Some data_alignment -> { conf with data_alignment }
  in
  conf
;;

let create
      ?(color = false)
      ?(interpret_atom_as_sexp = false)
      ?(drop_comments = false)
      ?(new_line_separator = false)
      ?custom_data_alignment
      ()
  =
  update
    ~color
    ~interpret_atom_as_sexp
    ~drop_comments
    ~new_line_separator
    ?custom_data_alignment
    default
;;
