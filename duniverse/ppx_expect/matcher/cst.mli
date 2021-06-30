(** Concrete syntax tree of expectations and actual outputs *)

(** These types represent the contents of an [%expect] node or of the actual output. We
    keep information about the original layout so that we can give an corrected
    expectation that follows the original formatting.

    In the following names, blank means ' ' or '\t', while space means blank or newline.
*)

open! Base
open Import

module Line : sig
  type 'a not_blank =
    { trailing_blanks : string (** regexp: "[ \t]*" *)
    ; orig : string
    (** Original contents of the line without the trailing blanks or indentation.
        regexp: "[^\n]*[^ \t\n]" *)
    ; data : 'a
    (** Data associated to the line. *)
    }
  [@@deriving_inline sexp_of, compare, equal]
  include
    sig
      [@@@ocaml.warning "-32"]
      val sexp_of_not_blank :
        ('a -> Ppx_sexp_conv_lib.Sexp.t) ->
        'a not_blank -> Ppx_sexp_conv_lib.Sexp.t
      val compare_not_blank :
        ('a -> 'a -> int) -> 'a not_blank -> 'a not_blank -> int
      val equal_not_blank :
        ('a -> 'a -> bool) -> 'a not_blank -> 'a not_blank -> bool
    end[@@ocaml.doc "@inline"]
  [@@@end]

  type 'a t =
    | Blank     of string  (** regexp: "[ \t]*" *)
    | Not_blank of 'a not_blank
  [@@deriving_inline sexp_of, compare, equal]
  include
    sig
      [@@@ocaml.warning "-32"]
      val sexp_of_t :
        ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
      val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    end[@@ocaml.doc "@inline"]
  [@@@end]

  val invariant : ('a -> unit) -> 'a t -> unit

  (** The callback receive the [orig] and [data] fields *)
  val map : 'a t -> f:(string -> 'a -> 'b) -> 'b t

  (** Delete trailing blanks (everything for blank lines) *)
  val strip : 'a t -> 'a t

  val data : 'a t -> blank:'a -> 'a
end

(** Single line represent [%expect] nodes with data on the first line but not on the
    subsequent ones.

    For instance:

    {[
      [%expect "  blah "];
      [%expect {|  blah
               |}]
    ]}
*)
type 'a single_line =
  { leading_blanks  : string    (** regexp: "[ \t]*" *)
  ; trailing_spaces : string (** regexp: "[ \t\n]*" *)
  ; orig            : string (** regexp: "[^ \t\n]([^\n]*[^ \t\n])?" *)
  ; data            : 'a
  }
[@@deriving_inline sexp_of, compare, equal]
include
  sig
    [@@@ocaml.warning "-32"]
    val sexp_of_single_line :
      ('a -> Ppx_sexp_conv_lib.Sexp.t) ->
      'a single_line -> Ppx_sexp_conv_lib.Sexp.t
    val compare_single_line :
      ('a -> 'a -> int) -> 'a single_line -> 'a single_line -> int
    val equal_single_line :
      ('a -> 'a -> bool) -> 'a single_line -> 'a single_line -> bool
  end[@@ocaml.doc "@inline"]
[@@@end]

(** Any [%expect] node with one or more newlines and at least one non-blank line.

    This also include the case with exactly one non-blank line such as:

    {[
      [%expect {|
        blah
      |}]
    ]}

    This is to preserve this formatting in case the correction is multi-line.

    [leading_spaces] contains everything until the first non-blank line, while
    [trailing_spaces] is either:

    - trailing blanks on the last line if of the form:

    {[
      [%expect {|
                 abc
                 def |}]
    ]}

    - all trailing spaces from the newline character (inclusive) on the last non-blank
      line to the end if of the form:

    {[
      [%expect {|
                 abc
                 def
      |}]
    ]}
*)
type 'a multi_lines =
  { leading_spaces  : string (** regexp: "\([ \t]*\n\)*" *)
  ; trailing_spaces : string (** regexp: "[ \t]*" or "\(\n[ \t]*\)*" *)
  ; indentation     : string (** regexp: "[ \t]*" *)
  ; lines           : 'a Line.t list (** regexp: not_blank (.* not_blank)? *)
  }
[@@deriving_inline sexp_of, compare, equal]
include
  sig
    [@@@ocaml.warning "-32"]
    val sexp_of_multi_lines :
      ('a -> Ppx_sexp_conv_lib.Sexp.t) ->
      'a multi_lines -> Ppx_sexp_conv_lib.Sexp.t
    val compare_multi_lines :
      ('a -> 'a -> int) -> 'a multi_lines -> 'a multi_lines -> int
    val equal_multi_lines :
      ('a -> 'a -> bool) -> 'a multi_lines -> 'a multi_lines -> bool
  end[@@ocaml.doc "@inline"]
[@@@end]

type 'a t =
  | Empty       of string (** regexp: "[ \t\n]*" *)
  | Single_line of 'a single_line
  | Multi_lines of 'a multi_lines
[@@deriving_inline sexp_of, compare, equal]
include
  sig
    [@@@ocaml.warning "-32"]
    val sexp_of_t :
      ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end[@@ocaml.doc "@inline"]
[@@@end]

val invariant : ('a -> unit) -> 'a t -> unit

val empty : 'a t

val map : 'a t -> f:(string -> 'a -> 'b) -> 'b t

val data : 'a t -> blank:'a -> 'a list

val strip : 'a t -> 'a t

val to_string : _ t -> string

(** For single line expectation, leading blanks and trailing spaces are dropped. *)
val to_lines : 'a t -> 'a Line.t list

(** Remove blank lines at the beginning and end of the list. *)
val trim_lines : 'a Line.t list -> 'a Line.t list

(** Given a contents [t] and a list of [lines], try to produce a new contents containing
    [lines] but with the same formating as [t].

    [default_indentation] is the indentation to use in case we ignore [t]'s indentation
    (for instance if [t] is [Single_line] or [Empty]). *)
val reconcile
  :  'a t
  -> lines               : 'a Line.t list
  -> default_indentation : int
  -> pad_single_line     : bool
  -> 'a t

(** Compuute the longest indentation of a list of lines and trim it from every line. It
    returns the found indentation and the list of trimmed lines. *)
val extract_indentation : 'a Line.t list -> string * 'a Line.t list

(** All the [.orig] fields of [Line.t] or [single_line] values, using [""] for blank
    lines. *)
val stripped_original_lines : _ t -> string list
