(** Compact set of positions *)

open! Import

(** A [t] value represent a sequence of positions. The focus is on small memory footprint.

    Given a s-expression and a sequence of positions, one can reconstruct the location of
    every sub s-expression. This is used to report location informations without having to
    annotate every node in the s-expression during parsing.

    The s-expression parser saves the positions of each opening and closing parentheses as
    well as the positions of the first and last character of each atom.

    Note that a [t] can hold the same given positions no more than twice. The parser
    stores the same position twice for non-quoted single character atoms.
*)
type t [@@deriving_inline sexp_of, compare]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val compare : t -> t -> int
end
[@@ocaml.doc "@inline"]

[@@@end]

(** Represent a position in the input *)
type pos =
  { line : int (** Line number. The first line has number [1].               *)
  ; col : int (** Column number. The first column has number [0].           *)
  ; offset : int
  (** Number of bytes from the beginning of the input. The first
      byte has offset [0]. *)
  }
[@@deriving_inline sexp_of, compare]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_pos : pos -> Ppx_sexp_conv_lib.Sexp.t
  val compare_pos : pos -> pos -> int
end
[@@ocaml.doc "@inline"]

[@@@end]

val beginning_of_file : pos
val shift_pos : pos -> cols:int -> pos

(** Range of positions, as reported in error messages. We follow the lexing conventions of
    OCaml, i.e. [start_pos] points to the first character and [end_pos] points to the
    position just after the last character.

    This allow for instance to represent empty ranges with [start_pos = end_pos].
*)
type range =
  { start_pos : pos
  ; end_pos : pos
  }
[@@deriving_inline sexp_of, compare]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_range : range -> Ppx_sexp_conv_lib.Sexp.t
  val compare_range : range -> range -> int
end
[@@ocaml.doc "@inline"]

[@@@end]

(** Make a range from two positions where both positions are inclusive, i.e. [start_pos]
    points to the first character and [end_pos] points to the last one.
    The character at [last_pos] is assumed to not be a newline character. *)
val make_range_incl : start_pos:pos -> last_pos:pos -> range

module Builder : sig
  type positions
  type t

  val create : ?initial_pos:pos -> unit -> t

  (** [add], [add_twice] and [add_newline] must be called with strictly increasing
      [offset] values. *)

  (** int is absolute offset of the position *)
  val add : t -> offset:int -> unit

  val add_twice : t -> offset:int -> unit

  (** int is absolute offset of the newline character *)
  val add_newline : t -> offset:int -> unit

  val contents : t -> positions
  val reset : t -> pos -> unit
end
with type positions := t

(** Build the list of all positions in [t]. *)
val to_list : t -> pos list

(** Build the array of all positions in [t]. *)
val to_array : t -> pos array

(** [find t start stop] returns the range of positions starting at position with index
    [start] in [t] and ending at position with index [stop].

    [find t i j] is the same as:

    {[
      let a = to_array t in
      make_range_incl ~start_pos:a.(i) ~last_pos:a.(j)
    ]}

    but more efficient.
*)
val find : t -> int -> int -> range

(** [find_sub_sexp_phys t sexp ~sub] looks for [sub] in [sexp] and return its location,
    assuming [t] is the sequence of positions associated with [sexp].

    Comparison is done using physical equality.
*)
val find_sub_sexp_phys : t -> Sexp.t -> sub:Sexp.t -> range option

val find_sub_sexp_in_list_phys : t -> Sexp.t list -> sub:Sexp.t -> range option

(** Returns how much memory is used by [t] *)
val memory_footprint_in_bytes : t -> int

(** API for iterating over positions in an efficient way *)
module Iterator : sig
  type positions = t
  type t

  val create : positions -> t

  (** Exception raised when the iterator has reached the end of the sequence. *)
  exception No_more

  (** [advance t ~skip] skips the next [skip] positions in the sequence, advance to the
      next position and return it.
      Raises [No_more] when reaching the end of the position set. *)
  val advance_exn : t -> skip:int -> pos
end
with type positions := t
