(** Type of S-expressions *)

type t = Atom of string | List of t list

(*_ We don't use [@@deriving sexp] as this would generated references to [Sexplib],
  creating a circular dependency *)
val t_of_sexp : t -> t
val sexp_of_t : t -> t
val t_sexp_grammar : Raw_grammar.t

val equal : t -> t -> bool
val compare : t -> t -> int

(** [Not_found_s] is used by functions that historically raised [Not_found], to allow them
    to raise an exception that contains an informative error message (as a sexp), while
    still having an exception that can be distinguished from other exceptions. *)
exception Not_found_s of t

(** [Of_sexp_error (exn, sexp)] the exception raised when an S-expression could not be
    successfully converted to an OCaml-value. *)
exception Of_sexp_error of exn * t

(** {1 Helpers} *)

(** Helper to build nice s-expressions for error messages.  It imitates the behavior of
    [[%message ...]] from the ppx_sexp_message rewriter.

    [message name key_values] produces a s-expression list starting with atom [name] and
    followed by list of size 2 of the form [(key value)].  When the key is the empty
    string, [value] is used directly instead as for [[%message]].

    For instance the following code:

    {[
      Sexp.message "error"
        [ "x", sexp_of_int 42
        ; "" , sexp_of_exn Exit
        ]
    ]}

    produces the s-expression:

    {[
      (error (x 42) Exit)
    ]} *)
val message : string -> (string * t) list -> t

(** {1 Defaults} *)

(** [default_indent] reference to default indentation level for human-readable
    conversions.

    Initialisation value: 2. *)
val default_indent : int ref

(** {1 Pretty printing of S-expressions} *)

(** [pp_hum ppf sexp] outputs S-expression [sexp] to formatter [ppf] in human readable
    form. *)
val pp_hum : Format.formatter -> t -> unit

(** [pp_hum_indent n ppf sexp] outputs S-expression [sexp] to formatter [ppf] in human
    readable form and indentation level [n]. *)
val pp_hum_indent : int -> Format.formatter -> t -> unit

(** [pp_mach ppf sexp] outputs S-expression [sexp] to formatter [ppf] in machine readable
    (i.e. most compact) form. *)
val pp_mach : Format.formatter -> t -> unit

(** Same as [pp_mach]. *)
val pp : Format.formatter -> t -> unit

(** {1 Conversion to strings} *)

(** [to_string_hum ?indent sexp] converts S-expression [sexp] to a
    string in human readable form with indentation level [indent].

    @param indent default = [!default_indent] *)
val to_string_hum : ?indent : int -> t -> string

(** [to_string_mach sexp] converts S-expression [sexp] to a string in
    machine readable (i.e. most compact) form. *)
val to_string_mach : t -> string

(** Same as [to_string_mach]. *)
val to_string : t -> string

(** {1 Styles} *)

val of_float_style : [ `Underscores | `No_underscores ] ref
val of_int_style   : [ `Underscores | `No_underscores ] ref
(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig

  (*_ exported for downstream tools *)
  module Raw_grammar : sig
    include module type of struct
      include Raw_grammar
    end

    module Builtin : sig
      val unit_sexp_grammar : t
      val bool_sexp_grammar : t
      val string_sexp_grammar : t
      val bytes_sexp_grammar : t
      val char_sexp_grammar : t
      val int_sexp_grammar : t
      val float_sexp_grammar : t
      val int32_sexp_grammar : t
      val int64_sexp_grammar : t
      val nativeint_sexp_grammar : t
      val ref_sexp_grammar : t
      val lazy_t_sexp_grammar : t
      val option_sexp_grammar : t
      val list_sexp_grammar : t
      val array_sexp_grammar : t
    end

    val empty_sexp_grammar : t
    val opaque_sexp_grammar : t
    val fun_sexp_grammar : t
    val tuple2_sexp_grammar : t
  end

  (*_ Exported for sexplib *)

  val size : t -> int * int

  val buffer : unit -> Buffer.t

  val to_buffer      : buf:Buffer.t ->                t -> unit
  val to_buffer_hum  : buf:Buffer.t -> ?indent:int -> t -> unit
  val to_buffer_mach : buf:Buffer.t ->                t -> unit
  val to_buffer_gen
    :  buf : 'buffer
    -> add_char : ('buffer -> char -> unit)
    -> add_string : ('buffer -> string -> unit)
    -> t
    -> unit

  val mach_maybe_esc_str : string -> string
  val must_escape : string -> bool
  val esc_str : string -> string

end
