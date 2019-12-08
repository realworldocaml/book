open! Base

module CR = struct
  type t =
    | CR
    | CR_soon
    | CR_someday
    | Comment
  [@@deriving sexp_of]
end

module Sexp_style = struct
  type t =
    | To_string_mach
    | To_string_hum
    | Pretty of Sexp_pretty.Config.t
  [@@deriving sexp_of]
end

module type Set = sig
  type t [@@deriving sexp_of]

  val diff : t -> t -> t
  val equal : t -> t -> bool
  val is_empty : t -> bool
end

module type With_compare = sig
  type t [@@deriving compare, sexp_of]
end

module type With_equal = sig
  type t [@@deriving sexp_of]

  include Equal.S with type t := t
end

module Quickcheck = Base_quickcheck

module type Expect_test_helpers_base = sig
  module type Set = Set
  module type With_compare = With_compare
  module type With_equal = With_equal

  module CR : sig
    include module type of struct
    include CR
  end

    (** [hide_unstable_output t] returns [false] if [t = CR] and [true] otherwise.  Useful
        to provide a default for arguments such as [?hide_positions] in functions that
        also have a [?cr] argument. *)
    val hide_unstable_output : t -> bool
  end

  module Sexp_style : sig
    include module type of struct
    include Sexp_style
  end

    (** Pretty-printing via [Sexp_pretty] with default config, except no colors. *)
    val default_pretty : t

    (** Pretty-printing via [Sexp_pretty] with most heuristics disabled. *)
    val simple_pretty : t
  end

  (** [hide_positions_in_string] does line-based regexp matching to replace line numbers
      and column numbers that appear in source-code positions with constant text [LINE]
      and [COL].  This can be useful in making displayed test output less fragile. *)
  val hide_positions_in_string : string -> string

  (** Renders an s-expression as a string.  With [~hide_positions:true], patterns in the
      string that match OCaml-style file positions are modified to hide the line number,
      column number, and character positions, to make output less fragile. *)
  val sexp_to_string : ?hide_positions:bool (** default is [false] *) -> Sexp.t -> string

  (** For printing an s-expression to stdout.  [hide_positions] works as in
      [sexp_to_string]. *)
  val print_s : ?hide_positions:bool (** default is [false] *) -> Sexp.t -> unit

  (** [print_cr here message] prints a [CR require-failed], which will appear in
      expect-test output. The CR will appear in the feature owner's [fe todo], thus
      preventing release of the feature. [print_cr] is an expect-test-friendly version of
      [assert false]. It works with the normal expect-test workflow because it does not
      raise, and it prevents mistakenly releasing features that violate a required
      property. There is no need to 'X' a [CR require-failed]; simply fix the property
      that triggered the [print_cr] and re-run the test to restore the empty output. *)
  val print_cr
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> Sexp.t
    -> unit

  (** [require here bool] is a no-op if [bool = true], but if not, prints a [CR
      require-failed] similarly to [print_cr], with a message determined by the
      [if_false_then_print_s] argument, if any.

      [if_false_then_print_s] is useful for including information that may help debug the
      problem, but that would otherwise be too voluminous.  [if_false_then_print_s] is
      lazy to avoid construction of the sexp except when needed. *)
  val require
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?if_false_then_print_s:Sexp.t Lazy.t
    -> Source_code_position.t
    -> bool
    -> unit

  (** [require_equal] compares its two arguments using the equality predicate of the
      provided module. If the comparison fails, prints a message that renders the
      arguments as sexps. *)
  val require_equal
    :  ?cr:CR.t (** default is [CR]    *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?if_false_then_print_s:Sexp.t Lazy.t
    -> ?message:string
    -> Source_code_position.t
    -> (module With_equal with type t = 'a)
    -> 'a
    -> 'a
    -> unit

  (** Like [require_equal], but derives an equality predicate from a comparison
      function. *)
  val require_compare_equal
    :  ?cr:CR.t (** default is [CR]    *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?message:string
    -> Source_code_position.t
    -> (module With_compare with type t = 'a)
    -> 'a
    -> 'a
    -> unit

  (** Like [require_equal], but when equality fails produces a message including sexps of
      both [Set.diff first second] and [Set.diff second first] to aid in debugging. *)
  val require_sets_are_equal
    :  ?cr:CR.t (** default is [CR]    *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?names:string * string (** default is ["first", "second"] *)
    -> Source_code_position.t
    -> (module Set with type t = 'a)
    -> 'a
    -> 'a
    -> unit

  (** [show_raise] calls [f ()] and prints the exception that it raises, or, if it doesn't
      raise, prints [did not raise].  [show_raise] ignores the result of [f] so that one
      doesn't have to put an [ignore] inside the body of an [f] that is expected to raise.
      [~hide_positions:true] operates as in [print_s], to make output less fragile.  Using
      [~show_backtrace:true] will result in a CR in the expectation, but it's still
      available here as it is still valuable when initially writing tests and
      debugging. *)
  val show_raise
    :  ?hide_positions:bool (** default is [false] *)
    -> ?show_backtrace:bool (** default is [false] *)
    -> (unit -> _)
    -> unit

  (** [require_does_not_raise] is like [show_raise], but does not print anything if the
      function does not raise, and prints a CR along with the exception if it does raise.
      Unlike for [show_raise], the supplied function is required to return [unit] to avoid
      mistakes like incomplete partial application that silently would not raise, but for
      the wrong reason. *)
  val require_does_not_raise
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?show_backtrace:bool (** default is [false] *)
    -> Source_code_position.t
    -> (unit -> unit)
    -> unit

  (** [require_does_raise] is like [show_raise], but additionally prints a CR if the
      function does not raise. *)
  val require_does_raise
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?show_backtrace:bool (** default is [false] *)
    -> Source_code_position.t
    -> (unit -> _)
    -> unit

  (** [quickcheck] is similar to [Base_quickcheck.Test.run]. It stops after the first
      iteration that raises or prints a CR, as detected by [on_print_cr]. *)
  val quickcheck
    :  Source_code_position.t
    -> ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?seed:Quickcheck.Test.Config.Seed.t
    -> ?sizes:int Sequence.t
    -> ?trials:int
    -> ?shrinker:'a Quickcheck.Shrinker.t
    -> ?shrink_attempts:int
    -> ?examples:'a list
    -> sexp_of:('a -> Sexp.t)
    -> f:('a -> unit)
    -> 'a Quickcheck.Generator.t
    -> unit

  (** [sexp_style] determines the sexp format used by [sexp_to_string], [print_s], and
      other functions in this module.  Defaults to [Sexp_style.default_pretty]. *)
  val sexp_style : Sexp_style.t ref

  (** [on_print_cr] determines the behavior of all functions above that print CRs, such as
      [print_cr] and [require].  The rendered string form of the CR is passed to
      [!on_print_cr].  The default value is [print_endline]; this can be overridden to
      replace or extend the default behavior.  For example, some testing harnesses may
      choose to abort a series of tests after the first CR is printed. *)
  val on_print_cr : (string -> unit) ref
end
