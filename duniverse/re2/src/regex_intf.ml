open Core_kernel

module type S = sig

  (** These are OCaml bindings for Google's re2 library.  Quoting from the re2 homepage:

      {v
RE2 is a fast, safe, thread-friendly alternative to backtracking regular
expression engines like those used in PCRE, Perl, and Python. It is a C++ library.

Unlike most automata-based engines, RE2 implements almost all the common Perl and
PCRE features and syntactic sugars. It also finds the leftmost-first match, the
same match that Perl would, and can return submatch information. The one
significant exception is that RE2 drops support for backreferences¹ and
generalized zero-width assertions, because they cannot be implemented
efficiently. The syntax page gives full details. v}

      Syntax reference: {:https://github.com/google/re2/wiki/Syntax}
   **)

  (** Although OCaml strings and C++ strings may legally have internal null bytes, this
      library doesn't handle them correctly by doing conversions via C strings.
      The failure mode is the search stops early, which isn't bad considering how rare
      internal null bytes are in practice.

      The strings are considered according to [Options.encoding] which is UTF-8 by
      default (the alternative is ISO 8859-1).
  *)

  (** {6 Basic Types} *)

  type t [@@deriving bin_io, compare, sexp, hash]
  type regex = t

  (** Subpatterns are referenced by name if labelled with the [/(?P<...>...)/] syntax, or
      else by counting open-parens, with subpattern zero referring to the whole regex. *)
  type id_t = [ `Index of int | `Name of string ]

  (** [index_of_id t id] resolves subpattern names and indices into indices. **)
  val index_of_id_exn : t -> id_t -> int

  (** The [sub] keyword argument means, omit location information for subpatterns with
      index greater than [sub].

      Subpatterns are indexed by the number of opening parentheses preceding them:

      [~sub:(`Index 0)]  : only the whole match
      [~sub:(`Index 1)]  : the whole match and the first submatch, etc.

      If you only care whether the pattern does match, you can request no location
      information at all by passing [~sub:(`Index -1)].

      With one exception, I quote from re2.h:443,

      {v
Don't ask for more match information than you will use:
runs much faster with nmatch == 1 than nmatch > 1, and
runs even faster if nmatch == 0. v}

      For [sub > 1], re2 executes in three steps:
      1. run a DFA over the entire input to get the end of the whole match
      2. run a DFA backward from the end position to get the start position
      3. run an NFA from the match start to match end to extract submatches
      [sub == 1] lets it stop after (2) and [sub == 0] lets it stop after (1).
      (See re2.cc:692 or so.)

      The one exception is for the functions [get_matches], [replace], and
      [Iterator.next]: Since they must iterate correctly through the whole string, they
      need at least the whole match (subpattern 0).  These functions will silently rewrite
      [~sub] to be non-negative.
  *)


  module Options = Options

  val create     : ?options:Options.t -> string -> t Or_error.t
  val create_exn : ?options:Options.t -> string -> t

  include Stringable with type t := t

  (** [num_submatches t] returns 1 + the number of open-parens in the pattern.

      N.B. [num_submatches t == 1 + RE2::NumberOfCapturingGroups()] because
      [RE2::NumberOfCapturingGroups()] ignores the whole match ("subpattern zero").
  *)
  val num_submatches : t -> int

  (** [pattern t] returns the pattern from which the regex was constructed. *)
  val pattern : t -> string

  val options : t -> Options.t

  (** [find_all t input] a convenience function that returns all non-overlapping
      matches of [t] against [input], in left-to-right order.

      If [sub] is given, and the requested subpattern did not capture, then no match is
      returned at that position even if other parts of the regex did match. *)
  val find_all     : ?sub:id_t -> t -> string -> string list Or_error.t
  val find_all_exn : ?sub:id_t -> t -> string -> string list

  (** [find_first ?sub pattern input] finds the first match of [pattern] in [input], and
      returns the subpattern specified by [sub], or an error if the subpattern didn't
      capture. *)
  val find_first     : ?sub:id_t -> t -> string -> string Or_error.t
  val find_first_exn : ?sub:id_t -> t -> string -> string

  (** [find_submatches t input] finds the first match and returns all submatches.
      Element 0 is the whole match and element 1 is the first parenthesized submatch, etc.
  *)
  val find_submatches     : t -> string -> string option array Or_error.t
  val find_submatches_exn : t -> string -> string option array

  (** [matches pattern input] @return true iff [pattern] matches [input] *)
  val matches : t -> string -> bool

  (** [split pattern input] @return [input] broken into pieces where [pattern]
      matches.  Subpatterns are ignored.

      @param max (default: unlimited) split only at the leftmost [max] matches

      @param include_matches (default: false) include the matched substrings in the
      returned list (e.g., the regex [/[,()]/] on ["foo(bar,baz)"] gives [["foo"; "(";
      "bar"; ","; "baz"; ")"]] instead of [["foo"; "bar"; "baz"]])

      If [t] never matches, the returned list has [input] as its one element.
  *)
  val split :
    ?max:int
    -> ?include_matches:bool
    -> t
    -> string
    -> string list

  (** [rewrite pattern ~template input] is a convenience function for [replace]:
      Instead of requiring an arbitrary transformation as a function, it accepts a
      template string with zero or more substrings of the form ["\\n"], each of
      which will be replaced by submatch [n].  For every match of [pattern]
      against [input], the template will be specialized and then substituted for
      the matched substring. *)
  val rewrite     : t -> template:string -> string -> string Or_error.t
  val rewrite_exn : t -> template:string -> string -> string

  (** [valid_rewrite_template pattern ~template] returns [true] iff [template] is a
      valid rewrite template for [pattern] *)
  val valid_rewrite_template        : t -> template:string -> bool

  (** [escape nonregex] returns a copy of [nonregex] with everything escaped (i.e.,
      if the return value were t to regex, it would match exactly the
      original input) *)
  val escape : string -> string

  (** {6 Infix Operators} *)

  module Infix : sig
    (** [input =~ pattern] an infix alias of [matches] *)
    val (=~)  : string -> t -> bool
  end

  (** {6 Complicated Interface} *)

  type 'a without_trailing_none [@@deriving sexp_of]

  (** This type marks call sites affected by a bugfix that eliminated a trailing
      None. When you add this wrapper, check that your call site does not still work
      around the bug by dropping the last element. *)
  val without_trailing_none : 'a -> 'a without_trailing_none

  module Match : sig
    (** A Match.t is the result of applying a regex to an input string *)
    type t [@@deriving sexp_of]

    (** If location information has been omitted (e.g., via [~sub]), the error returned is
        [Regex_no_such_subpattern], just as though that subpattern were never defined.
    *)
    val get     : sub:id_t -> t -> string option
    val get_exn : sub:id_t -> t -> string

    (** [get_all t] returns all available matches as strings in an array.  For the
        indexing convention, see comment above regarding [sub] parameter. *)
    val get_all : t without_trailing_none -> string option array

    (** [get_pos_exn ~sub t] returns the start offset and length in bytes.  Note that for
        variable-width encodings (e.g., UTF-8) this may not be the same as the character
        offset and character length.
    *)
    val get_pos_exn : sub:id_t -> t -> (int * int)
  end

  (** [get_matches pattern input] returns all non-overlapping matches of [pattern]
      against [input]

      @param max (default: unlimited) return only the leftmost [max] matches
      @param sub (default: all) returned Match.t's will contain only the first [sub]
      matches.
  *)
  val get_matches
    : ?sub:id_t
    -> ?max:int
    -> t
    -> string
    -> Match.t list Or_error.t

  val get_matches_exn
    : ?sub:id_t
    -> ?max:int
    -> t
    -> string
    -> Match.t list

  val to_sequence_exn
    : ?sub:id_t
    -> t
    -> string
    -> Match.t Sequence.t

  (** [first_match pattern input] @return the first match iff [pattern] matches [input] *)
  val first_match : t -> string -> Match.t Or_error.t
  val first_match_exn : t -> string -> Match.t

  (** [replace ?sub ?max ~f pattern input] @return an edited copy of [input] with every
      substring matched by [pattern] transformed by [f].

      @param only (default: all) replace only the nth match
  *)
  val replace
    :  ?sub:id_t
    -> ?only:int
    -> f:(Match.t -> string)
    -> t
    -> string
    -> string Or_error.t

  val replace_exn
    :  ?sub:id_t
    -> ?only:int
    -> f:(Match.t -> string)
    -> t
    -> string
    -> string

  module Exceptions : sig
    (** [Regex_no_such_subpattern (n, max)] means [n] was requested but only [max]
        subpatterns are defined (so [max] - 1 is the highest valid index) *)
    exception Regex_no_such_subpattern of int * int

    (** [Regex_no_such_named_subpattern (name, pattern)] *)
    exception Regex_no_such_named_subpattern of string * string

    (** [Regex_match_failed pattern] *)
    exception Regex_match_failed of string

    (** [Regex_submatch_did_not_capture (s, i)] means the [i]th subpattern in the
        regex compiled from [s] did not capture a substring. *)
    exception Regex_submatch_did_not_capture of string * int

    (** the string is the C library's error message, generally in the form of
        "(human-readable error): (piece of pattern that did not compile)" *)
    exception Regex_compile_failed of string

    (** [Regex_rewrite_template_invalid (template, error_msg)] *)
    exception Regex_rewrite_template_invalid of string * string
  end

  module Multiple : sig
    (** An efficient way to ask which of several regexes matches a string. *)
    type 'a t

    (** [create ?options [ (pattern1, value1); (pattern2, value2); ...]] associates each
        [pattern] with its [value]. The same [options] are used for all patterns. *)
    val create     : ?options:Options.t -> (string * 'a) list -> 'a t Or_error.t
    val create_exn : ?options:Options.t -> (string * 'a) list -> 'a t

    (** [matches t input] returns the values associated with those patterns that match the
        [input]. Values are in the order that [create] saw them. *)
    val matches : 'a t -> string -> 'a list

    (** Like [matches], but values are listed in unspecified order. *)
    val matches_no_order : 'a t -> string -> 'a list
  end

end
