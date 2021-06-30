(** An extension of the standard [StringLabels].  If you [open Base], you'll get these
    extensions in the [String] module. *)

open! Import

type t = string [@@deriving_inline sexp, sexp_grammar]

include Ppx_sexp_conv_lib.Sexpable.S with type t := t

val t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t

[@@@end]

val sub : (t, t) Blit.sub
val subo : (t, t) Blit.subo

include Container.S0 with type t := t with type elt = char
include Identifiable.S with type t := t
include Invariant.S with type t := t

(** Maximum length of a string. *)
val max_length : int

external length : t -> int = "%string_length"
external get : t -> int -> char = "%string_safe_get"

(** [unsafe_get t i] is like [get t i] but does not perform bounds checking. The caller
    must ensure that it is a memory-safe operation. *)
external unsafe_get : string -> int -> char = "%string_unsafe_get"

val make : int -> char -> t

(** Assuming you haven't passed -unsafe-string to the compiler, strings are immutable, so
    there'd be no motivation to make a copy. *)
val copy : t -> t
[@@deprecated "[since 2018-03] Use [Bytes.copy] instead"]

val init : int -> f:(int -> char) -> t

(** String append. Also available unqualified, but re-exported here for documentation
    purposes.

    Note that [a ^ b] must copy both [a] and [b] into a newly-allocated result string, so
    [a ^ b ^ c ^ ... ^ z] is quadratic in the number of strings.  [String.concat] does not
    have this problem -- it allocates the result buffer only once. *)
val ( ^ ) : t -> t -> t

(** Concatenates all strings in the list using separator [sep] (with a default separator
    [""]). *)
val concat : ?sep:t -> t list -> t

(** Special characters are represented by escape sequences, following the lexical
    conventions of OCaml. *)
val escaped : t -> t

val contains : ?pos:int -> ?len:int -> t -> char -> bool

(** Operates on the whole string using the US-ASCII character set,
    e.g. [uppercase "foo" = "FOO"]. *)
val uppercase : t -> t

val lowercase : t -> t

(** Operates on just the first character using the US-ASCII character set,
    e.g. [capitalize "foo" = "Foo"]. *)
val capitalize : t -> t

val uncapitalize : t -> t

(** [index] gives the index of the first appearance of [char] in the string when
    searching from left to right, or [None] if it's not found. [rindex] does the same but
    searches from the right.

    For example, [String.index "Foo" 'o'] is [Some 1] while [String.rindex "Foo" 'o'] is
    [Some 2].

    The [_exn] versions return the actual index (instead of an option) when [char] is
    found, and throw an exception otherwise.
*)

(** [Caseless] compares and hashes strings ignoring case, so that for example
    [Caseless.equal "OCaml" "ocaml"] and [Caseless.("apple" < "Banana")] are [true].

    [Caseless] also provides case-insensitive [is_suffix] and [is_prefix] functions, so
    that for example [Caseless.is_suffix "OCaml" ~suffix:"AmL"] and [Caseless.is_prefix
    "OCaml" ~prefix:"oc"] are [true]. *)
module Caseless : sig
  type nonrec t = t [@@deriving_inline hash, sexp]

  val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

  include Ppx_sexp_conv_lib.Sexpable.S with type t := t

  [@@@end]

  include Comparable.S with type t := t

  val is_suffix : t -> suffix:t -> bool
  val is_prefix : t -> prefix:t -> bool
  val is_substring : t -> substring:t -> bool
  val is_substring_at : t -> pos:int -> substring:t -> bool
  val substr_index : ?pos:int -> t -> pattern:t -> int option
  val substr_index_exn : ?pos:int -> t -> pattern:t -> int
  val substr_index_all : t -> may_overlap:bool -> pattern:t -> int list
  val substr_replace_first : ?pos:int -> t -> pattern:t -> with_:t -> t
  val substr_replace_all : t -> pattern:t -> with_:t -> t
end

(** [index_exn] and [index_from_exn] raise [Caml.Not_found] or [Not_found_s] when [char]
    cannot be found in [s]. *)
val index : t -> char -> int option

val index_exn : t -> char -> int
val index_from : t -> int -> char -> int option
val index_from_exn : t -> int -> char -> int


(** [rindex_exn] and [rindex_from_exn] raise [Caml.Not_found] or [Not_found_s] when [char]
    cannot be found in [s]. *)
val rindex : t -> char -> int option

val rindex_exn : t -> char -> int
val rindex_from : t -> int -> char -> int option
val rindex_from_exn : t -> int -> char -> int

(** Substring search and replace functions.  They use the Knuth-Morris-Pratt algorithm
    (KMP) under the hood.

    The functions in the [Search_pattern] module allow the program to preprocess the
    searched pattern once and then use it many times without further allocations. *)
module Search_pattern : sig
  type t [@@deriving_inline sexp_of]

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

  [@@@end]

  (** [create pattern] preprocesses [pattern] as per KMP, building an [int array] of
      length [length pattern].  All inputs are valid. *)
  val create : ?case_sensitive:bool (** default = true *) -> string -> t

  (** [pattern t] returns the string pattern used to create [t]. *)
  val pattern : t -> string

  (** [case_sensitive t] returns whether [t] matches strings case-sensitively. *)
  val case_sensitive : t -> bool

  (** [matches pat str] returns true if [str] matches [pat] *)
  val matches : t -> string -> bool

  (** [pos < 0] or [pos >= length string] result in no match (hence [index] returns
      [None] and [index_exn] raises). *)
  val index : ?pos:int -> t -> in_:string -> int option

  val index_exn : ?pos:int -> t -> in_:string -> int

  (** [may_overlap] determines whether after a successful match, [index_all] should start
      looking for another one at the very next position ([~may_overlap:true]), or jump to
      the end of that match and continue from there ([~may_overlap:false]), e.g.:

      - [index_all (create "aaa") ~may_overlap:false ~in_:"aaaaBaaaaaa" = [0; 5; 8]]
      - [index_all (create "aaa") ~may_overlap:true ~in_:"aaaaBaaaaaa" = [0; 1; 5; 6; 7;
        8]]

      E.g., [replace_all] internally calls [index_all ~may_overlap:false]. *)
  val index_all : t -> may_overlap:bool -> in_:string -> int list

  (** Note that the result of [replace_all pattern ~in_:text ~with_:r] may still
      contain [pattern], e.g.,

      {[
        replace_all (create "bc") ~in_:"aabbcc" ~with_:"cb" = "aabcbc"
      ]} *)
  val replace_first : ?pos:int -> t -> in_:string -> with_:string -> string

  val replace_all : t -> in_:string -> with_:string -> string

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    type public = t

    type t =
      { pattern : string
      ; case_sensitive : bool
      ; kmp_array : int array
      }
    [@@deriving_inline equal, sexp_of]

    val equal : t -> t -> bool
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]

    val representation : public -> t
  end
end

(** Substring search and replace convenience functions.  They call [Search_pattern.create]
    and then forget the preprocessed pattern when the search is complete.  [pos < 0] or
    [pos >= length t] result in no match (hence [substr_index] returns [None] and
    [substr_index_exn] raises).  [may_overlap] indicates whether to report overlapping
    matches, see [Search_pattern.index_all]. *)
val substr_index : ?pos:int -> t -> pattern:t -> int option

val substr_index_exn : ?pos:int -> t -> pattern:t -> int
val substr_index_all : t -> may_overlap:bool -> pattern:t -> int list
val substr_replace_first : ?pos:int -> t -> pattern:t -> with_:t -> t

(** As with [Search_pattern.replace_all], the result may still contain [pattern]. *)
val substr_replace_all : t -> pattern:t -> with_:t -> t

(** [is_substring ~substring:"bar" "foo bar baz"] is true. *)
val is_substring : t -> substring:t -> bool

(** [is_substring_at "foo bar baz" ~pos:4 ~substring:"bar"] is true. *)
val is_substring_at : t -> pos:int -> substring:t -> bool

(** Returns the reversed list of characters contained in a list. *)
val to_list_rev : t -> char list

(** [rev t] returns [t] in reverse order. *)
val rev : t -> t

(** [is_suffix s ~suffix] returns [true] if [s] ends with [suffix]. *)
val is_suffix : t -> suffix:t -> bool

(** [is_prefix s ~prefix] returns [true] if [s] starts with [prefix]. *)
val is_prefix : t -> prefix:t -> bool

(** If the string [s] contains the character [on], then [lsplit2_exn s ~on] returns a pair
    containing [s] split around the first appearance of [on] (from the left). Raises
    [Caml.Not_found] or [Not_found_s] when [on] cannot be found in [s]. *)
val lsplit2_exn : t -> on:char -> t * t

(** If the string [s] contains the character [on], then [rsplit2_exn s ~on] returns a pair
    containing [s] split around the first appearance of [on] (from the right). Raises
    [Caml.Not_found] or [Not_found_s] when [on] cannot be found in [s]. *)
val rsplit2_exn : t -> on:char -> t * t

(** [lsplit2 s ~on] optionally returns [s] split into two strings around the
    first appearance of [on] from the left. *)
val lsplit2 : t -> on:char -> (t * t) option

(** [rsplit2 s ~on] optionally returns [s] split into two strings around the first
    appearance of [on] from the right. *)
val rsplit2 : t -> on:char -> (t * t) option

(** [split s ~on] returns a list of substrings of [s] that are separated by [on].
    Consecutive [on] characters will cause multiple empty strings in the result.
    Splitting the empty string returns a list of the empty string, not the empty list. *)
val split : t -> on:char -> t list

(** [split_on_chars s ~on] returns a list of all substrings of [s] that are separated by
    one of the chars from [on].  [on] are not grouped.  So a grouping of [on] in the
    source string will produce multiple empty string splits in the result.  *)
val split_on_chars : t -> on:char list -> t list

(** [split_lines t] returns the list of lines that comprise [t].  The lines do not include
    the trailing ["\n"] or ["\r\n"]. *)
val split_lines : t -> t list

(** [lfindi ?pos t ~f] returns the smallest [i >= pos] such that [f i t.[i]], if there is
    such an [i].  By default, [pos = 0]. *)
val lfindi : ?pos:int -> t -> f:(int -> char -> bool) -> int option

(** [rfindi ?pos t ~f] returns the largest [i <= pos] such that [f i t.[i]], if there is
    such an [i].  By default [pos = length t - 1]. *)
val rfindi : ?pos:int -> t -> f:(int -> char -> bool) -> int option

(** [lstrip ?drop s] returns a string with consecutive chars satisfying [drop] (by default
    white space, e.g. tabs, spaces, newlines, and carriage returns) stripped from the
    beginning of [s]. *)
val lstrip : ?drop:(char -> bool) -> t -> t

(** [rstrip ?drop s] returns a string with consecutive chars satisfying [drop] (by default
    white space, e.g. tabs, spaces, newlines, and carriage returns) stripped from the end
    of [s]. *)
val rstrip : ?drop:(char -> bool) -> t -> t

(** [strip ?drop s] returns a string with consecutive chars satisfying [drop] (by default
    white space, e.g. tabs, spaces, newlines, and carriage returns) stripped from the
    beginning and end of [s]. *)
val strip : ?drop:(char -> bool) -> t -> t

val map : t -> f:(char -> char) -> t

(** Like [map], but passes each character's index to [f] along with the char. *)
val mapi : t -> f:(int -> char -> char) -> t


(** [foldi] works similarly to [fold], but also passes the index of each character to
    [f]. *)
val foldi : t -> init:'a -> f:(int -> 'a -> char -> 'a) -> 'a

(** Like [map], but allows the replacement of a single character with zero or two or more
    characters. *)
val concat_map : ?sep:t -> t -> f:(char -> t) -> t

(** [filter s ~f:predicate] discards characters not satisfying [predicate]. *)
val filter : t -> f:(char -> bool) -> t

(** [tr ~target ~replacement s] replaces every instance of [target] in [s] with
    [replacement]. *)
val tr : target:char -> replacement:char -> t -> t

(** [tr_multi ~target ~replacement] returns a function that replaces every
    instance of a character in [target] with the corresponding character in
    [replacement].

    If [replacement] is shorter than [target], it is lengthened by repeating
    its last character. Empty [replacement] is illegal unless [target] also is.

    If [target] contains multiple copies of the same character, the last
    corresponding [replacement] character is used. Note that character ranges
    are {b not} supported, so [~target:"a-z"] means the literal characters ['a'],
    ['-'], and ['z']. *)
val tr_multi : target:t -> replacement:t -> (t -> t) Staged.t

(** [chop_suffix_exn s ~suffix] returns [s] without the trailing [suffix],
    raising [Invalid_argument] if [suffix] is not a suffix of [s]. *)
val chop_suffix_exn : t -> suffix:t -> t

(** [chop_prefix_exn s ~prefix] returns [s] without the leading [prefix],
    raising [Invalid_argument] if [prefix] is not a prefix of [s]. *)
val chop_prefix_exn : t -> prefix:t -> t

val chop_suffix : t -> suffix:t -> t option
val chop_prefix : t -> prefix:t -> t option

(** [chop_suffix_if_exists s ~suffix] returns [s] without the trailing [suffix], or just
    [s] if [suffix] isn't a suffix of [s].

    Equivalent to [chop_suffix s ~suffix |> Option.value ~default:s], but avoids
    allocating the intermediate option. *)
val chop_suffix_if_exists : t -> suffix:t -> t

(** [chop_prefix_if_exists s ~prefix] returns [s] without the leading [prefix], or just
    [s] if [prefix] isn't a prefix of [s].

    Equivalent to [chop_prefix s ~prefix |> Option.value ~default:s], but avoids
    allocating the intermediate option. *)
val chop_prefix_if_exists : t -> prefix:t -> t

(** [suffix s n] returns the longest suffix of [s] of length less than or equal to [n]. *)
val suffix : t -> int -> t

(** [prefix s n] returns the longest prefix of [s] of length less than or equal to [n]. *)
val prefix : t -> int -> t

(** [drop_suffix s n] drops the longest suffix of [s] of length less than or equal to
    [n]. *)
val drop_suffix : t -> int -> t

(** [drop_prefix s n] drops the longest prefix of [s] of length less than or equal to
    [n]. *)
val drop_prefix : t -> int -> t

(** [concat_array sep ar] like {!String.concat}, but operates on arrays. *)
val concat_array : ?sep:t -> t array -> t

(** Slightly faster hash function on strings. *)
external hash : t -> int = "Base_hash_string"
[@@noalloc]

(** Fast equality function on strings, doesn't use [compare_val]. *)
val equal : t -> t -> bool

val of_char : char -> t
val of_char_list : char list -> t

(** Operations for escaping and unescaping strings, with parameterized escape and
    escapeworthy characters.  Escaping/unescaping using this module is more efficient than
    using Pcre. Benchmark code can be found in core/benchmarks/string_escaping.ml. *)
module Escaping : sig
  (** [escape_gen_exn escapeworthy_map escape_char] returns a function that will escape a
      string [s] as follows: if [(c1,c2)] is in [escapeworthy_map], then all occurrences
      of [c1] are replaced by [escape_char] concatenated to [c2].

      Raises an exception if [escapeworthy_map] is not one-to-one.  If [escape_char] is
      not in [escapeworthy_map], then it will be escaped to itself.*)
  val escape_gen_exn
    :  escapeworthy_map:(char * char) list
    -> escape_char:char
    -> (string -> string) Staged.t

  val escape_gen
    :  escapeworthy_map:(char * char) list
    -> escape_char:char
    -> (string -> string) Or_error.t

  (** [escape ~escapeworthy ~escape_char s] is
      {[
        escape_gen_exn ~escapeworthy_map:(List.zip_exn escapeworthy escapeworthy)
          ~escape_char
      ]}
      Duplicates and [escape_char] will be removed from [escapeworthy].  So, no
      exception will be raised *)
  val escape : escapeworthy:char list -> escape_char:char -> (string -> string) Staged.t

  (** [unescape_gen_exn] is the inverse operation of [escape_gen_exn]. That is,
      {[
        let escape = Staged.unstage (escape_gen_exn ~escapeworthy_map ~escape_char) in
        let unescape = Staged.unstage (unescape_gen_exn ~escapeworthy_map ~escape_char) in
        assert (s = unescape (escape s))
      ]}
      always succeed when ~escapeworthy_map is not causing exceptions. *)
  val unescape_gen_exn
    :  escapeworthy_map:(char * char) list
    -> escape_char:char
    -> (string -> string) Staged.t

  val unescape_gen
    :  escapeworthy_map:(char * char) list
    -> escape_char:char
    -> (string -> string) Or_error.t

  (** [unescape ~escape_char] is defined as [unescape_gen_exn ~map:\[\] ~escape_char] *)
  val unescape : escape_char:char -> (string -> string) Staged.t

  (** Any char in an escaped string is either escaping, escaped, or literal. For example,
      for escaped string ["0_a0__0"] with [escape_char] as ['_'], pos 1 and 4 are
      escaping, 2 and 5 are escaped, and the rest are literal.

      [is_char_escaping s ~escape_char pos] returns true if the char at [pos] is escaping,
      false otherwise. *)
  val is_char_escaping : string -> escape_char:char -> int -> bool

  (** [is_char_escaped s ~escape_char pos] returns true if the char at [pos] is escaped,
      false otherwise. *)
  val is_char_escaped : string -> escape_char:char -> int -> bool

  (** [is_char_literal s ~escape_char pos] returns true if the char at [pos] is not
      escaped or escaping. *)
  val is_char_literal : string -> escape_char:char -> int -> bool

  (** [index s ~escape_char char] finds the first literal (not escaped) instance of [char]
      in s starting from 0. *)
  val index : string -> escape_char:char -> char -> int option

  val index_exn : string -> escape_char:char -> char -> int

  (** [rindex s ~escape_char char] finds the first literal (not escaped) instance of
      [char] in [s] starting from the end of [s] and proceeding towards 0. *)
  val rindex : string -> escape_char:char -> char -> int option

  val rindex_exn : string -> escape_char:char -> char -> int


  (** [index_from s ~escape_char pos char] finds the first literal (not escaped) instance
      of [char] in [s] starting from [pos] and proceeding towards the end of [s]. *)
  val index_from : string -> escape_char:char -> int -> char -> int option

  val index_from_exn : string -> escape_char:char -> int -> char -> int

  (** [rindex_from s ~escape_char pos char] finds the first literal (not escaped)
      instance of [char] in [s] starting from [pos] and towards 0. *)
  val rindex_from : string -> escape_char:char -> int -> char -> int option

  val rindex_from_exn : string -> escape_char:char -> int -> char -> int

  (** [split s ~escape_char ~on] returns a list of substrings of [s] that are separated by
      literal versions of [on].  Consecutive [on] characters will cause multiple empty
      strings in the result.  Splitting the empty string returns a list of the empty
      string, not the empty list.

      E.g., [split ~escape_char:'_' ~on:',' "foo,bar_,baz" = ["foo"; "bar_,baz"]]. *)
  val split : string -> on:char -> escape_char:char -> string list

  (** [split_on_chars s ~on] returns a list of all substrings of [s] that are separated by
      one of the literal chars from [on].  [on] are not grouped.  So a grouping of [on] in
      the source string will produce multiple empty string splits in the result.

      E.g., [split_on_chars ~escape_char:'_' ~on:[',';'|'] "foo_|bar,baz|0" ->
      ["foo_|bar"; "baz"; "0"]]. *)
  val split_on_chars : string -> on:char list -> escape_char:char -> string list


  (** [lsplit2 s ~on ~escape_char] splits s into a pair on the first literal instance of
      [on] (meaning the first unescaped instance) starting from the left. *)
  val lsplit2 : string -> on:char -> escape_char:char -> (string * string) option

  val lsplit2_exn : string -> on:char -> escape_char:char -> string * string

  (** [rsplit2 s ~on ~escape_char] splits [s] into a pair on the first literal
      instance of [on] (meaning the first unescaped instance) starting from the
      right. *)
  val rsplit2 : string -> on:char -> escape_char:char -> (string * string) option

  val rsplit2_exn : string -> on:char -> escape_char:char -> string * string

  (** These are the same as [lstrip], [rstrip], and [strip] for generic strings, except
      that they only drop literal characters -- they do not drop characters that are
      escaping or escaped.  This makes sense if you're trying to get rid of junk
      whitespace (for example), because escaped whitespace seems more likely to be
      deliberate and not junk. *)
  val lstrip_literal : ?drop:(char -> bool) -> t -> escape_char:char -> t

  val rstrip_literal : ?drop:(char -> bool) -> t -> escape_char:char -> t
  val strip_literal : ?drop:(char -> bool) -> t -> escape_char:char -> t
end
