(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Alternative [Char] and [String] modules.

    Open the module to use it. This defines {{!strf}one value} in your
    scope, redefines the [(^)] operator, the [Char] module and the [String]
    module.

    Consult the {{!diff}differences} with the OCaml
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html}[String]}
    module, the {{!port}porting guide} and a few {{!examples}examples}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 String} *)

val strf : ('a, Format.formatter, unit, string) format4 -> 'a
(** [strf] is {!Format.asprintf}. *)

val ( ^ ) : string -> string -> string
(** [s ^ s'] is {!String.append}. *)

(** Characters (bytes in fact).

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)
module Char : sig

  (** {1 Bytes} *)

  type t = char
  (** The type for bytes. *)

  val of_byte : int -> char
  (** [of_byte b] is a byte from [b].

      @raise Invalid_argument if [b] is  not in the range \[[0x00];[0xFF]\]. *)

  (**/**)
  val unsafe_of_byte : int -> char
  (**/**)

  val of_int : int -> char option
  (** [of_int b] is a byte from [b]. [None] is returned if [b] is not in the
      range \[[0x00];[0xFF]\]. *)

  val to_int : char -> int
  (** [to_int b] is the byte [b] as an integer. *)

  val hash : char -> int
  (** [hash] is {!Hashtbl.hash}. *)

  (** {1:pred Predicates} *)

  val equal : char -> char -> bool
  (** [equal b b'] is [b = b']. *)

  val compare : char -> char -> int
  (** [compare b b'] is {!Pervasives.compare}[ b b']. *)

  (** {1 Bytes as US-ASCII characters} *)

  (** US-ASCII character support

      The following functions act only on US-ASCII code points, that
      is on the bytes in range \[[0x00];[0x7F]\]. The functions can
      be safely used on UTF-8 encoded strings, they will of course
      only deal with US-ASCII related matters.

      {b References.}
      {ul
      {- Vint Cerf.
      {{:http://tools.ietf.org/html/rfc20}
      {e ASCII format for Network Interchange}}. RFC 20, 1969.}} *)
  module Ascii : sig

    (** {1 Predicates} *)

    val is_valid : char -> bool
    (** [is_valid c] is [true] iff [c] is an US-ASCII character,
        that is a byte in the range \[[0x00];[0x7F]\]. *)

    val is_digit : char -> bool
    (** [is_digit c] is [true] iff [c] is an US-ASCII digit
        ['0'] ... ['9'], that is a byte in the range \[[0x30];[0x39]\]. *)

    val is_hex_digit : char -> bool
    (** [is_hex_digit c] is [true] iff [c] is an US-ASCII hexadecimal
        digit ['0'] ... ['9'], ['a'] ... ['f'], ['A'] ... ['F'],
        that is a byte in one of the ranges \[[0x30];[0x39]\],
        \[[0x41];[0x46]\], \[[0x61];[0x66]\]. *)

    val is_upper : char -> bool
    (** [is_upper c] is [true] iff [c] is an US-ASCII uppercase
        letter ['A'] ... ['Z'], that is a byte in the range
        \[[0x41];[0x5A]\]. *)

    val is_lower : char -> bool
    (** [is_lower c] is [true] iff [c] is an US-ASCII lowercase
        letter ['a'] ... ['z'], that is a byte in the range
        \[[0x61];[0x7A]\]. *)

    val is_letter : char -> bool
    (** [is_letter c] is [is_lower c || is_upper c]. *)

    val is_alphanum : char -> bool
    (** [is_alphanum c] is [is_letter c || is_digit c]. *)

    val is_white : char -> bool
    (** [is_white c] is [true] iff [c] is an US-ASCII white space
        character, that is one of space [' '] ([0x20]), tab ['\t']
        ([0x09]), newline ['\n'] ([0x0A]), vertical tab ([0x0B]), form
        feed ([0x0C]), carriage return ['\r'] ([0x0D]). *)

    val is_blank : char -> bool
    (** [is_blank c] is [true] iff [c] is an US-ASCII blank character,
        that is either space [' '] ([0x20]) or tab ['\t'] ([0x09]). *)

    val is_graphic : char -> bool
    (** [is_graphic c] is [true] iff [c] is an US-ASCII graphic
        character that is a byte in the range \[[0x21];[0x7E]\]. *)

    val is_print : char -> bool
    (** [is_print c] is [is_graphic c || c = ' ']. *)

    val is_control : char -> bool
    (** [is_control c] is [true] iff [c] is an US-ASCII control character,
        that is a byte in the range \[[0x00];[0x1F]\] or [0x7F]. *)

    (** {1 Casing transforms} *)

    val uppercase : char -> char
    (** [uppercase c] is [c] with US-ASCII characters ['a'] to ['z'] mapped
        to ['A'] to ['Z']. *)

    val lowercase : char -> char
    (** [lowercase c] is [c] with US-ASCII characters ['A'] to ['Z'] mapped
        to ['a'] to ['z']. *)

    (** {1 Escaping to printable US-ASCII} *)

    val escape : char -> string
    (** [escape c] escapes [c] with:
        {ul
        {- ['\\'] ([0x5C]) escaped to the sequence ["\\\\"] ([0x5C],[0x5C]).}
        {- Any byte in the ranges \[[0x00];[0x1F]\] and
           \[[0x7F];[0xFF]\] escaped by an {e hexadecimal} ["\xHH"]
           escape with [H] a capital hexadecimal number. These bytes
           are the US-ASCII control characters and non US-ASCII bytes.}
        {- Any other byte is left unchanged.}}

        Use {!String.Ascii.unescape} to unescape. *)

    val escape_char : char -> string
    (** [escape_char c] is like {!escape} except is escapes [s] according
        to OCaml's lexical conventions for characters with:
        {ul
        {- ['\b'] ([0x08]) escaped to the sequence ["\\b"] ([0x5C,0x62]).}
        {- ['\t'] ([0x09]) escaped to the sequence ["\\t"] ([0x5C,0x74]).}
        {- ['\n'] ([0x0A]) escaped to the sequence ["\\n"] ([0x5C,0x6E]).}
        {- ['\r'] ([0x0D]) escaped to the sequence ["\\r"] ([0x5C,0x72]).}
        {- ['\\''] ([0x27]) escaped to the sequence ["\\'"] ([0x5C,0x27]).}
        {- Other bytes follow the rules of {!escape}}}

        Use {!String.Ascii.unescape_string} to unescape. *)
  end

  (** {1:pp Pretty printing} *)

  val pp : Format.formatter -> char -> unit
  (** [pp ppf c] prints [c] on [ppf]. *)

  val dump : Format.formatter -> char -> unit
  (** [dump ppf c] prints [c] as a syntactically valid OCaml
      char on [ppf] using {!Ascii.escape_char} *)
end

(** Strings, {{!Sub}substrings}, string {{!Set}sets} and {{!Map}maps}.

    A string [s] of length [l] is a zero-based indexed sequence of [l]
    bytes. An index [i] of [s] is an integer in the range
    \[[0];[l-1]\], it represents the [i]th byte of [s] which can be
    accessed using the string indexing operator [s.[i]].

    {b Important.} OCaml's [string]s became immutable since 4.02.
    Whenever possible compile your code with the [-safe-string]
    option. This module does not expose any mutable operation on
    strings and {b assumes} strings are immutable. See the
    {{!port}porting guide}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)
module String : sig

  (** {1 String} *)

  type t = string
  (** The type for strings. Finite sequences of immutable bytes. *)

  val empty : string
  (** [empty] is an empty string. *)

  val v : len:int -> (int -> char) -> string
  (** [v len f] is a string [s] of length [len] with [s.[i] = f
      i] for all indices [i] of [s]. [f] is invoked
      in increasing index order.

      @raise Invalid_argument if [len] is not in the range \[[0];
      {!Sys.max_string_length}\]. *)

  val length : string -> int
  (** [length s] is the number of bytes in [s]. *)

  val get : string -> int -> char
  (** [get s i] is the byte of [s]' at index [i]. This is
      equivalent to the [s.[i]] notation.

      @raise Invalid_argument if [i] is not an index of [s]. *)

  val get_byte : string -> int -> int
  (** [get_byte s i] is [Char.to_int (get s i)] *)

  (**/**)
  val unsafe_get : string -> int -> char
  val unsafe_get_byte : string -> int -> int
  (**/**)

  val head : ?rev:bool -> string -> char option
  (** [head s] is [Some (get s h)] with [h = 0] if [rev = false] (default) or
      [h = length s - 1] if [rev = true]. [None] is returned if [s] is
      empty. *)

  val get_head : ?rev:bool -> string -> char
  (** [get_head s] is like {!head} but @raise Invalid_argument if [s]
      is empty. *)

  val hash : string -> int
  (** [hash s] is {!Hashtbl.hash}[ s]. *)

  (** {1:append Appending strings} *)

  val append : string -> string -> string
  (** [append s s'] appends [s'] to [s]. This is equivalent to
      [s ^ s'].

      @raise Invalid_argument if the result is longer than
      {!Sys.max_string_length}. *)

  val concat : ?sep:string -> string list -> string
  (** [concat ~sep ss] concatenates the list of strings [ss], separating
      each consecutive elements in the list [ss] with [sep] (defaults to
      {!empty}).

      @raise Invalid_argument if the result is longer than
      {!Sys.max_string_length}. *)

  (** {1 Predicates} *)

  val is_empty : string -> bool
  (** [is_empty s] is [length s = 0]. *)

  val is_prefix : affix:string -> string -> bool
  (** [is_prefix ~affix s] is [true] iff [affix.[i] = s.[i]] for
      all indices [i] of [affix]. *)

  val is_infix : affix:string -> string -> bool
  (** [is_infix ~affix s] is [true] iff there exists an index [j] in [s] such
      that for all indices [i] of [affix] we have [affix.[i] = s.[j + i]]. *)

  val is_suffix : affix:string -> string -> bool
  (** [is_suffix ~affix s] is true iff [affix.[n - i] = s.[m - i]] for all
      indices [i] of [affix] with [n = String.length affix - 1] and [m =
      String.length s - 1]. *)

  val for_all : (char -> bool) -> string -> bool
  (** [for_all p s] is [true] iff for all indices [i] of [s], [p s.[i]
      = true]. *)

  val exists : (char -> bool) -> string -> bool
  (** [exists p s] is [true] iff there exists an index [i] of [s] with
      [p s.[i] = true]. *)

  val equal : string -> string -> bool
  (** [equal s s'] is [s = s']. *)

  val compare : string -> string -> int
  (** [compare s s'] is [Pervasives.compare s s'], it compares the
      byte sequences of [s] and [s'] in lexicographical order. *)

  (** {1:extract Extracting substrings}

      {b Tip.} These functions extract substrings as new strings. Using
      {{!Sub}substrings} may be less wasteful and more flexible. *)

  val with_range : ?first:int -> ?len:int -> string -> string
  (** [with_range ~first ~len s] are the consecutive bytes of [s] whose
      indices exist in the range \[[first];[first + len - 1]\].

      [first] defaults to [0] and [len] to [max_int]. Note that
      [first] can be any integer and [len] any positive integer.

      @raise Invalid_argument if [len] is negative. *)

  val with_index_range : ?first:int -> ?last:int -> string -> string
  (** [with_index_range ~first ~last s] are the consecutive bytes of
      [s] whose indices exist in the range \[[first];[last]\].

      [first] defaults to [0] and [last] to [String.length s - 1].

      Note that both [first] and [last] can be any integer. If
      [first > last] the interval is empty and the empty string
      is returned. *)

  val trim : ?drop:(char -> bool) -> string -> string
  (** [trim ~drop s] is [s] with prefix and suffix bytes satisfying
      [drop] in [s] removed. [drop] defaults to {!Char.Ascii.is_white}. *)

  val span : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) ->
    string -> (string * string)
  (** [span ~rev ~min ~max ~sat s] is [(l, r)] where:
        {ul
        {- if [rev] is [false] (default), [l] is at least [min]
           and at most [max] consecutive [sat] satisfying initial bytes of
           [s] or {!empty} if there are no such bytes. [r] are the remaining
           bytes of [s].}
        {- if [rev] is [true], [r] is at least [min] and at most [max]
           consecutive [sat] satisfying final bytes of [s] or {!empty}
           if there are no such bytes. [l] are the remaining
           the bytes of [s].}}
      If [max] is unspecified the span is unlimited. If [min]
      is unspecified it defaults to [0]. If [min > max] the condition
      can't be satisfied and the left or right span, depending on [rev], is
      always empty. [sat] defaults to [(fun _ -> true)].

      The invariant [l ^ r = s] holds.

      @raise Invalid_argument if [max] or [min] is negative.  *)

  val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) ->
    string -> string
  (** [take ~rev ~min ~max ~sat s] is the matching span of {!span} without
      the remaining one. In other words:
      {[(if rev then snd else fst) @@ span ~rev ~min ~max ~sat s]} *)

  val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) ->
    string -> string
  (** [drop ~rev ~min ~max ~sat s] is the remaining span of {!span} without
      the matching span. In other words:
      {[(if rev then fst else snd) @@ span ~rev ~min ~max ~sat s]} *)

  val cut : ?rev:bool -> sep:string -> string -> (string * string) option
  (** [cut ~sep s] is either the pair [Some (l,r)] of the two
      (possibly empty) substrings of [s] that are delimited by the
      first match of the non empty separator string [sep] or [None] if
      [sep] can't be matched in [s]. Matching starts from the
      beginning of [s] ([rev] is [false], default) or the end ([rev]
      is [true]).

      The invariant [l ^ sep ^ r = s] holds.

      @raise Invalid_argument if [sep] is the empty string. *)

  val cuts : ?rev:bool -> ?empty:bool -> sep:string -> string -> string list
  (** [cuts sep s] is the list of all substrings of [s] that are
      delimited by matches of the non empty separator string
      [sep]. Empty substrings are omitted in the list if [empty] is
      [false] (defaults to [true]).

      Matching separators in [s] starts from the beginning of [s]
      ([rev] is [false], default) or the end ([rev] is [true]). Once
      one is found, the separator is skipped and matching starts
      again, that is separator matches can't overlap. If there is no
      separator match in [s], the list [[s]] is returned.

      The following invariants hold:
      {ul
      {- [concat ~sep (cuts ~empty:true ~sep s) = s]}
      {- [cuts ~empty:true ~sep s <> []]}}

      @raise Invalid_argument if [sep] is the empty string. *)

  val fields : ?empty:bool -> ?is_sep:(char -> bool) -> string -> string list
  (** [fields ~empty ~is_sep s] is the list of (possibly empty)
      substrings that are delimited by bytes for which [is_sep] is
      [true].  Empty substrings are omitted in the list if [empty] is
      [false] (defaults to [true]). [is_sep] defaults to
      {!Char.Ascii.is_white}. *)

  (** {1:subs Substrings} *)

  type sub
  (** The type for {{!Sub}substrings}. *)

  val sub : ?start:int -> ?stop:int -> string -> sub
  (** [sub] is {!Sub.v}. *)

  val sub_with_range : ?first:int -> ?len:int -> string -> sub
  (** [sub_with_range] is like {!with_range} but returns a substring
      value. If [first] is smaller than [0] the empty string at the start
      of [s] is returned. If [first] is greater than the last index of [s]
      the empty string at the end of [s] is returned. *)

  val sub_with_index_range : ?first:int -> ?last:int -> string -> sub
  (** [sub_with_index_range] is like {!with_index_range} but returns
      a substring value. If [first] and [last] are smaller than [0]
      the empty string at the start of [s] is returned. If [first] and
      is greater than the last index of [s] the empty string at
      the end of [s] is returned. If [first > last] and [first] is an
      index of [s] the empty string at [first] is returned. *)

  (** Substrings.

      A substring defines a possibly empty subsequence of bytes in
      a {e base} string.

      The positions of a string [s] of length [l] are the slits found
      before each byte and after the last byte of the string. They
      are labelled from left to right by increasing number in the
      range \[[0];[l]\].
{v
positions  0   1   2   3   4    l-1    l
           +---+---+---+---+     +-----+
  indices  | 0 | 1 | 2 | 3 | ... | l-1 |
           +---+---+---+---+     +-----+
v}

      The [i]th byte index is between positions [i] and [i+1].

      Formally we define a substring of [s] as being a subsequence
      of bytes defined by a {e start} and a {e stop} position. The
      former is always smaller or equal to the latter. When both
      positions are equal the substring is {e empty}. Note that for a
      given base string there are as many empty substrings as there
      are positions in the string.

      Like in strings, we index the bytes of a substring using
      zero-based indices.

      See how to {{!examples}use} substrings to parse data. *)
  module Sub : sig

    (** {1 Substrings} *)

    type t = sub
    (** The type for substrings. *)

    val empty : sub
    (** [empty] is the empty substring of the empty string {!String.empty}. *)

    val v : ?start:int -> ?stop:int -> string -> sub
    (** [v ~start ~stop s] is the substring of [s] that starts
        at position [start] (defaults to [0]) and stops at position
        [stop] (defaults to [String.length s]).

        @raise Invalid_argument if [start] or [stop] are not positions of
        [s] or if [stop < start]. *)

    val start_pos : sub -> int
    (** [start_pos s] is [s]'s start position in the base string. *)

    val stop_pos : sub -> int
    (** [stop_pos s] is [s]'s stop position in the base string. *)

    val base_string : sub -> string
    (** [base_string s] is [s]'s base string. *)

    val length : sub -> int
    (** [length s] is the number of bytes in [s]. *)

    val get : sub -> int -> char
    (** [get s i] is the byte of [s] at its zero-based index [i].

        @raise Invalid_argument if [i] is not an index of [s]. *)

    val get_byte : sub -> int -> int
    (** [get_byte s i] is [Char.to_int (get s i)]. *)

    (**/**)
    val unsafe_get : sub -> int -> char
    val unsafe_get_byte : sub -> int -> int
    (**/**)

    val head : ?rev:bool -> sub -> char option
    (** [head s] is [Some (get s h)] with [h = 0] if [rev = false] (default) or
        [h = length s - 1] if [rev = true]. [None] is returned if [s] is
        empty. *)

    val get_head : ?rev:bool -> sub -> char
    (** [get_head s] is like {!head} but @raise Invalid_argument if [s]
        is empty. *)

    val of_string : string -> sub
    (** [of_string s] is [v s] *)

    val to_string : sub -> string
    (** [to_string s] is the bytes of [s] as a string. *)

    val rebase : sub -> sub
    (** [rebase s] is [v (to_string s)]. This puts [s] on a base
        string made solely of its bytes. *)

    val hash : sub -> int
    (** [hash s] is {!Hashtbl.hash s}. *)

    (** {1:stretch Stretching substrings}

        See the {{!fig}graphical guide}. *)

    val start : sub -> sub
    (** [start s] is the empty substring at the start position of [s]. *)

    val stop : sub -> sub
    (** [stop s] is the empty substring at the stop position of [s]. *)

    val base : sub -> sub
    (** [base s] is a substring that spans the whole base string of [s]. *)

    val tail : ?rev:bool -> sub -> sub
    (** [tail s] is [s] without its first ([rev] is [false], default)
        or last ([rev] is [true]) byte or [s] if it is empty. *)

    val extend : ?rev:bool -> ?max:int -> ?sat:(char -> bool) -> sub -> sub
    (** [extend ~rev ~max ~sat s] extends [s] by at most [max]
        consecutive [sat] satisfiying bytes of the base string located
        after [stop s] ([rev] is [false], default) or before [start s]
        ([rev] is [true]). If [max] is unspecified the extension is
        limited by the extents of the base string of [s].  [sat]
        defaults to [fun _ -> true].

        @raise Invalid_argument if [max] is negative. *)

    val reduce : ?rev:bool -> ?max:int -> ?sat:(char -> bool) -> sub -> sub
    (** [reduce ~rev ~max ~sat s] reduces [s] by at most [max]
        consecutive [sat] satisfying bytes of [s] located before [stop
        s] ([rev] is [false], default) or after [start s] ([rev] is
        [true]). If [max] is unspecified the reduction is limited by
        the extents of the substring [s]. [sat] defaults to [fun _ ->
        true].

        @raise Invalid_argument if [max] is negative. *)

    val extent : sub -> sub -> sub
    (** [extent s s'] is the smallest substring that includes all the
        positions of [s] and [s'].

        @raise Invalid_argument if [s] and [s'] are not on the same base
        string according to physical equality. *)

    val overlap : sub -> sub -> sub option
    (** [overlap s s'] is the smallest substring that includes all the
        positions common to [s] and [s'] or [None] if there are no
        such positions. Note that the overlap substring may be empty.

        @raise Invalid_argument if [s] and [s'] are not on the same base
        string according to physical equality. *)

    (** {1:append Appending substrings} *)

    val append : sub -> sub -> sub
    (** [append s s'] is like {!String.append}. The substrings can be
        on different bases and the result is on a base string that holds
        exactly the appended bytes. *)

    val concat : ?sep:sub -> sub list -> sub
    (** [concat ~sep ss] is like {!String.concat}. The substrings can
        all be on different bases and the result is on a base string that
        holds exactly the concatenated bytes. *)

    (** {1:pred Predicates} *)

    val is_empty : sub -> bool
    (** [is_empty s] is [length s = 0]. *)

    val is_prefix : affix:sub -> sub -> bool
    (** [is_prefix] is like {!String.is_prefix}. Only bytes
        are compared, [affix] can be on a different base string. *)

    val is_infix : affix:sub -> sub -> bool
    (** [is_infix] is like {!String.is_infix}. Only bytes
        are compared, [affix] can be on a different base string. *)

    val is_suffix : affix:sub -> sub -> bool
    (** [is_suffix] is like {!String.is_suffix}. Only bytes
        are compared, [affix] can be on a different base string. *)

    val for_all : (char -> bool) -> sub -> bool
    (** [for_all] is like {!String.for_all} on the substring. *)

    val exists : (char -> bool) -> sub -> bool
    (** [exists] is like {!String.exists} on the substring. *)

    val same_base : sub -> sub -> bool
    (** [same_base s s'] is [true] iff the substrings [s] and [s']
        have the same base string according to physical equality. *)

    val equal_bytes : sub -> sub -> bool
    (** [equal_bytes s s'] is [true] iff the substrings [s] and [s'] have
        exactly the same bytes. The substrings can be on a different
        base string. *)

    val compare_bytes : sub -> sub -> int
    (** [compare_bytes s s'] compares the bytes of [s] and [s]' in
        lexicographical order. The substrings can be on a different
        base string. *)

    val equal : sub -> sub -> bool
    (** [equal s s'] is [true] iff [s] and [s'] have the same positions.

        @raise Invalid_argument if [s] and [s'] are not on the same base
        string according to physical equality. *)

    val compare : sub -> sub -> int
    (** [compare s s'] compares the positions of [s] and [s'] in
        lexicographical order.

        @raise Invalid_argument if [s] and [s'] are not on the same base
        string according to physical equality. *)

    (** {1:extract Extracting substrings}

        Extracted substrings are always on the same base string as the
        substring [s] acted upon. *)

    val with_range : ?first:int -> ?len:int -> sub -> sub
    (** [with_range] is like {!String.sub_with_range}. The indices are the
        substring's zero-based ones, not those in the base string. *)

    val with_index_range : ?first:int -> ?last:int -> sub -> sub
    (** [with_index_range] is like {!String.sub_with_index_range}. The
        indices are the substring's zero-based ones, not those in the
        base string. *)

    val trim : ?drop:(char -> bool) -> sub -> sub
    (** [trim] is like {!String.trim}. If all bytes are dropped returns
        an empty string located in the middle of the argument. *)

    val span : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) ->
      sub -> (sub * sub)
    (** [span] is like {!String.span}. For a substring [s] a left
        empty span is [start s] and a right empty span is [stop s]. *)

    val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) ->
      sub -> sub
    (** [take] is like {!String.take}. *)

    val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) ->
      sub -> sub
    (** [drop] is like {!String.drop}. *)

    val cut : ?rev:bool -> sep:sub -> sub -> (sub * sub) option
    (** [cut] is like {!String.cut}. [sep] can be on a different base string *)

    val cuts : ?rev:bool -> ?empty:bool -> sep:sub -> sub -> sub list
    (** [cuts] is like {!String.cuts}. [sep] can be on a different base
        string *)

    val fields : ?empty:bool -> ?is_sep:(char -> bool) -> sub -> sub list
    (** [fields] is like {!String.fields}. *)

    (** {1:traverse Traversing substrings} *)

    val find : ?rev:bool -> (char -> bool) -> sub -> sub option
    (** [find ~rev sat s] is the substring of [s] (if any) that spans the
        first byte that satisfies [sat] in [s] after position [start s]
        ([rev] is [false], default) or before [stop s] ([rev] is [true]).
        [None] is returned if there is no matching byte in [s]. *)

    val find_sub :?rev:bool -> sub:sub -> sub -> sub option
    (** [find_sub ~rev ~sub s] is the substring of [s] (if any) that
        spans the first match of [sub] in [s] after position [start s]
        ([rev] is [false], defaults) or before [stop s] ([rev] is
        [false]). Only bytes are compared and [sub] can be on a
        different base string. [None] is returned if there is no match of
        [sub] in [s]. *)

    val filter : (char -> bool) -> sub -> sub
    (** [filter sat s] is like {!String.filter}. The result is on a
        base string that holds only the filtered bytes. *)

    val filter_map : (char -> char option) -> sub -> sub
    (** [filter_map f s] is like {!String.filter_map}. The result is on a
        base string that holds only the filtered bytes. *)

    val map : (char -> char) -> sub -> sub
    (** [map] is like {!String.map}. The result is on a base string that
        holds only the mapped bytes. *)

    val mapi : (int -> char -> char) -> sub -> sub
    (** [mapi] is like {!String.mapi}. The result is on a base string that
        holds only the mapped bytes. The indices are the substring's
        zero-based ones, not those in the base string. *)

    val fold_left : ('a -> char -> 'a) -> 'a -> sub -> 'a
    (** [fold_left] is like {!String.fold_left}. *)

    val fold_right : (char -> 'a -> 'a) -> sub -> 'a -> 'a
    (** [fold_right] is like {!String.fold_right}. *)

    val iter : (char -> unit) -> sub -> unit
    (** [iter] is like {!String.iter}. *)

    val iteri : (int -> char -> unit) -> sub -> unit
    (** [iteri] is like {!String.iteri}. The indices are the
        substring's zero-based ones, not those in the base string.  *)

    (** {1:pp Pretty printing} *)

    val pp : Format.formatter -> sub -> unit
    (** [pp ppf s] prints [s]'s bytes on [ppf]. *)

    val dump : Format.formatter -> sub -> unit
    (** [dump ppf s] prints [s] as a syntactically valid OCaml string
        on [ppf] using {!Ascii.escape_string}. *)

    val dump_raw : Format.formatter -> sub -> unit
    (** [dump_raw ppf s] prints an unspecified raw internal
        representation of [s] on ppf. *)

    (** {1:convert OCaml base type conversions} *)

    val of_char : char -> sub
    (** [of_char c] is a string that contains the byte [c]. *)

    val to_char : sub -> char option
    (** [to_char s] is the single byte in [s] or [None] if there is no byte
        or more than one in [s]. *)

    val of_bool : bool -> sub
    (** [of_bool b] is a string representation for [b]. Relies on
        {!Pervasives.string_of_bool}. *)

    val to_bool : sub -> bool option
    (** [to_bool s] is a [bool] from [s], if any. Relies on
        {!Pervasives.bool_of_string}. *)

    val of_int : int -> sub
    (** [of_int i] is a string representation for [i]. Relies on
        {!Pervasives.string_of_int}. *)

    val to_int : sub -> int option
    (** [to_int] is an [int] from [s], if any. Relies on
        {!Pervasives.int_of_string}. *)

    val of_nativeint : nativeint -> sub
    (** [of_nativeint i] is a string representation for [i]. Relies on
        {!Nativeint.of_string}. *)

    val to_nativeint : sub -> nativeint option
    (** [to_nativeint] is an [nativeint] from [s], if any. Relies on
        {!Nativeint.to_string}. *)

    val of_int32 : int32 -> sub
    (** [of_int32 i] is a string representation for [i]. Relies on
        {!Int32.of_string}. *)

    val to_int32 : sub -> int32 option
    (** [to_int32] is an [int32] from [s], if any. Relies on
        {!Int32.to_string}. *)

    val of_int64 : int64 -> sub
    (** [of_int64 i] is a string representation for [i]. Relies on
        {!Int64.of_string}. *)

    val to_int64 : sub -> int64 option
    (** [to_int64] is an [int64] from [s], if any. Relies on
        {!Int64.to_string}. *)

    val of_float : float -> sub
    (** [of_float f] is a string representation for [f]. Relies on
        {!Pervasives.string_of_float}. *)

    val to_float : sub -> float option
    (** [to_float s] is a [float] from [s], if any. Relies
        on {!Pervasives.float_of_string}. *)

    (** {1:fig Substring stretching graphical guide}

{v
+---+---+---+---+---+---+---+---+---+---+---+
| R | e | v | o | l | t |   | n | o | w | ! |
+---+---+---+---+---+---+---+---+---+---+---+
        |---------------|                      a
        |                                      start a
                        |                      stop a
            |-----------|                      tail a
        |-----------|                          tail ~rev:true a
        |-----------------------------------|  extend a
|-----------------------|                      extend ~rev:true a
|-------------------------------------------|  base a
|-----------|                                  b
|                                              start b
            |                                  stop b
    |-------|                                  tail b
|-------|                                      tail ~rev:true b
|-------------------------------------------|  extend b
|-----------|                                  extend ~rev:true b
|-------------------------------------------|  base b
|-----------------------|                      extent a b
        |---|                                  overlap a b
                            |                  c
                            |                  start c
                            |                  stop c
                            |                  tail c
                            |                  tail ~rev:true c
                            |---------------|  extend c
|---------------------------|                  extend ~rev:true c
|-------------------------------------------|  base c
        |-------------------|                  extent a c
                                         None  overlap a c
                            |---------------|  d
                            |                  start d
                                            |  stop d
                                |-----------|  tail d
                            |-----------|      tail ~rev:true d
                            |---------------|  extend d
|-------------------------------------------|  extend ~rev:true d
|-------------------------------------------|  base d
                            |---------------|  extent d c
                            |                  overlap d c
v} *)
  end

  (** {1:traverse Traversing strings} *)

  val find : ?rev:bool -> ?start:int -> (char -> bool) -> string -> int option
  (** [find ~rev ~start sat s] is:
      {ul
      {- If [rev] is [false] (default). The smallest index [i], if any,
         greater or equal to [start] such that [sat s.[i]] is [true].
         [start] defaults to [0].}
      {- If [rev] is [true]. The greatest index [i], if any, smaller or equal
         to [start] such that [sat s.[i]] is [true].
         [start] defaults to [String.length s - 1].}}
      Note that [start] can be any integer. *)

  val find_sub :?rev:bool -> ?start:int -> sub:string -> string -> int option
  (** [find_sub ~rev ~start ~sub s] is:
      {ul
      {- If [rev] is [false] (default). The smallest index [i], if any,
         greater or equal to [start] such that [sub] can be found starting
         at [i] in [s] that is [s.[i] = sub.[0]], [s.[i+1] = sub.[1]], ...
         [start] defaults to [0].}
      {- If [rev] is [true]. The greatest index [i], if any, smaller
         or equal to [start] such that [sub] can be found starting at
         [i] in [s] that is [s.[i] = sub.[0]], [s.[i+1] = sub.[1]], ...
         [start] defaults to [String.length s - 1].}}
      Note that [start] can be any integer. *)

  val filter : (char -> bool) -> string -> string
  (** [filter sat s] is the string made of the bytes of [s] that satisfy [sat],
      in the same order. *)

  val filter_map : (char -> char option) -> string -> string
  (** [filter_map f s] is the string made of the bytes of [s] as mapped by
      [f], in the same order. *)

  val map : (char -> char) -> string -> string
  (** [map f s] is [s'] with [s'.[i] = f s.[i]] for all indices [i]
      of [s]. [f] is invoked in increasing index order. *)

  val mapi : (int -> char -> char) -> string -> string
  (** [mapi f s] is [s'] with [s'.[i] = f i s.[i]] for all indices [i]
      of [s]. [f] is invoked in increasing index order. *)

  val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
  (** [fold_left f acc s] is
      [f (]...[(f (f acc s.[0]) s.[1])]...[) s.[m]]
      with [m = String.length s - 1]. *)

  val fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a
  (** [fold_right f s acc] is
      [f s.[0] (f s.[1] (]...[(f s.[m] acc) )]...[)]
      with [m = String.length s - 1]. *)

  val iter : (char -> unit) -> string -> unit
  (** [iter f s] is [f s.[0]; f s.[1];] ...
      [f s.[m]] with [m = String.length s - 1]. *)

  val iteri : (int -> char -> unit) -> string -> unit
  (** [iteri f s] is [f 0 s.[0]; f 1 s.[1];] ...
      [f m s.[m]] with [m = String.length s - 1]. *)

  (** {1:unique Uniqueness} *)

  val uniquify : string list -> string list
  (** [uniquify ss] is [ss] without duplicates, the list order is
      preserved. *)

  (** {1:ascii Strings as US-ASCII character sequences} *)

  (** US-ASCII string support.

      {b References.}
      {ul
      {- Vint Cerf.
      {{:http://tools.ietf.org/html/rfc20}
      {e ASCII format for Network Interchange}}. RFC 20, 1969.}} *)
  module Ascii : sig

    (** {1:pred Predicates} *)

    val is_valid : string -> bool
    (** [is_valid s] is [true] iff only for all indices [i] of [s],
        [s.[i]] is an US-ASCII character, i.e. a byte in the range
        \[[0x00];[0x1F]\]. *)

    (** {1:case Casing transforms}

        The following functions act only on US-ASCII code points that
        is on bytes in range \[[0x00];[0x7F]\], leaving any other byte
        intact. The functions can be safely used on UTF-8 encoded
        strings; they will of course only deal with US-ASCII
        casings. *)

    val uppercase : string -> string
    (** [uppercase s] is [s] with US-ASCII characters ['a'] to ['z'] mapped
        to ['A'] to ['Z']. *)

    val lowercase : string -> string
    (** [lowercase s] is [s] with US-ASCII characters ['A'] to ['Z'] mapped
        to ['a'] to ['z']. *)

    val capitalize : string -> string
    (** [capitalize s] is like {!uppercase} but performs the map only
        on [s.[0]]. *)

    val uncapitalize : string -> string
    (** [uncapitalize s] is like {!lowercase} but performs the map only
        on [s.[0]]. *)

    (** {1:esc Escaping to printable US-ASCII} *)

    val escape : string -> string
    (** [escape s] is [s] with:
        {ul
        {- Any ['\\'] ([0x5C]) escaped to the sequence
           ["\\\\"] ([0x5C],[0x5C]).}
        {- Any byte in the ranges \[[0x00];[0x1F]\] and
           \[[0x7F];[0xFF]\] escaped by an {e hexadecimal} ["\xHH"]
           escape with [H] a capital hexadecimal number. These bytes
           are the US-ASCII control characters and non US-ASCII bytes.}
        {- Any other byte is left unchanged.}} *)

    val unescape : string -> string option
    (** [unescape s] unescapes what {!escape} did. The letters of hex
        escapes can be upper, lower or mixed case, and any two letter
        hex escape is decoded to its corresponding byte. Any other
        escape not defined by {!escape} or truncated escape makes the
        function return [None].

        The invariant [unescape (escape s) = Some s] holds. *)

    val escape_string : string -> string
    (** [escape_string s] is like {!escape} except it escapes [s]
        according to OCaml's lexical conventions for strings with:
        {ul
        {- Any ['\b'] ([0x08]) escaped to the sequence ["\\b"] ([0x5C,0x62]).}
        {- Any ['\t'] ([0x09]) escaped to the sequence ["\\t"] ([0x5C,0x74]).}
        {- Any ['\n'] ([0x0A]) escaped to the sequence ["\\n"] ([0x5C,0x6E]).}
        {- Any ['\r'] ([0x0D]) escaped to the sequence ["\\r"] ([0x5C,0x72]).}
        {- Any ['\"'] ([0x22]) escaped to the sequence ["\\\""] ([0x5C,0x22]).}
        {- Any other byte follows the rules of {!escape}}} *)

    val unescape_string : string -> string option
    (** [unescape_string] is to {!escape_string} what {!unescape}
        is to {!escape} and also additionally unescapes
        the sequence ["\\'"] ([0x5C,0x27]) to ["'"] ([0x27]). *)
  end

  (** {1:pp Pretty printing} *)

  val pp : Format.formatter -> string -> unit
  (** [pp ppf s] prints [s]'s bytes on [ppf]. *)

  val dump : Format.formatter -> string -> unit
  (** [dump ppf s] prints [s] as a syntactically valid OCaml string on
      [ppf] using {!Ascii.escape_string}. *)

  (** {1 String sets and maps} *)

  type set
  (** The type for string sets. *)

  (** String sets. *)
  module Set : sig

    (** {1 String sets} *)

    include Set.S with type elt := string
                   and type t := set

    type t = set

    val min_elt : set -> string option
    (** Exception safe {!Set.S.min_elt}. *)

    val get_min_elt : set -> string
    (** [get_min_elt] is like {!min_elt} but @raise Invalid_argument
        on the empty set. *)

    val max_elt : set -> string option
    (** Exception safe {!Set.S.max_elt}. *)

    val get_max_elt : set -> string
    (** [get_max_elt] is like {!max_elt} but @raise Invalid_argument
        on the empty set. *)

    val choose : set -> string option
    (** Exception safe {!Set.S.choose}. *)

    val get_any_elt : set -> string
    (** [get_any_elt] is like {!choose} but @raise Invalid_argument on the
        empty set. *)

    val find : string -> set -> string option
    (** Exception safe {!Set.S.find}. *)

    val get : string -> set -> string
    (** [get] is like {!Set.S.find} but @raise Invalid_argument if
        [elt] is not in [s]. *)

    val of_list : string list -> set
    (** [of_list ss] is a set from the list [ss]. *)

    val pp : ?sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> string -> unit) ->
        Format.formatter -> set -> unit
    (** [pp ~sep pp_elt ppf ss] formats the elements of [ss] on
        [ppf]. Each element is formatted with [pp_elt] and elements
        are separated by [~sep] (defaults to
        {!Format.pp_print_cut}. If the set is empty leaves [ppf]
        untouched. *)

    val dump : Format.formatter -> set -> unit
    (** [dump ppf ss] prints an unspecified representation of [ss] on
        [ppf]. *)
  end

  type +'a map
  (** The type for maps from strings to values of type 'a. *)

  (** String maps. *)
  module Map : sig

    (** {1 String maps} *)

    include Map.S with type key := string
                   and type 'a t := 'a map

    type 'a t = 'a map

    val min_binding : 'a map -> (string * 'a) option
    (** Exception safe {!Map.S.min_binding}. *)

    val get_min_binding : 'a map -> (string * 'a)
    (** [get_min_binding] is like {!min_binding} but @raise Invalid_argument
        on the empty map. *)

    val max_binding : 'a map -> (string * 'a) option
    (** Exception safe {!Map.S.max_binding}. *)

    val get_max_binding : 'a map -> string * 'a
    (** [get_max_binding] is like {!max_binding} but @raise Invalid_argument
        on the empty map. *)

    val choose : 'a map -> (string * 'a) option
    (** Exception safe {!Map.S.choose}. *)

    val get_any_binding : 'a map -> (string * 'a)
    (** [get_any_binding] is like {!choose} but @raise Invalid_argument
        on the empty map. *)

    val find : string -> 'a map -> 'a option
    (** Exception safe {!Map.S.find}. *)

    val get : string -> 'a map -> 'a
    (** [get k m] is like {!Map.S.find} but raises [Invalid_argument] if
        [k] is not bound in [m]. *)

    val dom : 'a map -> set
    (** [dom m] is the domain of [m]. *)

    val of_list : (string * 'a) list -> 'a map
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    val pp : ?sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> string * 'a -> unit) -> Format.formatter ->
      'a map -> unit
    (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
        [ppf]. Each binding is formatted with [pp_binding] and
        bindings are separated by [sep] (defaults to
        {!Format.pp_print_cut}). If the map is empty leaves [ppf]
        untouched. *)

    val dump : (Format.formatter -> 'a -> unit) -> Format.formatter ->
      'a map -> unit
    (** [dump pp_v ppf m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to print the map codomain elements. *)

    val dump_string_map : Format.formatter -> string map -> unit
    (** [dump_string_map ppf m] prints an unspecified representation of the
        string map [m] on [ppf]. *)
  end

  (** {1:convert OCaml base type conversions} *)

  val of_char : char -> string
  (** [of_char c] is a string that contains the byte [c]. *)

  val to_char : string -> char option
  (** [to_char s] is the single byte in [s] or [None] if there is no byte
      or more than one in [s]. *)

  val of_bool : bool -> string
  (** [of_bool b] is a string representation for [b]. Relies on
      {!Pervasives.string_of_bool}. *)

  val to_bool : string -> bool option
  (** [to_bool s] is a [bool] from [s], if any. Relies on
      {!Pervasives.bool_of_string}. *)

  val of_int : int -> string
  (** [of_int i] is a string representation for [i]. Relies on
      {!Pervasives.string_of_int}. *)

  val to_int : string -> int option
  (** [to_int] is an [int] from [s], if any. Relies on
      {!Pervasives.int_of_string}. *)

  val of_nativeint : nativeint -> string
  (** [of_nativeint i] is a string representation for [i]. Relies on
      {!Nativeint.of_string}. *)

  val to_nativeint : string -> nativeint option
  (** [to_nativeint] is an [nativeint] from [s], if any. Relies on
      {!Nativeint.to_string}. *)

  val of_int32 : int32 -> string
  (** [of_int32 i] is a string representation for [i]. Relies on
      {!Int32.of_string}. *)

  val to_int32 : string -> int32 option
  (** [to_int32] is an [int32] from [s], if any. Relies on
      {!Int32.to_string}. *)

  val of_int64 : int64 -> string
  (** [of_int64 i] is a string representation for [i]. Relies on
      {!Int64.of_string}. *)

  val to_int64 : string -> int64 option
  (** [to_int64] is an [int64] from [s], if any. Relies on
      {!Int64.to_string}. *)

  val of_float : float -> string
  (** [of_float f] is a string representation for [f]. Relies on
      {!Pervasives.string_of_float}. *)

  val to_float : string -> float option
  (** [to_float s] is a [float] from [s], if any. Relies
      on {!Pervasives.float_of_string}. *)
end

(** {1:diff Differences with the OCaml [String] module}

    First note that it is not a goal of {!Astring} to maintain
    compatibility with the OCaml
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html}
    [String]} module.

    In [Astring]:
    {ul
    {- Strings are assumed to be immutable.}
    {- Deprecated functions are not included.}
    {- Some rarely used functions are dropped, some signatures and names
       are altered, a few often needed functions are added.}
    {- Scanning functions are not doubled for supporting forward and
       reverse directions. Both directions are supported via a single
       function and an optional [rev] argument.}
    {- Functions do not raise [Not_found]. They return [option] values
       instead.}
    {- Functions escaping bytes to printable US-ASCII characters use
       capital hexadecimal escapes rather than decimal ones.}
    {- US-ASCII string support is collected in the {!Char.Ascii} and
       {!String.Ascii} submodules.  The functions make sure to operate
       only on the US-ASCII code points (rather than
       {{:http://www.ecma-international.org/publications/standards/Ecma-094.htm}ISO/IEC
       8859-1} code points). This means they can safely be used on
       UTF-8 encoded strings, they will of course only deal with the
       US-ASCII subset U+0000 to U+001F of
       {{:http://unicode.org/glossary/#unicode_scalar_value} Unicode
       scalar values}.}
     {- The module has pre-applied exception safe {!String.Set}
        and {!String.Map} submodules.}}

    {1:port Porting guide}

    Opening [Astring] at the top of a module that uses the OCaml
    standard library in a project that compiles with [-safe-string]
    will either result in typing errors or compatible behaviour except
    for uses of the {!String.trim} function, {{!porttrim}see below}.

    If for some reason you can't compile your project with
    [-safe-string] this {b may} not be a problem. However you have to
    make sure that your code does not depend on fresh strings being
    returned by functions of the [String] module. The functions of
    {!Astring.String} assume strings to be immutable and thus do not
    always allocate fresh strings for their results. This is the case
    for example for the {!( ^ )} operator redefinition: no string is
    allocated whenever one of its arguments is an empty string. That
    being said it is still better to first make your project compile
    with [-safe-string] and then port to [Astring].

    The
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html#VALsub}[String.sub]} function
    is renamed to {!String.with_range}. If you are working with
    {!String.find} you may find it easier to use
    {!String.with_index_range} which takes indices as arguments and is thus
    directly usable with the result of {!String.find}. But in general
    index based string processing should be frown upon and replaced
    by {{!String.extract} substring extraction} combinators.

    {2:porttrim Porting [String.trim] usages}

    The standard OCaml [String.trim] function only trims the
    characters [' '], ['\t'], ['\n'], ['\012'], ['\r']. In
    [Astring] the {{!Char.Ascii.is_white}default set} adds
    vertical tab ([0x0B]) to the set to match the behaviour of
    the C [isspace(3)] function.

    If you want to preserve the behaviour of the original function you
    can replace any use of [String.trim] with the following
    [std_ocaml_trim] function:
{[
let std_ocaml_trim s =
  let drop = function
  | ' ' | '\n' | '\012' | '\r' | '\t' -> true
  | _ -> false
  in
  String.trim ~drop s
]}

   {1:examples Examples}

   We show how to use {{!String.Sub}substrings} to quickly devise LL(1)
   parsers. To keep it simple we do not implement precise error
   report, but note that it would be easy to add it by replacing the
   [raise Exit] calls by an exception with more information: we have
   everything at hand at these call points to report good error
   messages.

   The first example parses version numbers structured as follows:
{[
[v|V]major.minor[.patch][(+|-)info]
]}
   an unreadable {!Str} regular expression for this would be:
{[
  "[vV]?\\([0-9]+\\)\\.\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?\\([+-]\\(.*\\)\\)?"
]}
Using substrings is certainly less terse but note that the parser is
made of reusable sub-functions.
{[
let parse_version : string -> (int * int * int * string option) option =
fun s -> try
  let parse_opt_v s = match String.Sub.head s with
  | Some ('v'|'V') -> String.Sub.tail s
  | Some _ -> s
  | None -> raise Exit
  in
  let parse_dot s = match String.Sub.head s with
  | Some '.' -> String.Sub.tail s
  | Some _ | None -> raise Exit
  in
  let parse_int s =
    match String.Sub.span ~min:1 ~sat:Char.Ascii.is_digit s with
    | (i, _) when String.Sub.is_empty i -> raise Exit
    | (i, s) ->
        match String.Sub.to_int i with
        | None -> raise Exit | Some i -> i, s
  in
  let maj, s = parse_int (parse_opt_v (String.sub s)) in
  let min, s = parse_int (parse_dot s) in
  let patch, s = match String.Sub.head s with
  | Some '.' -> parse_int (parse_dot s)
  | _ -> 0, s
  in
  let info = match String.Sub.head s with
  | Some ('+' | '-') -> Some (String.Sub.(to_string (tail s)))
  | Some _ -> raise Exit
  | None -> None
  in
  Some (maj, min, patch, info)
with Exit -> None
]}

The second example parses space separated key-value bindings
environments of the form:
{[
key0 = value0 key2 = value2 ...]}
To support values with spaces, values can be quoted between two
['"'] characters. If they are quoted then any ["\\\""] subsequence
([0x2F],[0x22]) is interpreted as the character ['"'] ([0x22]) and
["\\\\"] ([0x2F],[0x2F]) is interpreted as the character ['\\']
([0x22]).

{[
let parse_env : string -> string String.map option =
fun s -> try
  let skip_white s = String.Sub.drop ~sat:Char.Ascii.is_white s in
  let parse_key s =
    let id_char c = Char.Ascii.is_letter c || c = '_' in
    match String.Sub.span ~min:1 ~sat:id_char s with
    | (key, _) when String.Sub.is_empty key -> raise Exit
    | (key, rem) -> (String.Sub.to_string key), rem
  in
  let parse_eq s = match String.Sub.head s with
  | Some '=' -> String.Sub.tail s
  | Some _ | None -> raise Exit
  in
  let parse_value s = match String.Sub.head s with
  | Some '"' -> (* quoted *)
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.Sub.span ~sat:is_data s in
        match String.Sub.head rem with
        | Some '"' ->
            let acc = List.rev (data :: acc) in
            String.Sub.(to_string @@ concat acc), (String.Sub.tail rem)
        | Some '\\' ->
            let rem = String.Sub.tail rem in
            begin match String.Sub.head rem with
            | Some ('"' | '\\' as c) ->
                let acc = String.(sub (of_char c)) :: data :: acc in
                loop acc (String.Sub.tail rem)
            | Some _ | None -> raise Exit
            end
        | None | Some _ -> raise Exit
      in
      loop [] (String.Sub.tail s)
  | Some _ ->
      let is_data c = not (Char.Ascii.is_white c) in
      let data, rem = String.Sub.span ~sat:is_data s in
      String.Sub.to_string data, rem
  | None -> "", s
  in
  let rec parse_bindings acc s =
    if String.Sub.is_empty s then acc else
    let key, s = parse_key s in
    let value, s = s |> skip_white |> parse_eq |> skip_white |> parse_value in
    parse_bindings (String.Map.add key value acc) (skip_white s)
  in
  Some (String.sub s |> skip_white |> parse_bindings String.Map.empty)
with Exit -> None
]}

*)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
