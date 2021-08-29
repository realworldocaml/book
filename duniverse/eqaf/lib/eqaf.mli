(** Eqaf - constant time / timing side channel resistant functions *)

(** {1 Basics}

    In cryptography, a timing-attack is a side-channel attack in which the
   attacker attempts to compromise a cryptosystem by analyzing the time taken to
   execute cryptographic algorithms.

    In some cases, a process needs to compare two values (input value and
   expected password). An attacker can analyze time needed by
   {!String.compare}/{!String.equal} to calculate expected password.

    This side-channel attack is due implementation of
   {!String.compare}/{!String.equal} which leaves as soon as possible when it
   reachs a difference between [a] and [b]. By this way, time taken to compare
   two values differs if they are equal or not.

    Distribution provides a little example of this kind of attack where we
   construct step by step (byte per byte) expected value from time spended to
   execute {!Stdlib.compare}.

    Distribution wants to provide some functions which protect user against this
   kind of attack:

    {ul
    {- [equal] like {!String.equal}}
    {- [compare_be] like {!String.compare}}
    {- [compare_le] which is a {!String.compare} with a reverse operation on
   inputs}
    {- {!divmod} like {!Int32.unsigned_div} and {!Int32.unsigned_rem}}}

    These functions are tested to see how long they took to compare two equal
   values and two different values. See {i check} tool for more informations. *)

(** {1 Comparison functions} *)

(** {2 Equal} *)

val equal : string -> string -> bool
(** [equal a b] returns [true] if [a] and [b] are equals. [String.equal a b =
   equal a b] for any [a] and [b]. The execution time of [equal] depends solely
   on the length of the strings, not the contents. *)

(** {2 Big-endian comparison} *)

val compare_be : string -> string -> int
(** [compare_be a b] returns [0] if [a] is equal to [b], a negative integer if
   [a] if {i less} (lexicographically) than [b], and a positive integer if [a]
   is {i greater} (lexicographically) than [b].

    [compare_be a b] returns the same {i order} than [String.compare a b] for
   any [a] and [b] (but not necessary the same integer!). Order is defined as:

    {ul
    {- [compare_be a b < 0] means [a < b]}
    {- [compare_be a b > 0] means [a > b]}
    {- [compare_be a b = 0] means [a = b]}}

    About time, if [String.length a <> String.length b], [compare_be] does not
   look into [a] or [b] and no comparison in bytes will be done. *)

val compare_be_with_len : len:int -> string -> string -> int
(** [compare_be_with_len ~len a b] does {!compare_be}[ a b] on [len] bytes.

    @raise Invalid_argument if [len] is upper than [String.length a] or
   [String.length b]. *)

(** {2 Little-endian comparison} *)

val compare_le : string -> string -> int
(** [compare_le a b] is semantically [compare_be (rev a) (rev b)],
    where [rev] is a function that reverses a string bytewise
    ([a = rev (rev a)]). *)

val compare_le_with_len : len:int -> string -> string -> int
(** [compare_le_with_len a b] is semantically [compare_be_with_len ~len (rev a)
   (rev b)]. With [rev] reverse a string ([a = rev (rev a)]).

    @raise Invalid_argument if [len] is upper than [String.length a] or
   [String.length b]. *)

(** {1 Arithmetic} *)

(** {2 Division} *)

val divmod : x:int32 -> m:int32 -> int32 * int32
(** 32-bit unsigned division with remainder,
    constant-time with respect to [x] ({b not} [m]).

    @param x Dividend (number to be divided). {b Can be secret}.
    @param m Divisor  {b Must not be secret}. Must be [0 < m < 16384]

    This function is useful for implementation that need to produce e.g.
    pincodes from binary values in int32 format, example: {!ascii_of_int32}.

    @return [quotient, remainder]

    Example:
    {[
      let ct = Eqaf.divmod ~x ~m in
      let not_ct = Int32.unsigned_div x m, Int32.unsigned_rem x m in
      assert ct = not_ct ;
    ]}

    That is, an attacker might be able to learn [m] by measuring
    execution time, but not the value of [x].

    @raise Invalid_argument when [not (0 < m && m < 16384)].

    @see "supercop/crypto_kem/sntrup761/ref/uint32.c" Adapted from the NTRU Prime team's algorithm from [supercop/sntrup761], see round-2 NTRU Prime submission to NISTPQC (March 2019).
*)

(** {1:stringutil String utilities} *)

(** {2 String search}*)

val find_uint8 : ?off:int -> f:(int -> bool) -> string -> int
(** [find_uint8 ?off ~f v] returns the index of the first occurrence which
    respects the predicate [f] in string [v]. Otherwise, it returns [-1].
    The caller is responsible for ensuring that [~f] operates in constant time.
    The {!bool_of_int} function can be relevant when writing [~f] functions.
*)

val exists_uint8 : ?off:int -> f:(int -> bool) -> string -> bool
(** [exists_uint8 ?off ~f v] tests if an occurrence respects the predicate
    [f] in the string [v]. *)

(** {2:ascii ASCII functions}*)

val ascii_of_int32 : digits:int -> int32 -> string
(** [ascii_of_int64 ~digits ~n] is a string consisting of
    the rightmost [digits] characters of the decimal representation
    of [n].
    If [digits] is larger than the decimal representation, it is left-padded
    with ['0'].
    If [digits] is smaller, the output is truncated.

    Example:
    {[
      let s1 = ascii_of_int64 ~digits:6 ~n:12345678L in
      assert (s = "345678") ;
      let s2 = ascii_of_int64 ~digits:6 ~n:1234L in
      assert (s = "001234") ;
    ]}

    @raise Invalid_argument when [digits < 0]
*)

val lowercase_ascii : string -> string
(** [lowercase_ascii str] is [str] where [A-Z] is replaced with [a-z].
    It is a constant time implementation of {!String.lowercase_ascii}
*)

val uppercase_ascii : string -> string
(** [uppercase_ascii str] is [str] where [a-z] is replaced with [A-Z].
    It is a constant time implementation of {!String.uppercase_ascii}
*)

(** {2:hex Hex encoding and decoding}*)

val hex_of_bytes : bytes -> string
(** [hex_of_bytes raw] is [raw] hex-encoded in constant time.
    Can be used to serialize sensitive values.
    The {b contents can be secret}, but an attacker can learn the
    {b length of} [raw] by timing this function.

    Hex has two valid forms, lowercase and uppercase.
    This function produces lowercase hex characters exclusively.

    Example:
    {[
      let secret = "--Hi\x24" in
      let serialized = hex_of_string secret in
      (* serialized is now "2d2d486924" *)
    ]}

    @param raw is the source buffer. [raw] is not mutated by this function.
*)

val hex_of_string : string -> string
(** [hex_of_string raw] is [hex_of_bytes raw] as a {!string} *)

val bytes_of_hex : string -> bytes * int
(** [bytes_of_hex hex] is [raw, error] decoded in constant time.
    Can be used to e.g. decode secrets from configuration files.
    The {b contents can be secret}, but an attacker can learn
    the {b length of} [hex] by timing this function.

    {b Error handling:} The second tuple element [error] is {b non-zero}
    when the length of [hex] is not a multiple of 2,
    or [hex] contains invalid characters.
    Implementations should ensure that `error = 0` before using [raw].
    The function signals errors this way to allow implementations to handle
    invalid input errors in constant time.

    @param hex The hex-encoded octet string. Accepts characters [0-9 a-f A-F].
    Note that [0x] prefixes or whitespace are not accepted.

    Example:
    {[
      let serialized = "2d2d486924" in
      let secret, error = string_of_hex serialized in
      assert (error = 0);
      (* secret is now [--Hi$] *)
    ]}
*)

val string_of_hex : string -> string * int
(** [string_of_hex hex] is {!bytes_of_hex} [hex],
    but returning a {!type:string} instead of {!type:bytes}.
    See {!bytes_of_hex} regarding handling of invalid input errors.
*)

(** {1 Low-level primitives} *)

(** {2 Bithacks} *)

val one_if_not_zero : int -> int
(** [one_if_not_zero n] is a constant-time version of
    [if n <> 0 then 1 else 0]. This is functionally equivalent to [!!n] in the C
    programming language. *)

val zero_if_not_zero : int -> int
(** [zero_if_not_zero n] is a constant-time of
    [if n <> 0 then 0 else 1]. This is functionnaly equivalent to [!n] in the C
    programming language. *)

val int_of_bool : bool -> int
(** [int_of_bool b] is equivalent to [if b then 1 else 0].
    Internally it cast with [%identity] instead of branching.*)

val bool_of_int : int -> bool
(** [bool_of_int n] is equivalent to [if n = 0 then false else true]. *)

(** {2 Composition} *)

val select_int : int -> int -> int -> int
(** [select_int choose_b a b] is [a] if [choose_b = 0] and [b] otherwise.
    This comparison is constant-time and it should not be possible for a measuring
    adversary to determine anything about the values of [choose_b], [a], or [b]. *)

val select_a_if_in_range : low:int -> high:int ->
  n:int -> int -> int -> int
(** [select_a_if_in_range ~low ~high ~n a b]
    - is [a] if [low <= n <= high] (in range)
    - is [b] is [n < low || high < n] (out of range)

    This function {b only works for positive ranges}:
    @param low  invariant: [0 <= low <= max_int]
    @param high invariant: [low <= high <= max_int]

    This function can be used like {!select_int} but using an integer range
    instead of zero/non-zero to select.

    It operates in constant time and is safe to use with secret parameters for
    [low, high, n, a, b].

    Example:
    {[
      let a = 123 and b = 456 in
      let x = select_a_if_in_range ~low:10 ~high:20 ~n:10 a b in
      (* x = 123 *)
      let x = select_a_if_in_range ~low:10 ~high:20 ~n:0 a b in
      (* x = 456 *)
      let x = select_a_if_in_range ~low:10 ~high:20 ~n:20 a b in
      (* x = 123 *)
      let x = select_a_if_in_range ~low:10 ~high:20 ~n:21 a b in
      (* x = 456 *)

      (* Constant-time subpatterns can be expressed by nesting,
         Below is a constant time version of:
         match 3 with
         | 1 | 2| 3 | 4 ->
            begin match 3 with
            | 2 | 3 -> 111
            | _ -> 222
            end
         | _ -> 333

      *)
      let n = 3
      select_a_if_in_range ~low:1 ~high:4 ~n
        (select_a_if_in_range ~low:2 ~high:3 ~n
           (111)
           (222)
        )
        333
      (* evalutes to 111 because [1 <= n  <= 4] selects the inner pattern
         and [2 <= n <= 3] selects the first branch ("a") of the inner pattern.
      *)

      (* The applications can also be composed in constant time,
         note that all the branches are always evaluated, so large
         expressions can get slow:
      *)
      4
      |> fun n -> select_a_if_in_range ~low:1 ~high:5 ~n
         (n * 10)
         (n - 100)
      |> fun n -> select_a_if_in_range ~low:20 ~high 30 ~n
         (n * 100)
         (n+3)
      (* evaluates to [4 *10 + 3] *)

      (* Below the normal, non-constant time version: *)
      4
      |> (function
           | n when 1 <= n && n <= 5 -> n * 10
           | n -> n - 100)
      |> (function
          | n when 20 <= n && n <= 30 -> n * 100
          | n -> n + 3)
    ]}

    Another example of how to use this can be found in the implementations of
    {!lowercase_ascii} and {!bytes_of_hex}.
*)

