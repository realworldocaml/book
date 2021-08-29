(** Eqaf - timing-safe comparison functions

    {1 Basics}

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
   execute [compare].

    Distribution wants to provide some functions which protect user against this
   kind of attack:

    {ul
    {- [equal] like {!String.equal}}
    {- [compare_be] like {!String.compare}}
    {- [compare_le] which is a {!String.compare} with a reverse operation on
   inputs}}

    These functions are tested to see how long they took to compare two equal
   values and two different values. See {i check} tool for more informations. *)

(** {2 Implementations} *)

val equal : string -> string -> bool
(** [equal a b] returns [true] if [a] and [b] are equals. [String.equal a b =
   equal a b] for any [a] and [b]. The execution time of [equal] depends solely
   on the length of the strings, not the contents. *)

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
(** [compare_be_with_len ~len a b] does {!compare_be a b} on [len] bytes.

    @raise Invalid_argument if [len] is upper than [String.length a] or
   [String.length b]. *)

val compare_le : string -> string -> int
(** [compare_le a b] is semantically [compare_be (rev a) (rev b)]. With [rev]
   reverses a string ([a = rev (rev a)]). *)

val compare_le_with_len : len:int -> string -> string -> int
(** [compare_le_with_len a b] is semantically [compare_be_with_len ~len (rev a)
   (rev b)]. With [rev] reverse a string ([a = rev (rev a)]).

    @raise Invalid_argument if [len] is upper than [String.length a] or
   [String.length b]. *)

val one_if_not_zero : int -> int
(** [one_if_not_zero n] is a constant-time version of
    [if n <> 0 then 1 else 0]. This is functionally equivalent to [!!n] in the C
    programming language. *)

val zero_if_not_zero : int -> int
(** [zero_if_not_zero n] is a constant-time of
    [if n <> 0 then 0 else 1]. This is functionnaly equivalent to [!n] in the C
    programming language. *)

val select_int : int -> int -> int -> int
(** [select_int choose_b a b] is [a] if [choose_b = 0] and [b] otherwise.
    This comparison is constant-time and it should not be possible for a measuring
    adversary to determine anything about the values of [choose_b], [a], or [b]. *)

val find_uint8 : ?off:int -> f:(int -> bool) -> string -> int
(** [find_uint8 ?off ~f v] returns the index of the first occurrence which
    respects the predicate [f] in string [v]. Otherwise, it returns [-1]. *)

val exists_uint8 : ?off:int -> f:(int -> bool) -> string -> bool
(** [exists_uint8 ?off ~f v] tests if an occurrence respects the predicate
    [f] in the string [v]. *)
