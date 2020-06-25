(** Functions for formatted output.

    [fprintf] and related functions format their arguments according to the given format
    string. The format string is a character string which contains two types of objects:
    plain characters, which are simply copied to the output channel, and conversion
    specifications, each of which causes conversion and printing of arguments.

    Conversion specifications have the following form:

    {[% [flags] [width] [.precision] type]}

    In short, a conversion specification consists in the [%] character, followed by
    optional modifiers and a type which is made of one or two characters.

    The types and their meanings are:

    - [d], [i]: convert an integer argument to signed decimal.
    - [u], [n], [l], [L], or [N]: convert an integer argument to unsigned
      decimal. Warning: [n], [l], [L], and [N] are used for [scanf], and should not be used
      for [printf].
    - [x]: convert an integer argument to unsigned hexadecimal, using lowercase letters.
    - [X]: convert an integer argument to unsigned hexadecimal, using uppercase letters.
    - [o]: convert an integer argument to unsigned octal.
    - [s]: insert a string argument.
    - [S]: convert a string argument to OCaml syntax (double quotes, escapes).
    - [c]: insert a character argument.
    - [C]: convert a character argument to OCaml syntax (single quotes, escapes).
    - [f]: convert a floating-point argument to decimal notation, in the style [dddd.ddd].
    - [F]: convert a floating-point argument to OCaml syntax ([dddd.] or [dddd.ddd] or
      [d.ddd e+-dd]).
    - [e] or [E]: convert a floating-point argument to decimal notation, in the style
      [d.ddd e+-dd] (mantissa and exponent).
    - [g] or [G]: convert a floating-point argument to decimal notation, in style [f] or
      [e], [E] (whichever is more compact). Moreover, any trailing zeros are removed from
      the fractional part of the result and the decimal-point character is removed if there
      is no fractional part remaining.
    - [h] or [H]: convert a floating-point argument to hexadecimal notation, in the style
      [0xh.hhhh e+-dd] (hexadecimal mantissa, exponent in decimal and denotes a power of 2).
    - [B]: convert a boolean argument to the string true or false
    - [b]: convert a boolean argument (deprecated; do not use in new programs).
    - [ld], [li], [lu], [lx], [lX], [lo]: convert an int32 argument to the format
      specified by the second letter (decimal, hexadecimal, etc).
    - [nd], [ni], [nu], [nx], [nX], [no]: convert a nativeint argument to the format
      specified by the second letter.
    - [Ld], [Li], [Lu], [Lx], [LX], [Lo]: convert an int64 argument to the format
      specified by the second letter.
    - [a]: user-defined printer. Take two arguments and apply the first one to outchan
      (the current output channel) and to the second argument. The first argument must
      therefore have type [out_channel -> 'b -> unit] and the second ['b]. The output
      produced by the function is inserted in the output of [fprintf] at the current point.
    - [t]: same as [%a], but take only one argument (with type [out_channel -> unit]) and
      apply it to [outchan].
    - [{ fmt %}]: convert a format string argument to its type digest. The argument must
      have the same type as the internal format string [fmt].
    - [( fmt %)]: format string substitution. Take a format string argument and substitute
      it to the internal format string fmt to print following arguments. The argument must
      have the same type as the internal format string fmt.
    - [!]: take no argument and flush the output.
    - [%]: take no argument and output one [%] character.
    - [@]: take no argument and output one [@] character.
    - [,]: take no argument and output nothing: a no-op delimiter for conversion
      specifications.

    The optional [flags] are:

    - [-]: left-justify the output (default is right justification).
    - [0]: for numerical conversions, pad with zeroes instead of spaces.
    - [+]: for signed numerical conversions, prefix number with a [+] sign if positive.
    - space: for signed numerical conversions, prefix number with a space if positive.
    - [#]: request an alternate formatting style for the hexadecimal and octal integer
      types ([x], [X], [o], [lx], [lX], [lo], [Lx], [LX], [Lo]).

    The optional [width] is an integer indicating the minimal width of the result. For
    instance, [%6d] prints an integer, prefixing it with spaces to fill at least 6
    characters.

    The optional [precision] is a dot [.] followed by an integer indicating how many
    digits follow the decimal point in the [%f], [%e], and [%E] conversions. For instance,
    [%.4f] prints a [float] with 4 fractional digits.

    The integer in a [width] or [precision] can also be specified as [*], in which case an
    extra integer argument is taken to specify the corresponding [width] or
    [precision]. This integer argument precedes immediately the argument to print. For
    instance, [%.*f] prints a float with as many fractional digits as the value of the
    argument given before the float.
*)

open! Import0

(** Same as [fprintf], but does not print anything. Useful for ignoring some material when
    conditionally printing. *)
val ifprintf : 'a -> ('r, 'a, 'c, unit) format4 -> 'r

(** Same as [fprintf], but instead of printing on an output channel, returns a string. *)
val sprintf : ('r, unit, string) format -> 'r

(** Same as [fprintf], but instead of printing on an output channel, appends the formatted
    arguments to the given extensible buffer. *)
val bprintf : Caml.Buffer.t -> ('r, Caml.Buffer.t, unit) format -> 'r

(** Same as [sprintf], but instead of returning the string, passes it to the first
    argument. *)
val ksprintf : (string -> 'a) -> ('r, unit, string, 'a) format4 -> 'r

(** Same as [bprintf], but instead of returning immediately, passes the buffer, after
    printing, to its first argument. *)
val kbprintf
  :  (Caml.Buffer.t -> 'a)
  -> Caml.Buffer.t
  -> ('r, Caml.Buffer.t, unit, 'a) format4
  -> 'r

(** {6 Formatting error and exit functions}

    These functions have a polymorphic return type, since they do not return.  Naively,
    this doesn't mix well with variadic functions: if you define, say,

    {[
      let f fmt = ksprintf (fun s -> failwith s) fmt
    ]}

    then you find that [f "%d" : int -> 'a], as you'd expect, and [f "%d" 7 : 'a]. The
    problem with this is that ['a] unifies with (say) [int -> 'b], so [f "%d" 7 4] is not
    a type error -- the [4] is simply ignored.

    To mitigate this problem, these functions all take a final unit parameter. These
    rarely arise as formatting positional parameters (they can do with e.g. "%a", but not
    in a useful way) so they serve as an effective signpost for
    "end of formatting arguments". *)


(** Raises [Failure]. *)
val failwithf : ('r, unit, string, unit -> _) format4 -> 'r

(** Raises [Invalid_arg]. *)
val invalid_argf : ('r, unit, string, unit -> _) format4 -> 'r
