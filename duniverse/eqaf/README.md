# Eq(af) - Constant time equal function

This library implements various *constant time* algorithms,
first and foremost the `Eqaf.equal` equality testing function for `string`.

While the test suite has a number of external dependencies, the library itself
does not have any dependencies besides the OCaml standard library.
This should make `eqaf` small, self-contained, and easy to integrate in your
code.

The "constant time" provided by this library is *constant* in the sense that
the time required to execute the functions does not depend on the *values* of
the operands. The purpose is to help programmer shield their code from
timing-based side-channel attacks where an adversary tries to measure execution
time to learn about the contents of the operands. They are *not* necessarily
"constant time" in the sense that each invocation takes exactly the same amount
of microseconds.

Constant time implementations are beneficial in many different applications;
cryptographic libraries like [digestif](https://github.com/mirage/digestif),
but also practial code that deal with sensitive information (like passwords)
benefit from constant time execution in select situations.
A practical example of where this matters is the
[Lucky Thirteen attack](https://en.wikipedia.org/wiki/Lucky_Thirteen_attack)
against the TLS protocol, where a short-circuiting comparison compromised
the message encryption layer in many vulnerable implementations.

You can generate and view the documentation in a browser with:
```shell
dune build @doc
xdg-open ./_build/default/_doc/_html/index.html
```

# Contents

We found that we often had to duplicate the constant time `equal` function, and
to avoid replication of code and ensure maintainability of the implementation,
we decided to provide a little package which implements the `equal`
function on `string`. Since then, we have added a number of other useful
constant time implementations:

- `compare_be` and `compare_le`: can be used to compare integers (or actual
  strings) of any size in either big-endian or little-endian representation.
  Regular string comparison (e.g. `String.compare`) is usually
  *short-circuiting*, which results in an adversary being able to learn the
  contents of compared strings by timing repeated executions.
  If the lengths do not match, the semantics for `compare_be` follow those
  of `String.compare`, and `compare_le` those of `String.compare (reverse str)`.
  - The `compare_be_with_len` and `compare_le_with_len` are similar,
    but does a constant time comparison of the lengths of the two
    strings and the `~len` parameter. If the lengths do not match,
    an exception is thrown (which leaks the fact that the lengths did not
    match, but not the lengths or the contents of the input operands).

- `exists_uint8 : ?off -> f:(int -> bool) -> string -> bool`:
  implements the equivalent of `List.exists` on `string`, but executing in
  constant time with respect to the contents of the string and `?off`.
  The user provides a callback function that is given each byte as an integer,
  and is responsible for ensuring this function also operates in constant time.

- `find_uint8`: similar to `exists_uint8`, but implementing the
  functionality of `List.find`: It returns the string index of the first match.

- `divmod`: constant time division and modulo operations.
  The execution time of both operations in normal implementations are
  notoriously dependent on the operands. The `eqaf` implementation uses an
  algorithm ported from `SUPERCOP`.

- `ascii_of_int32 : digits:int -> int32 -> string`:
  Turns the `int32` argument into a fixed-width (of length `= digits`)
  left-padded string containing the decimal representation,
  ex: `ascii_of_int32 ~digits:4 123l` is `"0123"`.
  Usually programming languages provide similar functionality
  (ex: `Int32.to_string`), but are vulnerable to timing attacks  since they
  rely on division. This implementation is similar, but uses `Eqaf.divmod` to
  mitigate side channels.

- `lowercase_ascii` and `uppercase_ascii` implement functionality equivalent to
  the identically named functions in `Stdlib.String` module, but without
  introducing a timing side channel.

- `hex_of_string`: constant-time hex encoding.
  Normally hex encoding is implemented with either a table lookup or
  processor branches, both of which introduce side channels for an adversary to
  learn about the contents of the string being encoded.
  That can be a problem if an adversary can repeatedly trigger encoding of
  sensitive values in your application and measure the response time.

- `string_of_hex`: constant-time hex decoding.
  Inverse of `hex_of_string`, but with support for decoding uppercase and
  lowercase letters alike.

This package, if `cstruct` or `base-bigarray` is available, will make this
`equal` function for them too (as `eqaf.cstruct` and `eqaf.bigarray`).

A number of low-level primitives used by `Eqaf` are also exposed to enable you
to construct your own constant time implementations:

- `zero_if_not_zero : int -> int`:
  `(if n <> 0 then 0 else 1)`, or `!n` in the C programming language.
- `one_if_not_zero : int -> int`:
  `(if n <> 0 then 1 else 0)`, or `!!n` in the C programming language.
  - `bool_of_int : int -> bool`, like `one_if_not_zero` but cast to `bool`
  - `int_of_bool : bool -> int`, inverse of `bool_of_int`
- `select_int : int -> int -> int -> int`:
  `select_int choose_b a b` is a constant time utility for branching,
  but always executing all the branches (to ensure constant time operation):
  ```ocaml
  let select_int choose_b a b = if choose_b = 0 then a else b
  ```
- `select_a_if_in_range : ~low:int -> ~high:int -> n:int -> int -> int -> int`
  Similar to `select_int`, but checking for inclusion in a range rather than
  testing for zero - a CT version of:
  ```ocaml
  let select_a_if_in_range ~low ~high ~n a b =
    if low <= n && n <= high
    then a
    else b
  ```

## Check tool

To ensure correctness,  `eqaf` ships with a test suite to measure the functions
and comparing results both to the `Stdlib` implementations and to executions of
the same function with similar length/size input.
The goal is to try to spot implementation weaknesses that rely on the *values*
of the function operands.

The check tool will first attempt to calculate how many executions are
required to get statistically sound numbers (sorting out random jitter from
external factors like other programs executing on the computer).

Then, using linear regression, we compare the results and verify that we did
not spot differences: the regression coefficient should be close to `0.0`.

You can test `eqaf` with this:

```sh
$ dune exec check/check.exe
```

### Q/A

**Q** How to update `eqaf` implementation?

**A** `eqaf` is fragile where the most important assumption is times needed to
compute `equal`. So `eqaf` provides the `check` tool but results from it can be
disturb by side-channel (like hypervisor). In a bare-metal environment, `check`
strictly works and should return `0`.

**Q** `eqaf` is slower than `String.compare`, it's possible to optimize it?

**A** The final goal of `eqaf` is to provide a _safe_ equal function. Speed is
clearly not what we want where we prefer to provide an implementation which does
not leak informations like: where is the first byte which differs between `a`
and `b`.

**Q** Which attack `eqaf` prevents?

**A** `eqaf` provide an equal function to avoid a timing attack. Most of equal
or compare functions (like `String.compare`) leave at the first byte which
differs. A possible attack is to see how long we need to compare two values,
like an user input and a password.

Logically, the longer this time is, the more user input is the password. So when
we need to compare sensible values (like hashes), we should use something like
`eqaf`. The distribution provides an example of this attack:

```sh
$ dune exec attack/attack.exe
Random: [|218;243;59;121;8;57;151;218;212;91;181;41;|].
471cd8bc03992a31f8f0f0c55e9e477d
471cd8bc03992a31f8f0f0c55e9e477d
```

The first value is the hash, the second is what we found just by an
introspection of time needed by our `equal` function.

**Q** `eqaf` provides only equal function on `string`?

**A** The first implementation use `string`, then, we copy/paste the code with
`bigarray` and provide it only if `base-bigarray` is available. Finally, we
provide an `equal` function for `cstruct` only if this package is available.

So, it's not only about `string` but for some others data-structures.

**Q** Why we need to do a linear regression to check assumptions on `eqaf`?

**A** As we said, times are noisy by several side-computation (hypervisor,
kernel, butterfly...). So, if we record two times how long we spend to compute
`equal`, we will have 2 different values - close each others but different.

So we need to have a bunch of samples and do an analyze on them to get an
approximation. From that, we do 2 analyzes:
- get the approximation where we compare 2 same values
- get the approximation where we compare 2 different values

From these results, we need to do an other analyze on these approximations to
see if they are close each others or not. In the case of `eqaf`, it should be
the case (and if it is, that means `eqaf` does not leak a time information
according inputs). In the case of `String.compare`, we should have a big
difference - and confirm expected behaviors.

[digestif]: https://github.com/mirage/digestif.git
