ppx\_assert
===========

Assert-like extension nodes that raise useful errors on failure.

This ppx rewriter defines 3 extension nodes.

`[%test_eq: typ]` in expressions
--------------------------------

`[%test_eq: typ]` in expressions expands to a function of type:

```ocaml
?equal:(type -> type -> bool) -> ?here:Lexing.position list -> type -> type -> unit
```

i.e., it asserts the equality of its two anonymous arguments, using the provided equality
or `[%compare: typ]` (sadly the type need to be comparable even if you provide an
equality), and if they are not equal, an exception containing the value displayed by using
`[%sexp_of: typ]` is thrown.

The exception also contains the source code position of the extension node and the
additional positions from the here parameter. The ?here parameter is meant for cases where
you want additional locations, usually because the backtrace is lacking information.

`[%test_result: typ]` in expressions
------------------------------------

`[%test_result: typ]` is very similar to `[%test_eq:typ]`. It has a slightly improved
error message for the common case where rather than comparing two arbitrary values, you
have one expected value, and one computed value.

`[%test_result: typ]` expands to a function of type:

```ocaml
?here:Lexing.position list -> ?message:string -> ?equal:(type -> type -> bool) -> expect:typ -> typ -> unit
```

`[%test_pred: typ]` in expressions
----------------------------------

This one is the least useful. `[%test_pred: typ]` expands to a function of type:

```ocaml
?here:Lexing.position list -> ?message:string -> (type -> bool) -> type -> unit
```

It simply applies the given predicate to the given value, and if the predicate returns `false`,
then an exception containing the value shown using `[%sexp_of: typ]` is thrown.

Intended usage
--------------

These assertions are very useful when testing. Compared to using `assert (x = y)`, you can
see the values that are not equal, and the assertion is not turned off by
`-noassert`. Compared to using the various `assert_bool` or `assert_string` functions you
can find in various unit testing libraries, it works with any sexpable and comparable type
for zero effort.  For instance, tests commonly look like this:

```ocaml
let%test_unit "List.length" =
  [%test_result: int] (List.length [1; 2]) ~expect:2
let%test_unit "List.tail" =
  [%test_result: int list] (List.tail [1; 2]) ~expect:[2]
```

However convenient these extensions are for testing, it is also possible to use these
extensions even outside of test, in production code, for instance in a function that
checks invariants, or when checking some form of precondition.
