# Testing

Testing is not the best loved part of software engineering.  It can be
painful work, and it often feels like a distraction from the important
work of adding functionality to your code.  In some ways, OCaml's
type-system makes testing seem even less appealing, since the type
system's ability to squash many kinds of bugs at compile time makes it
seem like that testing isn't all that important.

But make no mistake, clever types notwithstanding, testing is
essential for developing and evolving complex software systems.  The
goal of this chapter is to teach you more about how to write effective
tests in OCaml, and to teach you some of the best tools for doing so.

Tooling is especially important in the context of testing because one
of the things that prevents people from writing all the tests they
should is the sheer tedium of it all.  But with the right tools in
hand, writing tests can be lightweight and fun.  And when testing is
fun, people do a lot more of it.

But before talking about the testing tools that are available in
OCaml, let's discuss at a high level what we want out of our tests and
our testing tools in the first place.

## What makes for good tests?

Here are some of the properties that characterize well-written tests
in a good testing environment.

- **Easy to write and run**. Tests should require a minimum of
  boilerplate to create and to hook into your build pipeline.
  Ideally, you should set things up so that tests are run
  automatically on every proposed change, preventing people from
  accidentally breaking the build.
- **Easy to update**. Tests that are hard to adjust in the face of
  code changes can become their own form of technical debt.
- **Fast**, so they don't slow down your development process.
- **Deterministic**. It's hard to take test failures seriously if
  there's a decent chance that the failure is a random glitch.  You
  want your test failures to be believable indications of a problem,
  which requires determinism.
- **Understandable**. Good tests should be easy to read, and their
  failures should be localized and specific, so it's easy to find and
  fix the problem flagged by a failing test.

No testing framework can ensure that your tests satisfy these
properties.  But the tools you choose can help or hinder on all these
fronts.

As we go through the rest of this chapter and introduce you to some of
the available tooling, you should be able to see how each tool can
help advance these goals.

## Inline tests

The first step towards a good testing environment is making it easy to
set up and and run a test.  To that end, we'll show you how to write
tests with `ppx_inline_test`, which lets you add tests to any module
in a library with a specially annotated `let` binding.

To use inline tests in a library, we need to do two things:

- Tell Dune to expect inline tests to show up in the library, and
- enable `ppx_inline_test` as a preprocessor.

The first of these is achieved by adding an `(inline_tests)`
declaration, and the second is achieved by adding `ppx_jane` to the
set of preprocessors.  (`ppx_jane` bundles together `ppx_inline_test`
with a collection of other useful preprocessors.)  Here's the
resulting `dune` file.

```scheme file=examples/simple_inline_test/dune
(library
 (name foo)
 (libraries base stdio)
 (inline_tests)
 (preprocess (pps ppx_jane)))
```

With this done, any module in this library can host a test. We'll
demonstrate this by creating a file called `test.ml`, containing one
test.

```ocaml file=examples/simple_inline_test/test.ml
open! Base

let%test "rev" =
  List.equal Int.equal (List.rev [3;2;1]) [1;2;3]
```

The test passes if the expression on the right-hand side of the
equals-sign evaluates to true.  Inline tests are not automatically run
with the instantiation of the module, but are instead registered for
running via the test runner.

```sh dir=examples/simple_inline_test
  $ dune runtest
```

No output is generated because the test passed successfully.
But if we break the test,

```ocaml file=examples/broken_inline_test/test.ml
open! Base

let%test "rev" =
  List.equal Int.equal (List.rev [3;2;1]) [3;2;1]
```

we'll see an error when we run it.

```sh dir=examples/broken_inline_test
  $ dune runtest
  File "test.ml", line 3, characters 0-66: rev is false.

  FAILED 1 / 1 tests
  [1]
```

### More readable errors with `test_eq`

One annoyance with the test output we just saw is that it doesn't show
the data associated with the failed test, thus making it harder to
diagnose and fix the problem when it occurs.  We can fix this by
having the test signal by throwing an exception, rather than by
returning false.  That exception can then be used to report the
details of what went wrong.

To do this, we'll change our test declaration to use `let%test_unit`
instead of `let%test`, so that the test no longer expects a body that
returns a bool.  We're also going to use the `[%test_eq]` syntax,
which, given a type, generates code to test for equality and throw a
meaningful exception if the arguments are unequal.

Here's what our new test looks like. You'll notice that it's a little
more concise, mostly because this is a less verbose way to express the
comparison function.

```ocaml file=examples/test_eq-inline_test/test.ml
open! Base

let%test_unit "rev" =
  [%test_eq: int list] (List.rev [3;2;1]) [3;2;1]
```

Here's what it looks like when we run the test.

```sh dir=examples/test_eq-inline_test
  $ dune runtest
  File "test.ml", line 3, characters 0-71: rev threw
  (duniverse/ppx_assert/runtime-lib/runtime.ml.E "comparison failed"
    ((1 2 3) vs (3 2 1) (Loc test.ml:4:13))).
    Raised at file "duniverse/ppx_assert/runtime-lib/runtime.ml", line 28, characters 28-53
    Called from file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 501, characters 15-19
    Called from file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 342, characters 8-12
    Re-raised at file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 345, characters 6-13
    Called from file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 358, characters 15-52
    Called from file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 445, characters 52-83

  FAILED 1 / 1 tests
  [1]
```

As you can see, the data that caused the comparison to fail is printed
out, along with the stacktrace.  Note that in this case the stacktrace
is mostly a distraction, which is a downside of this way of writing
the test.

### Where should tests go?

The inline test framework lets you put tests into any `.ml` file
that's part of a library. But just because you can do something
doesn't mean you should.

Putting tests directly in the library you're building certainly has
some benefits. For one thing, it lets you put a test for a given
function directly after the definition of that function, which in some
cases can be good for readability. This approach also lets you test
aspects of your code that aren't exposed by its external interface.

While this sounds appealing at first glance, putting tests in
libraries has several downsides.

- **Bloat**. When your tests are written as a part of your library, it
  means that every user of your library has to link in that testing
  code in their production application. Even though that code won't be
  run, it still adds to the size of the executable.

- **Excess dependencies**. Testing code doesn't just add size to your
  final executable; it can also require dependencies on libraries that
  you don't need in production.  This can further bloat your
  application, and can reduce portability and cause you to link risky
  code into your production application.

- **Testing against unexposed APIs**. Writing tests on the inside of
  your libraries has the virtue of letting you write tests against any
  of the code you've written, not just what has been exposed in the
  API. But this is a two-edged sword.  Most of the time, it's a good
  mental discipline to express your testing in terms of the public
  API, rather than in terms of the implementation. This encourages you
  to think about and test the invariants exposed to users.

- **Readability**. Including all of your tests directly in your
  application code can make that code itself harder to read. This can
  lead to people writing too few tests in an effort to keep their
  application code uncluttered.

For all of these reasons, our recommendation is to put your tests in
test-only libraries created for that purpose.  There are some
legitimate reasons to want to put some test directly in your
production library, e.g., there's something that's really awkward to
expose in a way that makes it possible to test. But these examples are
few and far between.

::: {data-type=note}
##### Why can't inline tests go in executables?

We've only talked about putting tests into libraries. What about
executables? After all, you want to test the logic of your
command-line tools as well. It turns out you can't do this directly,
since Dune doesn't support the `inline_tests` declaration in
executable files.

There's a good reason for this: the `ppx_inline_test` test runner
needs to instantiate the modules that contain the tests. If those
modules have toplevel side-effects, that's a recipe for disaster. You
don't want your test-framework running lots of copies of your
executables in parallel without your say-so.

So, how do we test code that's part of an executable? The solution is
to break up your program in to two pieces: a directory containing a
library that contains all of the logic of your program, and is
suitable for testing (either with embedded inline tests, or from a
purpose-built testing library); and a directory for the executable
that links in the library, and is just responsible for launching the
program.

:::

## Property testing with Quickcheck

The tests we've discussed so far have been pretty simple, amounting to
little more than writing down individual examples and checking that
their behavior is as expected.  We often want to write tests that do
more than that. One good example of a more powerful style of tests is
called *property testing*.

The basic idea is simple enough. A property test requires two things:
a function that takes an example input and checks that a given
property holds on that example; and a way of generating random
examples. The test then checks whether the predicate holds over many
randomly generated examples.

We can write a property test using only the tools we've learned so
far.  Here's an example.

```ocaml file=examples/manual_property_test/test.ml
open! Base

let%test_unit "negation flips the sign" =
  for _ = 0 to 100_000 do
    let x = Random.int_incl Int.min_value Int.max_value in
    [%test_eq: Sign.t]
      (Int.sign (Int.neg x))
      (Sign.flip (Int.sign x))
  done
```

As you can see below, this test passes.

```sh dir=examples/manual_property_test
  $ dune runtest
```

One thing that was implicit in the example we gave above is the
probability distribution that was used for selecting
examples. Whenever you pick things at random, you're always making a
choice as to the probability with which each possible example is
selected. But not all probability distributions are equally good for
testing.  In fact, the choice we made above, which was to pick
integers uniformly and at random, is problematic, since it picks
interesting special cases, like zero and one, with the same
probability as everything else.

That's where Quickcheck comes in.  Quickcheck is a library to help
automate the construction of testing distributions. Let's try
rewriting the example we provided above with Quickcheck.  Note that we
open `Core_kernel` here because `Core_kernel` integrates the
quickcheck library with some convenient helpers.  There's also a
standalone `Base_quickcheck` library that can be used without
`Core_kernel`.

```ocaml file=examples/quickcheck_property_test/test.ml
open Core_kernel

let%test_unit "negation flips the sign" =
  Quickcheck.test ~sexp_of:[%sexp_of: int]
    (Int.gen_incl Int.min_value Int.max_value)
    ~f:(fun x ->
        [%test_eq: Sign.t]
          (Int.sign (Int.neg x))
          (Sign.flip (Int.sign x)))
```

Note that we didn't explictly state how many examples should be
tested. Quickcheck has a built in default which can be overridden by
way of an optional argument.

In any case, running the test uncovers the fact that the property
we've been testing doesn't actually hold on all outputs, and
Quickcheck has found a counterexample.

```sh dir=examples/quickcheck_property_test
  $ dune runtest
  File "test.ml", line 3, characters 0-244: negation flips the sign threw
  ("Base_quickcheck.Test.run: test failed" (input -4611686018427387904)
    (error
      ((duniverse/ppx_assert/runtime-lib/runtime.ml.E "comparison failed"
         (Neg vs Pos (Loc test.ml:7:19)))
         "Raised at file \"duniverse/ppx_assert/runtime-lib/runtime.ml\", line 28, characters 28-53\
        \nCalled from file \"duniverse/base/src/or_error.ml\", line 64, characters 9-15\
        \n"))).
    Raised at file "duniverse/base/src/error.ml", line 9, characters 14-30
    Called from file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 501, characters 15-19
    Called from file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 342, characters 8-12
    Re-raised at file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 345, characters 6-13
    Called from file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 358, characters 15-52
    Called from file "duniverse/ppx_inline_test/runtime-lib/runtime.ml", line 445, characters 52-83

  FAILED 1 / 1 tests
  [1]
```

The example that triggers the exception is `-4611686018427387904`,
which can also be referred to as `Int.min_value`, and is the smallest
value of type `Int.t`. Note that the largest int, `Int.max_value`, is
smaller in absolute value than `Int.max_value`.

```ocaml env=main
# Int.min_value
- : int = -4611686018427387904
# Int.max_value
- : int = 4611686018427387903
```

It turns out that the standard behavior for negation is that the
negation of the minimum value of an int is equal to itself, as you can
see here:

```ocaml env=main
# Int.neg Int.min_value
- : int = -4611686018427387904
```

Quickcheck found this example for us because it's careful about the
probability distributions it uses, and in particular is careful about
making sure to keep track of and explore special cases like
`min_value` and `max_value`.

### Building more complex values

Tests can't subsist on simple atomic types alone, which is why you'll
often want to build probability distributions over more complex types.
Here's a simple example, where we want to test the behavior of
`List.rev_append`, which requires us to create lists of randomly
generated values.

```ocaml file=examples/bigger_quickcheck_test/test.ml
open Core_kernel

let%test_unit "List.rev_append is List.append of List.rev" =
  let int_list_gen =
    List.gen_non_empty (Int.gen_incl Int.min_value Int.max_value)
  in
  Quickcheck.test
    ~sexp_of:[%sexp_of: int list * int list]
    (Quickcheck.Generator.both int_list_gen int_list_gen)
    ~f:(fun (l1,l2) ->
        [%test_eq: int list]
          (List.rev_append l1 l2)
          (List.append (List.rev l1) l2))
```

Here, we made use of `Quickcheck.Generator.both`, which is useful for
creating a generator for pairs from two generators for the constituent
types.

```ocaml env=main
# open Core_kernel
# Quickcheck.Generator.both
- : 'a Base_quickcheck.Generator.t ->
    'b Base_quickcheck.Generator.t -> ('a * 'b) Base_quickcheck.Generator.t
= <fun>
```

Quickcheck's generator also form a monad, meaning that it supports
operators like `bind` and `map`, which we say in an error handling
context in [Error
Handling](error-handling.html#bind-and-other-error-handling-idioms){data-type=xref}.

In combination with `Let_syntax`, the generator monad gives us a
convenient way to specify generators for custom types. Imagine we
wanted to generate examples of the following variant type.

```ocaml env=main
# type shape = | Circle of { radius: float }
               | Rect of { height: float; width: float }
               | Poly of (float * float) list
type shape =
    Circle of { radius : float; }
  | Rect of { height : float; width : float; }
  | Poly of (float * float) list
```

Using `Let_syntax`, a generator for this would look as follows.

```ocaml env=main
# let gen_shape =
    let open Quickcheck.Generator.Let_syntax in
    let module G = Quickcheck.Generator in
    let circle =
      let%map radius = Float.gen_positive in
      Circle { radius }
    in
    let rect =
      let%map height = Float.gen_positive
      and width = Float.gen_positive
      in
      Rect { height; width }
    in
    let poly =
      let%map points =
        List.gen_non_empty
          (G.both Float.gen_positive Float.gen_positive)
      in
      Poly points
    in
    G.union [circle; rect; poly]
val gen_shape : shape Base_quickcheck.Generator.t = <abstr>
```

It may not be obvious, but throughout this function we're making
choices about the probability distribution. For example, the use of
the `union` operator means that circles, rectangles and polygons will
be equally likely. We could have used `weighted_union` to pick a
different distribution.

The full API for building generators is beyond the scope of this
chapter, but it's worth digging in to the API docs if you want more
control over the distribution of your test examples.

## Expect Tests

While property-based tests are extremely useful, they're not always
what you want. Sometimes, instead of writing down properties, you want
to express your tests as a way of capturing and visualizing the
behavior of your code under simple, concrete scenarios. *Expect tests*
provide a way of doing just that.

### Basic mechanics

With expect tests, your source file specifies both the code to be
executed and the expected output. Upon running an expect test, any
discrepancy between the expected output and what was actually
generated is reported as a test failure.

Here's a simple example of a test written in this style.  Note that
the test generates output, but that output isn't captured in the
source, at least, not yet.

```ocaml file=examples/trivial_expect_test/test.ml
open! Base
open! Stdio

let%expect_test "trivial" =
  print_endline "Hello World!"
```

If we run the test, we'll be presented with a diff between what we
wrote, and a *corrected* version of the source file that now has an
`[%expect]` clause containing the output.

```sh dir=examples/trivial_expect_test
  $ dune runtest
       patdiff (internal) (exit 1)
  (cd _build/default && /home/yminsky/.opam/default/bin/patdiff -keep-whitespace -location-style omake -ascii test.ml test.ml.corrected)
  ------ test.ml
  ++++++ test.ml.corrected
  File "test.ml", line 5, characters 0-1:
   |open! Base
   |open! Stdio
   |
   |let%expect_test "trivial" =
  -|  print_endline "Hello World!"
  +|  print_endline "Hello World!";
  +|  [%expect {| Hello World! |}]
  [1]
```

It doesn't just generate this output, it also creates a version of the
file with the captured output, with `.corrected` appended to the end.
If this new output looks correct, we can *promote* it to be the
expected output by copying it over the original source.  The `dune
promote` does just this, leaving our original test looking like this:

```ocaml file=examples/trivial_expect_test_fixed/test.ml
open! Base
open! Stdio

let%expect_test "trivial" =
  print_endline "Hello World!";
  [%expect {| Hello World! |}]
```

Now, if we run the test again, we'll see that it passes.

```sh dir=examples/trivial_expect_test_fixed
  $ dune runtest
```

We only have one expect block in this example, but the system supports
having multiple expect blocks, as you can see below.

(Something about multi-expect-tests here?
examples/multi_expect_test/test.ml)


### What are expect tests good for?

It's not obvious why one would want to use expect tests in the first
place. Why should this:

```ocaml file=examples/simple_expect_test/test.ml
open! Base
open! Stdio

let%expect_test _ =
  print_s [%sexp (List.rev [3;2;1] : int list)];
  [%expect {| (1 2 3) |}]
```

be preferable to this?

```ocaml file=examples/simple_inline_test/test.ml
open! Base

let%test "rev" =
  List.equal Int.equal (List.rev [3;2;1]) [1;2;3]
```

Indeed, for examples like this, expect tests aren't really better.
Simple example-based tests like the one above are a great solution
when it's easy and convenient to write out specific examples in full.
And property tests are your best bet when you have a clear set of
predicates that you want to test, and examples can be naturally
generated at random.

Where expect tests shine is where you want to capture some aspect of
the behavior of your system that's hard to capture in either
predicates or hand-written examples.  Instead, expect tests give you a
way to visualize the behavior of your code, and then to be notified
whenever that visualization changes.

This is more useful than it might seem at first.  One common use-case
of expect tests is simply to capture the behavior of code where you
don't necessarily have a concise specification of how the code should
behave, and you just want to generate some examples and look at the
output to make sure it make sense to human eyes.

(UNFINISHED)

## Ideas for extending this chapter

- More realistic examples of expect tests.  This is tricky, because
  natural examples are often kind of long.
- Test the boolean simplifier, maybe the one in blang?
- Show an example of HTML-scraping?
- Use expect_test_helpers to do shell commands.
-
- Maybe a tic-tac-toe game?
- How to use randomness in a non-random way?
- Is it worth talking about the technique of capturing random
  examples, and then printing out changes to those examples?
- Talk about how to write things so they're more testable. Determinism
  is a big deal here.  Should we show off the
  state-machine-with-events style?
- Talk about using Synchronous_time_source, for Async applications?
- Give examples of ways of making expect tests with pretty output?
