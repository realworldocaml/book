# Testing

Testing is not the best loved part of software engineering, often
feeling like a painful distraction from the work of building out the
functionality of a project.  OCaml's type-system can make testing seem
even less appealing, since the type system's ability to squash many
kinds of bugs at compile time makes it seem like that testing isn't
all that important.

But make no mistake, clever types notwithstanding, testing is
essential for developing and evolving complex software systems.  The
goal of this chapter is to teach you more about how to write effective
tests in OCaml, and to teach you some of the best tools for doing so.

Tooling is especially important in the context of testing because one
of the things that prevents people from doing as much testing as they
should is the sheer tedium of it.  But with the right tools in hand,
writing tests can be lightweight and fun.  And when testing is fun, a
lot more testing gets done.

Before talking about the testing tools that are available in OCaml,
let's discuss at a high level what we want out of our tests and our
testing tools in the first place.

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

```scheme file=examples/correct/simple_inline_test/dune
(library
 (name foo)
 (libraries base stdio)
 (inline_tests)
 (preprocess (pps ppx_jane)))
```

With this done, any module in this library can host a test. We'll
demonstrate this by creating a file called `test.ml`, containing one
test.

```ocaml file=examples/correct/simple_inline_test/test.ml
open! Base

let%test "rev" =
  List.equal Int.equal (List.rev [3;2;1]) [1;2;3]
```

The test passes if the expression on the right-hand side of the
equals-sign evaluates to true.  Inline tests are not automatically run
with the instantiation of the module, but are instead registered for
running via the test runner.

```sh dir=examples/correct/simple_inline_test
  $ dune runtest
```

No output is generated because the test passed successfully.
But if we break the test,

```ocaml file=examples/erroneous/broken_inline_test/test.ml
open! Base

let%test "rev" =
  List.equal Int.equal (List.rev [3;2;1]) [3;2;1]
```

we'll see an error when we run it.

```sh dir=examples/erroneous/broken_inline_test
  $ dune runtest
  File "test.ml", line 3, characters 0-66: rev is false.
  
  FAILED 1 / 1 tests
  [1]
```

### More readable errors with `test_eq`

One annoyance with the test output we just saw is that it doesn't show
the data associated with the failed test, thus making it harder to
diagnose and fix the problem when it occurs.  We can fix this if we
signal a test failure by throwing an exception, rather than by
returning false.  That exception can then be used to report the
details of what went wrong.

To do this, we'll change our test declaration to use `let%test_unit`
instead of `let%test`, so that the test no longer expects a body that
returns a bool.  We're also going to use the `[%test_eq]` syntax,
which, given a type, generates code to test for equality and throw a
meaningful exception if the arguments are unequal.

Here's what our new test looks like. You'll notice that it's a little
more concise, mostly because this is a more concise way to express the
comparison function.

```ocaml file=examples/erroneous/test_eq-inline_test/test.ml
open! Base

let%test_unit "rev" =
  [%test_eq: int list] (List.rev [3;2;1]) [3;2;1]
```

Here's what it looks like when we run the test.

```sh dir=examples/erroneous/test_eq-inline_test
  $ dune runtest
  File "test.ml", line 3, characters 0-71: rev threw
  (duniverse/ppx_assert.v0.13.0/runtime-lib/runtime.ml.E "comparison failed"
    ((1 2 3) vs (3 2 1) (Loc test.ml:4:13))).
    Raised at file "duniverse/ppx_assert.v0.13.0/runtime-lib/runtime.ml", line 28, characters 28-53
    Called from file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 502, characters 15-19
    Called from file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 343, characters 8-12
    Re-raised at file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 346, characters 6-13
    Called from file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 359, characters 15-52
    Called from file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 446, characters 52-83
  
  FAILED 1 / 1 tests
  [1]
```

As you can see, the data that caused the comparison to fail is printed
out, along with the stacktrace.  Note that in this case the stacktrace
is mostly a distraction, which is a downside of using exceptions to
report test failures.

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

- **Readability**. Including all of your tests directly in your
  application code can make that code itself harder to read. This can
  lead to people writing too few tests in an effort to keep their
  application code uncluttered.

- **Bloat**. When your tests are written as a part of your library, it
  means that every user of your library has to link in that testing
  code in their production application.  Even though that code won't
  be run, it still adds to the size of the executable.  It can also
  require dependencies on libraries that you don't need in production,
  which can reduce the portability of your code.

- **Testing mindset**. Writing tests on the inside of your libraries
  lets you write tests against any part of your implementation, rather
  than just the exposed API.  This freedom is useful, but can also put
  you in the wrong testing mindset.  Testing that's phrased in terms
  of the public API often does a better job of testing what's
  fundamental about your code, and will better survive refactoring of
  the implementation.  Also, the discipline of keeping tests outside
  of requires you to write code that can be tested that way, which
  pushes towards designs that are better factored out.

For all of these reasons, our recommendation is to put the bulk of
your tests in test-only libraries created for that purpose.  There are
some legitimate reasons to want to put some test directly in your
production library, e.g., when you need access to some functionality
to do the test that's important but is really awkward to expose.  But
such cases are very much the exception.

::: {data-type=note}
##### Why can't inline tests go in executables?

We've only talked about putting tests into libraries. What about
executables? After all, you want to test the logic of your
command-line tools as well.  It turns out you can't do this directly,
because Dune doesn't support the `inline_tests` declaration in source
files that are directly part of an executable.

There's a good reason for this: the `ppx_inline_test` test runner
needs to instantiate the modules that contain the tests. If those
modules have toplevel side-effects, that's a recipe for disaster,
since you don't want those top-level effects to be triggered by the
test framework without your say-so.

So, how do we test code that's part of an executable? The solution is
to break up your program in to two pieces: a directory containing a
library that contains the logic of your program, but no dangerous
top-level effects; and a directory for the executable that links in
the library, and is responsible for launching the code.

:::

## Property testing with Quickcheck

The tests we've discussed so far have been quite simple, amounting to
little more than individual examples paired with assertions checking
that the example in question worked as expected.  *Property testing*
is a useful extension of this approach, which lets you explore a much
larger portion of your code's behavior with only a small amount of
extra code.

The basic idea is simple enough. A property test requires two things:
a function that takes an example input and checks that a given
property holds on that example; and a way of generating random
examples. The test then checks whether the predicate holds over many
randomly generated examples.

We can write a property test using only the tools we've learned so
far.  In this example, we'll check an obvious-seeming invariant
connecting three operations: `Int.sign`, which computes a `Sign.t`
representing the sign of an integer (`Positive`, `Negative`, or
`Zero`), `Int.neg`, which negates a number, and `Sign.flip`, which,
well, flips a `Sign.t`, i.e., mapping `Positive` to `Negative` and
vice-versa.  The invariant is simply that the sign of the negation of
an integer `x` is the flip of the sign of `x`.

Here's one way of implementing this test as a property test.

```ocaml file=examples/correct/manual_property_test/test.ml
open! Base

let%test_unit "negation flips the sign" =
  for _ = 0 to 100_000 do
    let x = Random.int_incl Int.min_value Int.max_value in
    [%test_eq: Sign.t]
      (Int.sign (Int.neg x))
      (Sign.flip (Int.sign x))
  done
```

As you might expect, the test passes.

```sh dir=examples/correct/manual_property_test
  $ dune runtest
```

One choice we had to make in our implementation is which probability
distribution to use for selecting examples.  This may seem like an
unimportant question, but when it comes to testing, not all
probability distributions are equally good.

In fact, the choice we made, which was to pick integers uniformly and
at random from the full set of integers, is problematic, since it
picks interesting special cases, like zero and one, with the same
probability as everything else.  Given the number of integers, the
chance of testing any of those special cases is rather low.  This
accords poorly with the intuition that one should make sure to test
out corner cases.

That's where Quickcheck comes in.  Quickcheck is a library to help
automate the construction of testing distributions. Let's try
rewriting the example we provided above with Quickcheck.  Note that we
open `Core_kernel` here because `Core_kernel` integrates the
quickcheck library with some convenient helpers.  There's also a
standalone `Base_quickcheck` library that can be used without
`Core_kernel`.

```ocaml file=examples/erroneous/quickcheck_property_test/test.ml
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
we've been testing doesn't actually hold on all outputs, as you can
see below.

```sh dir=examples/erroneous/quickcheck_property_test
  $ dune runtest
  File "test.ml", line 3, characters 0-244: negation flips the sign threw
  ("Base_quickcheck.Test.run: test failed" (input -4611686018427387904)
    (error
      ((duniverse/ppx_assert.v0.13.0/runtime-lib/runtime.ml.E
         "comparison failed" (Neg vs Pos (Loc test.ml:7:19)))
         "Raised at file \"duniverse/ppx_assert.v0.13.0/runtime-lib/runtime.ml\", line 28, characters 28-53\
        \nCalled from file \"duniverse/base.v0.13.2/src/or_error.ml\", line 75, characters 9-15\
        \n"))).
    Raised at file "duniverse/base.v0.13.2/src/error.ml", line 8, characters 14-30
    Called from file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 502, characters 15-19
    Called from file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 343, characters 8-12
    Re-raised at file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 346, characters 6-13
    Called from file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 359, characters 15-52
    Called from file "duniverse/ppx_inline_test.v0.13.1/runtime-lib/runtime.ml", line 446, characters 52-83
  
  FAILED 1 / 1 tests
  [1]
```

The example that triggers the exception is `-4611686018427387904`,
also known as `Int.min_value`, which is the smallest value of type
`Int.t`. Note that the largest int, `Int.max_value`, is smaller in
absolute value than `Int.max_value`.

```ocaml env=main
# Int.min_value
- : int = -4611686018427387904
# Int.max_value
- : int = 4611686018427387903
```

It turns out that the standard behavior for negation is that the
negation of the minimum value of an int is equal to itself, as you can
see here.

```ocaml env=main
# Int.neg Int.min_value
- : int = -4611686018427387904
```

Quickcheck's insistence on tracking and testing special cases is what
allowed it to find this error.

### Handling complex types

Tests can't subsist on simple atomic types alone, which is why you'll
often want to build probability distributions over more complex types.
Here's a simple example, where we want to test the behavior of
`List.rev_append`.  For this test, we're going to use a probability
distribution for generating pairs of lists of integers.  The following
example shows how htat can be done using Quickcheck's combinators.

```ocaml file=examples/correct/bigger_quickcheck_test/test.ml
open Core_kernel

let gen_int_list_pair =
  let int_list_gen =
    List.gen_non_empty (Int.gen_incl Int.min_value Int.max_value)
  in
  Quickcheck.Generator.both int_list_gen int_list_gen

let%test_unit "List.rev_append is List.append of List.rev" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int list * int list]
    gen_int_list_pair
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

The declaration of the generator is pretty simple, but it's also
tedious.  Happily, Quickcheck ships with a PPX that can automate
creation of the generator given just the type declaration.  We can use
that to simplify our code, as shown below.

```ocaml file=examples/correct/bigger_quickcheck_test_with_ppx/test.ml
open Core_kernel

let%test_unit "List.rev_append is List.append of List.rev" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int list * int list]
    [%quickcheck.generator: int list * int list]
    ~f:(fun (l1,l2) ->
        [%test_eq: int list]
          (List.rev_append l1 l2)
          (List.append (List.rev l1) l2))
```

This also works with other, more complex data-types, like variants.
Here's a simple example.

```ocaml env=main
# type shape =
    | Circle of { radius: float }
    | Rect of { height: float; width: float }
    | Poly of (float * float) list
  [@@deriving quickcheck]
type shape =
    Circle of { radius : float; }
  | Rect of { height : float; width : float; }
  | Poly of (float * float) list
val quickcheck_generator_shape : shape Base_quickcheck.Generator.t = <abstr>
val quickcheck_observer_shape : shape Base_quickcheck.Observer.t = <abstr>
val quickcheck_shrinker_shape : shape Base_quickcheck.Shrinker.t = <abstr>
```

This will make a bunch of reasonable default decisions, like picking
`Circle`, `Rect`, and `Poly` with equal probability.  We can use
annotations to adjust this, for example, by specifying the weight on a
particular variant.

```ocaml env=main
# type shape =
    | Circle of { radius: float } [@quickcheck.weight 0.2]
    | Rect of { height: float; width: float }
    | Poly of (float * float) list
  [@@deriving quickcheck]
type shape =
    Circle of { radius : float; }
  | Rect of { height : float; width : float; }
  | Poly of (float * float) list
val quickcheck_generator_shape : shape Base_quickcheck.Generator.t = <abstr>
val quickcheck_observer_shape : shape Base_quickcheck.Observer.t = <abstr>
val quickcheck_shrinker_shape : shape Base_quickcheck.Shrinker.t = <abstr>
```

Note that the default weight on each case is `1`.

### More control with let-syntax

If the annotations associated with `ppx_quickcheck` don't let you do
precisely what you want, you can get more control by taking advantage
of the fact that Quickcheck's generators form a monad.  That means it
supports operators like `bind` and `map`, which we first presented in
an error handling context in [Error
Handling](error-handling.html#bind-and-other-error-handling-idioms){data-type=xref}.

In combination with `Let_syntax`, the generator monad gives us a
convenient way to specify generators for custom types. Imagine we
wanted to construct a generator for the `shape` type defined above.

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

Throughout this function we're making choices about the probability
distribution. For example, the use of the `union` operator means that
circles, rectangles and polygons will be equally likely. We could have
used `weighted_union` to pick a different distribution.

The full API for building generators is beyond the scope of this
chapter, but it's worth digging in to the API docs if you want more
control over the distribution of your test examples.

## Expect Tests

While property-based tests are very useful, they're not always what
you want.  Sometimes, instead of writing down properties, you want
tests that capture and make visible the behavior of your code under
specified, concrete scenarios, and which warn you when that captured
behavior changes.  *Expect tests* provide a way of doing just that.

### Basic mechanics

With expect tests, your source file specifies both the code to be
executed and the expected output.  Upon running an expect test, any
discrepancy between the expected output and what was actually
generated is reported as a test failure.

Here's a simple example of a test written in this style.  While the
test generates output (though a call to `print_endline`), that output
isn't captured in the source, at least, not yet.

```ocaml file=examples/erroneous/trivial_expect_test/test.ml
open! Base
open! Stdio

let%expect_test "trivial" =
  print_endline "Hello World!"
```

If we run the test, we'll be presented with a diff between what we
wrote, and a *corrected* version of the source file that now has an
`[%expect]` clause containing the output.

```sh dir=examples/erroneous/trivial_expect_test,unset-INSIDE_DUNE
  $ dune runtest
       patdiff (internal) (exit 1)
  ...
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

The expect test runner also creates a version of the file with the
captured output, with `.corrected` appended to the end of the
filename.  If this new output looks correct, we can *promote* it by
copying the corrected file it over the original source.  The `dune
promote` command does just this, leaving our source as follows.

```ocaml file=examples/correct/trivial_expect_test_fixed/test.ml
open! Base
open! Stdio

let%expect_test "trivial" =
  print_endline "Hello World!";
  [%expect {| Hello World! |}]
```

Now, if we run the test again, we'll see that it passes.

```sh dir=examples/correct/trivial_expect_test_fixed
  $ dune runtest
```

We only have one expect block in this example, but the system supports
having multiple expect blocks, as you can see below.

```ocaml file=examples/correct/multi_block_expect_test/test.ml
open! Base
open! Stdio

let%expect_test "multi-block" =
  print_endline "Hello";
  [%expect{| Hello |}];
  print_endline "World!";
  [%expect{| World! |}]
```


### What are expect tests good for?

It's not obvious why one would want to use expect tests in the first
place. Why should this:

```ocaml file=examples/correct/simple_expect_test/test.ml
open! Base
open! Stdio

let%expect_test _ =
  print_s [%sexp (List.rev [3;2;1] : int list)];
  [%expect {| (1 2 3) |}]
```

be preferable to this?

```ocaml file=examples/correct/simple_inline_test/test.ml
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
output to make sure it makes sense to the human eye.

### An example: web-scraping

A routine programming task which often suffers from a lack of a clear
specification is web-scraping.  The goal is to extract some useful
information from an arbitrary web page.

Here's some code that attempts to just that.  The following function
uses the `lambdasoup` package to traverse some HTML and spit out a set
of strings.  The goal of this function is to produce the set of
hosts that show up in the href of links within the document.

```ocaml file=examples/erroneous/soup_test/test.ml,part=0
open! Base
open! Stdio

let get_href_hosts soup =
  Soup.select "a[href]" soup
  |> Soup.to_list
  |> List.map ~f:(Soup.R.attribute "href")
  |> Set.of_list (module String)
```

We can then try this out by adding an expect test that runs this code
on some sample data.

```ocaml file=examples/erroneous/soup_test/test.ml,part=1
let%expect_test _ =
  let example_html = {|
    <html>
      Some random <b>text</b> with a
      <a href="http://ocaml.org/base">link</a>.
      And here's another
      <a href="http://github.com/ocaml/dune">link</a>.
      And here is <a>link</a> with no href.
    </html>|}
  in
  let soup = Soup.parse example_html in
  let hrefs = get_href_hosts soup in
  print_s [%sexp (hrefs : Set.M(String).t)]
```

If we run the test, we'll see that the output isn't exactly what was
intended.

```sh dir=examples/erroneous/soup_test,unset-INSIDE_DUNE
  $ dune runtest
       patdiff (internal) (exit 1)
  ...
  ------ test.ml
  ++++++ test.ml.corrected
  File "test.ml", line 24, characters 0-1:
   |  |> List.map ~f:(Soup.R.attribute "href")
   |  |> Set.of_list (module String)
   |
   |[@@@part "1"] ;;
   |let%expect_test _ =
   |  let example_html = {|
   |    <html>
   |      Some random <b>text</b> with a
   |      <a href="http://ocaml.org/base">link</a>.
   |      And here's another
   |      <a href="http://github.com/ocaml/dune">link</a>.
   |      And here is <a>link</a> with no href.
   |    </html>|}
   |  in
   |  let soup = Soup.parse example_html in
   |  let hrefs = get_href_hosts soup in
  -|  print_s [%sexp (hrefs : Set.M(String).t)]
  +|  print_s [%sexp (hrefs : Set.M(String).t)];
  +|  [%expect {| (http://github.com/ocaml/dune http://ocaml.org/base) |}]
  [1]
```

The problem here is that we failed to extract the host from the URI
string.  I.e., we ended up with `http://github.com/ocaml/dune` instead
of simple `github.com`.  We can fix that by using the `uri` library to
parse the string and extract the host.  Here's the modified code.

```ocaml file=examples/erroneous/soup_test_half_fixed/test.ml,part=0
let get_href_hosts soup =
  Soup.select "a[href]" soup
  |> Soup.to_list
  |> List.map ~f:(Soup.R.attribute "href")
  |> List.filter_map ~f:(fun uri -> Uri.host (Uri.of_string uri))
  |> Set.of_list (module String)
```

And if we run the test again, we'll see that the output is now as it
should be.

```sh dir=examples/erroneous/soup_test_half_fixed,unset-INSIDE_DUNE
  $ dune runtest
       patdiff (internal) (exit 1)
  ...
  ------ test.ml
  ++++++ test.ml.corrected
  File "test.ml", line 26, characters 0-1:
   |  |> Set.of_list (module String)
   |
   |[@@@part "1"] ;;
   |let%expect_test _ =
   |  let example_html = {|
   |    <html>
   |      Some random <b>text</b> with a
   |      <a href="http://ocaml.org/base">link</a>.
   |      And here's another
   |      <a href="http://github.com/ocaml/dune">link</a>.
   |      And here is <a>link</a> with no href.
   |    </html>|}
   |  in
   |  let soup = Soup.parse example_html in
   |  let hrefs = get_href_hosts soup in
   |  print_s [%sexp (hrefs : Set.M(String).t)];
  -|  [%expect {| (http://github.com/ocaml/dune http://ocaml.org/base) |}]
  +|  [%expect {| (github.com ocaml.org) |}]
  [1]
```


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
