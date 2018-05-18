# Testing {#testing data-type=chapter}

Testing is fundamental to building reliable software, but you wouldn't
know it from watching how software engineers spend their time.
Testing can be tedious, and in the early stages of a project, it's
often not obvious how important testing is going to become down the
line. This leads people to test less than they should, and more
critically, to design systems without taking testability into account,
which makes such omissions harder to fix down the line.

In some ways, OCaml's type-system makes this worse, by enhancing the
illusion that you can get by without testing.  After all, many trivial
bugs are caught cheaply by OCaml's type system, no testing
required. But make no mistake, with or without types, testing is
essential for developing and evolving complex software systems.

One way to improve the situation is to fix the tedium problem. With
the right tools, writing tests can be made lightweight and fun.  With
better infrastructure in place, you'll find yourself writing more
tests, and your creations will be more reliable as a result.

The goal of this chapter is to teach you about some of the testing
infrastructure available in the OCaml ecosystem. But first, we'll
discuss more generally what you should be optimizing for in your tests
and in your testing infrastructure.

## What makes for good tests? {#what-makes-for-good-tests data-type=sect1}

Here are some of the properties that characterize well written tests
in a good testing environment.

- **Easy to write**. The less overhead there is to adding a test, the
  more people will do it.
- **Easy to run**. Ideally, they should be run automatically, every time
  you push changes.
- **Easy to update**. Tests that are hard to adjust in the face of code
  changes can become their own form of technical debt.
- **Fast**, so they don't slow down your development process.
- **Readable**, so that someone can go back later and understand what
  the test is for.
- **Deterministic**. It's hard to take test failures seriously if
  there's a decent chance that the failure might be random.  You want
  your test failures to be believable indications of a problem, which
  requires determinism.
- **Understandable in their failures**. Tests whose failures are localized and easy
  to comprehend make it easier to find and fix the problem flagged by
  the failing test.

No testing framework can ensure that your tests satisfy these
properties. But the testing tools you choose can help or hinder on all
of these fronts.

As we go through the rest of this chapter, we'll try to show how the
various testing tools available for OCaml can help you on each of
these fronts.

## Inline tests {data-type=sect1}

The first step towards a good testing environment is making it easy to
set up and and run a test.  To that end, we'll show you how to write
tests with `ppx_inline_test`, which lets you add tests to any module
in your library with a specially annotated let binding.

To use inline tests, we need to enable `ppx_inline_test` as a
preprocessor, as well as tell Dune that the files in this library
contain inline tests.  To achieve the first goal, we'll add `ppx_jane`
to the set of preprocessors, which bundles together `ppx_inline_test`
with a collection of other useful preprocessors.  The second goal is
achieved by adding the `inline_tests` declaration to the library
stanza. Here's the resulting `jbuild` file.

<link rel="import" href="code/testing/simple_inline_test/jbuild" />

With this done, any module in this library can host a test. We'll
demonstrate this by creating a file called `test.ml`, containing just
a single test.

<link rel="import" href="code/testing/simple_inline_test/test.ml" />

The test passes if the expression on the right-hand side of the
equals-sign evaluates to true.  These tests are not automatically run
with the instantiation of the module, but are instead registered for
running via the test runner, which can be invoked via Dune.  While it
doesn't affect this example, it's worth noting that the test runner
will execute tests declared in different files in parallel.

<link rel="import" href="code/testing/simple_inline_test/run.sh" />

No output is generated because the test passed successfully.
But if we break the test,

<link rel="import" href="code/testing/broken_inline_test/test.ml" />

then we'll see an error when we run it.

<link rel="import" href="code/testing/broken_inline_test/run.sh" />

### More readable errors with `test_eq` {data-type=sect2}

One problem with the test output we just saw is that it doesn't show
the data associated with the failed test.  We can fix this by having
the test signal success or failure by throwing an exception, and
putting extra diagnostic information in that exception.

To do this, we need to change our test declaration to use
`let%test_unit` instead of `let%test`, so that the test allows a
unit-returning body. We're also going to use the `[%test_eq]` syntax,
which, given a type, generates code to test for equality and throw a
meaningful exception if the arguments are unequal.

Here's what our new test looks like. You'll notice that it's a little
more concise, mostly because this is a less verbose way to express the
comparison function.

<link rel="import" href="code/testing/test_eq-inline_test/test.ml" />

Here's what it looks like when we run the test.

<link rel="import" href="code/testing/test_eq-inline_test/run.sh" />

As you can see, the data that caused the comparison to fail is printed
out, along with the stacktrace that identifies the location of the
error.

### Where should tests go? {data-type=sect2}

The inline test framework lets you put tests into any `.ml` file
that's part of a library. But just because you *can* do something
doesn't mean you *should*. Where in practice should your tests go?

One obvious approach is to put the tests directly in the library
you're developing. That way, you can put a test for a given function
directly after the defintion of that function. This is a appealing at
first glance, since it encourages you to think about testing as you're
writing your application code. But the approach has several downsides.

- **Bloat**. When your tests are written as a part of your library, it
  means that every user of your library has to link in that testing
  code in their production application. Even though that code won't
  get executed in production, it still adds to the size of the
  executable.

- **Dependencies**. Adding testing code to your library doesn't just
  add the generated code for the specific tests you write; it can also
  require you to add dependencies on libraries and modules that you
  only really need for testing. This can further bloat your
  application, and can also require you to link libraries into your
  application that you'd rather not rely on in production.

- **Testing against unexposed APIs**. Writing tests on the inside of
  your libraries has the virtue of letting you write tests against any
  of the code you've written, not just what has been exposed in the
  API. But this is a two-edged sword.  Most of the time, it's a good
  mental discipline to express your testing in terms of the public
  API, rather than in terms of the implementation. This encourages you
  to think about and test the invariants exposed to users.

- **Readability**. Including all of your tests directly in your
  applicatoin code can make that code itself harder to read. This can
  lead to people writing too few tests in an effort to keep your
  application code uncluttered.

For all of these reasons, our recommendation is to put your tests in
test-only libraries created for that purpose.  There are some
legitimate reasons to want to put some test directly in your
production library, e.g., there's something that's really awkward to
expose in a way that makes it possible to test. But these examples are
few and far between.

::: {.allow_break data-type=note}
#### Why can't inline tests go in executables?

We've only talked about putting tests into libraries. What about
executables? After all, you want to test the logic of your
command-line tools as well. It turns out you can't do this directly,
since Dune doesn't support the `inline_tests` declaration in
executable files.

There's a good reason for this, which is that the `ppx_inline_test`
test runner needs to instantiate the modules that contain the
tests. If those modules have toplevel side-effects, that's a recipe
for disaster. You don't want your test-framework running lots of
copies of your executables in parallel without your say-so.

So, how do we test code that's part of an executable? The solution is
to break up your program in to two pieces: a directory containing a
library that contains all of the logic of your program, and is
suitable for testing (either with embedded inline tests, or from a
purpose-built testing library); and a directory for the executable
that links in the library, and is just responsible for launching the
code contained in the companion library.

:::

## Property testing with Quickcheck {data-type=sect1}

The tests we've discussed so far have been pretty simple, amounting to
little more than writing down individual examples and checking that
their behavior is as expected.  We often want to write tests that do
more than that, and one useful form of testing that lets you step
beyond example-based testing is called *property testing*.

The basic idea is simple enough. A property test requires two things:
a function that takes an example input and checks that a given
property holds on that example; as well as a way of generating random
examples. The test then checks whether the predicate holds over many
randomly generated examples.

We can write a property test using only the tools we've learned so
far.  Here's an example.

<link rel="import" href="code/testing/manual_property_test/test.ml" />

As you can see below, this test passes.

<link rel="import" href="code/testing/manual_property_test/run.sh" />

One thing that was implicit in the example we gave above is the
probability distribution that was used for selecting
examples. Whenever you pick things at random, you're always making a
choice as to the probability with which each possible example is
selected. But not all probability distributions are equally good for
testing.  In fact, the choice we made above, which was to pick
integers uniformly and at random, is problematic, since it picks
interesting special, like zero or one, with the same probability as
everything else.

That's where Quickcheck comes in. Quickcheck is a library to help
automate the construction of testing distributions. Let's try
rewriting the example we provided above with Quickcheck.

<link rel="import" href="code/testing/quickcheck_property_test/test.ml" />

Note that we didn't explictly state how many examples should be
tested. Quickcheck has a built in default which can be overridden by
way of an optional argument.

In any case, running the test uncovers the fact that the property
we've been testing doesn't actually hold on all outputs, and
Quickcheck has found a counterexample.

<link rel="import" href="code/testing/quickcheck_property_test/run.sh" />

The example that triggers the exception is `-4611686018427387904`,
which can also be referred to as `Int.min_value`, and is the smallest
value of type `Int.t`. Note that the largest value

<link rel="import" part="min-int1" href="code/testing/main.mlt" />

  `Int.min_value`, which is the
smallest value of type `int`. This is a side effect of how integers
are encoded at the machine level, which means that there's exactly one
extra negative `int` without a corresponding positive
`int`. Quickcheck found



## Expect Tests {data-type=sect1}




## Coverage Testing {data-type=sect1}
