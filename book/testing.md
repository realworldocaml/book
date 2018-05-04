# Testing {#testing data-type=chapter}

Testing is a fundamental part of building reliable software, but that
doesn't prevent people from routinely under-investing in testing. The
reasons are clear enough. Testing can be tedious, and in the early
stages of a project, it's often not obvious how important testing is
going to become down the line. Since testing is harder to do if it
hasn't been designed in from the start, it's all too easy to get stuck
with system that's both under-tested and difficult to add tests to.

In some ways, OCaml's type-system makes this worse, by enhancing the
illusion that you can get by without testing.  After all, many trivial
bugs are caught cheaply by OCaml's type system, no testing
required. But make no mistake, type system or no type system, testing
is essential.

A good way of motivating yourself to write tests is to make it easy
and fun. Testing tools can reduce the tedium of writing tests and
simplify the process of maintaining them.  With better infrastructure
in place, you'll find yourself writing way more tests, and your
creations will be more reliable as a result.

The goal of this chapter is to teach you about some of the available
infrastructure for testing in OCaml. But before that, we'll discuss
what makes for good tests, and what kind of test automation is
important to have. Then we'll turn back to what tools are available
specifically for OCaml.

## What makes for good tests? {#what-makes-for-good-tests data-type=sect1}

Here are some properties that are important for tests to have.

- **Easy to write**. The less overhead there is to adding a test, the
  more people will do it.
- **Easy to run**. Ideally, they should be run automatically, every time
  you push changes.
- **Easy to update**. Tests that are hard to adjust in the face of code
  changes can become their own form of technical debt.
- **Fast**, so they don't slow down your development process.
- **Readable**, so that someone can go back later and understand what
  the test is for.
- **Deterministic**. Test failures that might just be noise are hard
  to take seriously. You want your test failures to be believable
  indications of a problem, which requires determinism.
- **Fail understandably**. Tests whose failures are localized and easy
  to comprehend make it easier to find and fix the problem flagged by
  the failing test.

No testing framework can ensure that your tests satisfy all of these
properties. But the test framework you choose can help or hinder on
all of these fronts.

As we go through the rest of this chapter, we'll try to show how the
various testing tools available for OCaml can help you on each of
these fronts.

## Inline tests {data-type=sect1}

The first step towards building good tests is making them easy to set
up and and run.  The *inline test* framework helps here, by letting
you add tests to any module in your library with a specially annotated
let binding.

To use inline tests, we need to enable the `ppx_inline_test`
preprocessor, as well as mark that the files in this library contain
inline tests. On the preprocessor front, we'll add `ppx_jane` to the
set of preprocessors, which bundles together `ppx_inline_test` with a
collection of other preprocessors that are useful for testing and for
other purposes.  And we'll tell jbuilder to expect tests in this
library by adding the `inline_tests` declaration to the library
stanza, as shown below.

<link rel="import" href="code/testing/simple_inline_test/jbuild" />

With this done, any module in this library can host a test. We'll
demonstrate this by creating a file called `test.ml`, containing just
a single test.

<link rel="import" href="code/testing/simple_inline_test/test.ml" />

The test passes if the expression on the right-hand side of the
equals-sign evaluates to true.  These tests are not automatically run
with the instantiation of the module, but are instead registered for
running via the test runner, which can be invoked via jbuilder.

<link rel="import" href="code/testing/simple_inline_test/run.sh" />

Since the test was correct, it passes, generating no output.  Note
that Jbuilder will run tests declared in different files in parallel,
which is important for making your test suite run quickly.

If we modify the test to have an error, as below,

<link rel="import" href="code/testing/broken_inline_test/test.ml" />

then we will see an error when we run the test.

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
  code in their production application. The code won't get executed in
  production, of course, but it still adds to the size of the
  executable.

- **Dependencies**. Adding testing code to your library doesn't just
  add the generated code for the specific tests you write; it can also
  require you to add dependencies on libraries and modules that you
  only really need for testing. This can further bloat your
  application, and can also require you to link libraries into your
  application that you'd rather not rely on in production.

- **Testing against unexposed APIs**. Writing tests on the inside of your
  libraries has the virtue of letting you write tests against any of
  the code you've written, not just what has been exposed in the
  API. But this is a two-edged sword, most of the time, it's a good
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

## Quickcheck {data-type=sect1}

The tests we've discussed so far have themselves been extraordinarily
simple.

## Expect Tests {data-type=sect1}




## Coverage Testing {data-type=sect1}
