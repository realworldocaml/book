# Testing {#testing data-type=chapter}

Testing is a fundamental part of building reliable software. And yet,
many software systems are under-tested. The reasons are clear
enough. Testing can be tedious, and in the early stages of a project,
it's often not obvious how important testing is going to become down
the line.

In some ways, OCaml's type-system makes this worse, by enhancing the
illusion that you can get by without testing.  After all, many trivial
bugs are caught cheaply by OCaml's type system without testing. But
make no mistake, type system or no type system, testing is still
essential.

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
- **Deterministic**. For test failures to be actionable, they need to be
  believable, which requires determinism.

Frameworks can't solve all of your testing problems, but they can help
you with all of the issues described above.

## Inline tests {data-type=sect1}

We're going to show you how to build tests using the `ppx_inline_test`
framework. This framework lets you add tests to any module in your
library by using a specially annotated let binding. To do that, we
need to enable the appropriate syntax extensions, as well as mark that
the files in this library contain inline tests. We do the first by
adding `ppx_jane` to the set of preprocessors, and the second by
adding an `inline_tests` declaration to the library stanza, as shown
below.

<link rel="import" href="code/testing/simple_inline_test/jbuild" />

We use `ppx_jane` in this example because it pulls in a collection of
useful syntax extensions with a single declartion, but it is also
possible to pull in just the specific preprocessors you want to use.

Any module in this library can contain a test, so in the following
we'll create a file called `test.ml` which contains only a single test.

<link rel="import" href="code/testing/simple_inline_test/test.ml" />
>
The test passes if the expression on the right-hand side of the
equals-sign evaluates to true.  These tests are not automatically run
with the instantiation of the module, but are instead registered for
running via the test runner, which can be invoked via jbuilder.

<link rel="import" href="code/testing/simple_inline_test/run.sh" />

Since the test was correct, it passes, generating no output. If we
modify the test to have an error, as below,

<link rel="import" href="code/testing/broken_inline_test/test.ml" />

then we will see an error when we run the test.

<link rel="import" href="code/testing/broken_inline_test/run.sh" />

### More readable errors with `test_eq` {data-type=sect2}

One problem with the test output above is that it doesn't print out
the data associated with the failed test. That's not really a problem
in this case, since the data in question is pretty clear. But in more
realistic tests, it's often to see the concrete data that failed.

We often do this by running a test that fails by throwing an
exception, rather than just returning false. That exception can then
contain more useful debugging information. To do this, we need to
change our top-level test to use `let%test_unit` instead of
`let%test`, so that the test allows a unit-returning body. We're also
going to use the `[%test_eq]` syntax, which, given a type, generates
code to test for equality and throw a meaningful exception if the
arguments are unequal.

Here's what our new test looks like.

<link rel="import" href="code/testing/test_eq-inline_test/test.ml" />

And here's what it looks like when we run a test.

<link rel="import" href="code/testing/test_eq-inline_test/run.sh" />
