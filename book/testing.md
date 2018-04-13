# Testing {#testing data-type=chapter}

Testing is a fundamental part of building reliable software. And yet,
many software systems are under-tested. The reasons are clear
enough. Testing can be tedious, and the need for testing is often not
apparent in the early stages of a project, so people often skimp on
it, and adding testing after the fact is a big hurdle that people
often never get to.

In some ways, OCaml makes this worse, by enhancing the illusion that
you can get away without testing.  After all, many trivial bugs are
caught cheaply and without testing by OCaml's type system. But make no
mistake, type system or no type system, testing is still essential.

A good way of motivating yourself to write tests is to make it easy
and fun. Testing tools can reduce the tedium of writing tests and
simplify the process of maintaining them.  With better infrastructure
in place, you'll find yourself writing way more tests, and your
creations will be more reliable as a result.

The goal of this chapter is to teach you about some of the available
infrastructure for testing in OCaml. But before that, we're going to
discuss in a language-agnostic way about what makes for good tests,
and what kind of automation you should be looking for. Then we'll turn
back to what tools are available specifically for OCaml.

## What makes for good tests? {#what-makes-for-good-tests data-type=sect1}

Here are some of the properties we want from our tests.

- Tests should be *easy to write*. The better the ease of use, the
  more energy you'll find yourself and your collaborators putting into
  writing tests.
- Tests should be *easy to run*. Ideally, they should be run
  automatically, every time you push changes. In a context where
  you're collaborating with many people, you want controls that
  prevent breaking changes from being released.
- Tests should *execute quickly*, so they don't slow down your
  development process.
- *Updating tests should be easy*. If tests are complicated to adjust
  in the face of changes to your code, they can become their own form
  of technical debt, acting as grit in the wheels.
- Part of that means your tests should be *readable*, so that someone
  can go back later and understand what the test is for.
- Tests need to be *deterministic*. Non-deterministic tests interfere
  with automation, and make it hard to take a test failure seriously.
  For test failures to be actionable, they need to be believable,
  which requires determinism.
- Tests should have *high coverage*, exercising enough of the
  functionality of your program to be likely to catch errors when
  they're introduced.

When you start thinking about how to approach testing your
application, one barrier is just understanding all of the terminology
surrounding testing and testing frameworks.  Do I want a regression
test or a unit test? Should we b doing functional tests or smoke
tests?

The first thing to understand about testing terminology is that they
don't have


- *Unit tests* are tests that aim to exercise, small, independent
  units of your code-base in isolation.
- *Functional tests*
- *Regression tests*
- *Property tests*
- *Integration tests*


It's worth noting that these categories aren't precise, and they
definitely don't partition the space of possibilities, e.g., something
can be both a property test and a unit test.  But they do give you a
flavor of some of the different kinds of testing you might need to do.
