# Testing {#testing data-type=chapter}


OCaml's type system is a great tool for improving the correctness of
your code and catching bugs at the earliest possible stage; but it's
no replacement for testing. Testing is a fundamental part of real
world software engineering, and it's one that you want your tools and
your language to support you in.

Good testing infrastructure is important because it encourages you to
test more. Testing can be tedious, and you can often get away without
it, for a while at least. But writing a lot of code without adequate
testing is a recipe for headaches down the road.

To combat this, you need to reduce the tedium, make testing easy and
fun. If you do, you'll find yourself and your teammates writing way
more tests, and ending up with a more reliable system as a result.

Happily, over the years a lot of testing infrastructure has grown up
for OCaml that makes testing massively easier and more pleasant. This
chapter is going to teach you how that infrastructure works, and show
you how to leverage it to make your code more reliable.

## Types of tests {#ypes-of-tests data-type=sect1}

Before we start discussing testing infrastructure, it's worth
discussing some of the kinds of tests that you might want to be able
to run. It's worth noting that these categories aren't precise, and
they definitely don't partition the space of possibilities, e.g.,
something can be oth a property test and a unit test.  But they do
give you a flavor of some of the different kinds of testing you might
need to do.

- *Unit tests* are tests that aim to exercise, small, independent
  units of your code-base in isolation.
- *Functional tests*
- *Regression tests*
- *Property tests*
- *Integration tests*

## What makes for a good test? {#what-makes-for-a-good-test data=type=sect1}

Some of the properties of good tests:

- *Easy to write.* The easier it is to write a test, the more tests
  you'll find yourself writing. It's as simple as that.
- *Easy to run.* Indeed, ideally you want your tests to be run
  automatically as part of your development and deployment process. If
  you're working in a context with many people, you want to prevent
  people from releasing changes that break the test suite.
- *Easy to update*
- *Exhaustive*
- *Fast*
- *Readable*
- *Deterministic*
