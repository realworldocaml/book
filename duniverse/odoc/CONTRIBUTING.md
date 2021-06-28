# Contributing to odoc

Please ask any questions you have about odoc, [open any issues][issues],
[offer feedback][contact], etc. All of these are valued contributions :)

If you'd like specifically to work on the code of odoc, we hope that you will
find the information in this file helpful.

[contact]: https://github.com/ocaml/odoc#contact

<br/>

#### Table of contents

- [Quick start: HTML and CSS](#Quick_start)
- [Testing](#Testing)
  - [Debug prints](#Debug_prints)
  - [Expect tests](#Expect_tests)
  - [Coverage analysis](#Coverage_analysis)
- [Project structure](#Project_structure)
- [Roadmap](#Roadmap)
  - [Project status](#Project_status)
  - [General direction](#General_direction)
  - [Not supported in the near term](#Not_supported_in_the_near_term)
  - [Releases](#Releases)
  - [Issue organization](#Issue_organization)

<br/>

<a id="Quick_start"></a>
## Quick start: HTML and CSS

The odoc CSS is found at [`src/odoc/etc/odoc.css`][css-file]. It needs a lot of
work, and PRs are very welcome. You can edit CSS using your browser's developer
mode, then send us a PR for the same changes made to this file.

Working on the HTML is more involved. The main HTML generator is in
[`src/html/to_html_tree.ml`][to-html-tree]. This has one function for each kind
of OCaml language item that needs to be displayed in docs.

To make edits to the HTML generation, run the following commands:

1. Set up for development:

    ```
    git clone https://github.com/ocaml/odoc.git
    cd odoc
    opam pin add --no-action odoc .
    opam install --deps-only odoc
    ```

    Also make sure to install a recent version of
    [tidy](http://www.html-tidy.org/) (used for HTML validity testing):

    ```
    # On MacOS (should be version 5.6.0 by the date of this writing)
    brew install tidy-html5

    # Debian / Ubuntu
    sudo apt-get install tidy
    ```

2. Make changes to the code. To compile it,

    ```
    make
    ```

    To run the repo's tests,

    ```
    make test
    ```

    For smaller changes, you don't have to make the repo's tests pass. The
    change having the right effect on your use-case is more important.

    There could be a lot of failures due to how thorough the repo test suite is
    in places, and we can update the tests for you by pushing into your PR. For
    larger changes, see [Testing](#Testing) below.

3. To test odoc against your own project, install it

    ```
    make clean
    opam install odoc
    ```

   Since odoc is pinned, this installs your modified version. Then, you can run
   odoc in your project as normal:

    ```
    dune build @doc
    ```

4. If all looks good, send odoc a PR :)

[css-file]: https://github.com/ocaml/odoc/blob/master/src/odoc/etc/odoc.css
[to-html-tree]: https://github.com/ocaml/odoc/blob/master/src/html/to_html_tree.ml

<br/>

<a id="Testing"></a>
## Testing

The basic testing instructions are covered in [Quick start](#Quick_start), but
here is some more detail on odoc's testing setup.

<br/>

<a id="Debug_prints"></a>
### Debug prints

If you want to display something during the execution of the tests, write to
STDERR with [`prerr_endline`][prerr_endline] or [`Printf.eprintf`][eprintf].
The [testing framework][alcotest] will display STDERR if a test fails.

[prerr_endline]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VALprerr_endline
[eprintf]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html#VALeprintf
[alcotest]: https://github.com/mirage/alcotest

<br/>

<a id="Expect_tests"></a>
### Expect tests

Most of odoc's tests are *expect tests*, which means that they convert output
of some code that is being tested to strings, and then check that those strings
are correct:

1. The tests run some code, for example the odoc parser on the string `{e foo}`.
2. They take the output, in this case an AST representing "emphasized `foo`,"
   and convert that output to a string. In this case, it will be an S-expression
   roughly like `(emphasis (foo))`.
3. There is an *expected* copy of this S-expression in a file somewhere in the
   repo. If the S-expression from the code doesn't match the expected one, the
   test fails.

The reason for using expect tests is that when a test fails, there are two
possibilities:

1. The code being tested has become wrong, in which case the *first* failure
   should trigger fixing the code.
2. The code being tested has been changed in some way, but is correct (perhaps
   more correct than it used to be), and it is the test case that is wrong. It
   is possible that *dozens* or even *hundreds* of tests are now wrong. It is
   not practical to fix them fully by hand.

When an expect test fails, the string that the code emitted is saved, so that
the human developer can choose to *replace* the now-incorrect expected string.
In odoc, a test faiilure looks like this:

```
-- bold.000 [basic.] Failed --
in _build/_tests/bold.000.output:

{e foo}

--- expect/bold/basic.txt       2018-04-15 14:42:32.356895400 -0500
+++ _actual/bold/basic.txt      2018-04-15 17:36:26.812747400 -0500
@@ -2,5 +2,5 @@
   (ok
    (((f.ml (1 0) (1 7))
      (paragraph
-      (((f.ml (1 0) (1 7)) (bold (((f.ml (1 3) (1 6)) (word foo)))))))))))
+      (((f.ml (1 0) (1 7)) (emphasis (((f.ml (1 3) (1 6)) (word foo)))))))))))
  (warnings ()))

To replace expected output with actual, run

bash _build/default/test/parser/_actual/replace.sh
```

The intended response to this is:

1. Check the diff. If the `-` line is correct, the code is broken. If the `+`
   line is correct, the test is broken.
2. If the test is broken, copy/paste the command that the output suggests,
   and re-run the tests:

    ```
    bash _build/default/test/parser/_actual/replace.sh; make test
    ```

   This command is the same within one test category (e.g. HTML tests, parser
   tests), so if you have lots of tests to fix, you paste it once, then use
   UP, ENTER to repeat it over and over again, quickly checking each failure.

<br/>

<a id="Coverage_analysis"></a>
### Coverage analysis

The odoc repo is set up for coverage analysis. This is most useful if you're
writing new tests, and want to know what they are actually touching. To use it,

1. Run `make clean` once, before beginning to work with coverage. This rebuilds
   odoc with Bisect_ppx linked in.

2. Run `make coverage`. This will run the tests as normal, except at the end you
   will get a message like

    ```
    Coverage summary: 1914/2594 (73.79%)
    See _coverage/index.html
    ```

   You can then open `_coverage/index.html` and see the coverage of the code you
   would like your new test to reach. It is possible that it is already covered
   "accidentally" by tests that are checking other properties, however, in which
   case coverage analysis will not be very useful :)

3. Write new tests.

4. Check coverage again.

<br/>

<a id="Project_structure"></a>
## Project structure

odoc is divided into several sub-libraries, each of which is a directory
under `src/`. Most of these have a *main file*, whose name is the directory
name prefixed with "`odoc__`". That main file is the interface for the entire
sub-library directory. For example, [`src/parser`][parser-dir] has
[`src/parser/odoc__parser.mli`][parser-api], and everything in `src/parser` is
hidden behind that interface.

We use an alias module, in [`src/alias/odoc__alias.ml`][alias] to shorten these
names in odoc's own code. For example, as you can see in the alias module, we
`Odoc__parser` is shortened to `Parser_`. The underscore is there to avoid
needlessly shadowing OCaml's module `Parser`, which is part of `compiler-libs`.

The `dune` files in each directory can be used to figure out how the
directories depend on each other. Mostly, however, everything depends on
`model`, and `odoc` depends on everything.

The directories are:

- [`src/compat`][compat-api] &mdash; backports of functions to old versions of
OCaml.

- [`src/model`][model-dir] &mdash; datatypes representing the OCaml
language ([`src/model/lang.ml`][lang]), error-handling
([`src/model/error.ml`][error]), cross-references
([`src/model/paths-types.ml`][paths]), etc. This directory actually has no main
file. It is a collection of the datatypes that the rest of the odoc
sub-libraries use to communicate with each other, so everything else depends on
`model`.

- [`src/loader`][loader-dir] &mdash; functions from `cmt`, `cmti`, `cmi` files
to `model`. You can see the three functions' signatures in the main file,
[`src/loader/odoc__loader.mli`][loader-api].

- [`src/parser`][parser-dir] &mdash; a single function from strings to comment
ASTs. You can see its signature in the main file,
[`src/parser/odoc__parser.mli`][parser-api].

- [`src/xref`][xref-dir] &mdash; functions for resolving cross-references. These
consume things from `model`, and return transformed instances. The signature, in
[`src/xref/odoc__xref.mli`][xref-api] is not very pretty, but the usage of
`xref` is pretty isolated in the rest of odoc, and can be found by grepping for
`Xref`.

- [`src/html`][html-dir] &mdash; the HTML generator. The main file is
[`src/html/odoc__html.mli`][html-api].

- [`src/odoc`][odoc-dir] &mdash; the overall `odoc` command-line tool that ties
the other parts together. This doesn't have the same kind of main file, because
what's generated from this is the odoc executable, not a sub-library. The entry
point for the executable is [`src/odoc/bin/main.ml`][main].

- [`src/util`][util-dir] is for things that help with the development of odoc,
but aren't part of the regular build, and [`src/vendor`][vendor-dir] is for
third-party software.

[compat-api]: https://github.com/ocaml/odoc/blob/master/src/compat/odoc__compat.ml
[model-dir]: https://github.com/ocaml/odoc/tree/master/src/model
[lang]: https://github.com/ocaml/odoc/blob/master/src/model/lang.ml
[error]: https://github.com/ocaml/odoc/blob/master/src/model/error.ml
[paths]: https://github.com/ocaml/odoc/blob/master/src/model/paths_types.ml
[parser-dir]: https://github.com/ocaml/odoc/tree/master/src/parser
[parser-api]: https://github.com/ocaml/odoc/blob/master/src/parser/odoc__parser.mli
[loader-dir]: https://github.com/ocaml/odoc/tree/master/src/loader
[loader-api]: https://github.com/ocaml/odoc/blob/master/src/loader/odoc__loader.mli
[xref-dir]: https://github.com/ocaml/odoc/tree/master/src/xref
[xref-api]: https://github.com/ocaml/odoc/blob/master/src/xref/odoc__xref.mli
[html-dir]: https://github.com/ocaml/odoc/tree/master/src/html
[html-api]: https://github.com/ocaml/odoc/blob/master/src/html/odoc__html.ml
[odoc-dir]: https://github.com/ocaml/odoc/tree/master/src/odoc
[main]: https://github.com/ocaml/odoc/blob/master/src/odoc/bin/main.ml
[util-dir]: https://github.com/ocaml/odoc/tree/master/src/util
[vendor-dir]: https://github.com/ocaml/odoc/tree/master/src/vendor

The tests parallel the structure of `src/`:

- [`test/parser`][test-parser] is expect tests for the parser. Each [one]
[parser-test] calls the parser on a string, converts the AST to a string, and
compares it with an [expected string][parser-expect].

- [`test/html`][test-html] is end-to-end expect tests for the HTML generator.
Each [one][html-test] is an OCaml source file. The tester runs the `odoc` tool
on it, and compares the resulting HTML to some [expected HTML][html-expect].

- [`test/print`][test-print] is converters from odoc datatypes to strings, so
they can be used in expect tests.

- [`test/dune`][test-dune] is a tiny project for checking that Dune drives odoc
correctly. It is mainly used in the odoc CI.

- [`test/inactive`][test-inactive] is some old tests that have suffered some bit
rot, and we haven't gotten around to restoring yet.

[test-parser]: https://github.com/ocaml/odoc/blob/master/test/parser/test.ml
[parser-test]: https://github.com/ocaml/odoc/blob/4c09575a5b25f4b224322f25d7867ce41fa4d032/test/parser/test.ml#L35
[parser-expect]: https://github.com/ocaml/odoc/blob/4c09575a5b25f4b224322f25d7867ce41fa4d032/test/parser/expect/one-paragraph/word.txt
[test-html]: https://github.com/ocaml/odoc/blob/master/test/html/test.ml
[html-test]: https://github.com/ocaml/odoc/blob/master/test/html/cases/val.mli
[html-expect]: https://github.com/ocaml/odoc/blob/master/test/html/expect/val.html
[test-print]: https://github.com/ocaml/odoc/tree/master/test/print
[test-dune]: https://github.com/ocaml/odoc/tree/master/test/dune
[test-inactive]: https://github.com/ocaml/odoc/tree/master/test/inactive

<br/>

<a id="Roadmap"></a>
## Roadmap

Everything here is subject to your input. Please discuss the roadmap in [#210, the roadmap issue][roadmap-issue].

[roadmap-issue]: https://github.com/ocaml/odoc/issues/210

<br/>

<a id="Project_status"></a>
### Project status

odoc is currently in **beta**. We aim for odoc to be good for diverse use cases
*in the future*, but for now we are focused on fast development satisfying
limited goals.

<br/>

<a id="General_direction"></a>
### General direction

The current goal of odoc is to become more useful for single projects. This
includes:

- **Quality of output** &mdash; Emitting good HTML, with usability features such
as whole-project search, etc. See the
[**Usability project**][usability-project].
- **Build integration** &mdash; Good interop with Dune for the OCaml and Reason
native ecosystems, and BuckleScript for the Reason/BuckleScript ecosystem. See
the [**Reason and BuckleScript project**][re-bs-project]. The Dune integration
is handled in the [Dune repo][dune].

Eventually, we want to start generating centralized docs for the entire OCaml
(and/or Reason) ecosystem, and hosting them at docs.ocaml.org. We are not
focused on this right now.

[usability-project]: https://github.com/ocaml/odoc/projects/1
[re-bs-project]: https://github.com/ocaml/odoc/projects/2
[dune]: https://github.com/ocaml/dune

<br/>

<a id="Not_supported_in_the_near_term"></a>
### Not supported in the near term

We'd like to support most of these things *eventually*, but the code base is
not ready for them, or we don't have enough time to implement them in the near
term. They are:

- The ability to emit HTML fragments.
- Compatibility with odig or other tools that drive odoc, besides the build
  systems Dune and bsb.
- Stable markup at the HTML level.
- Explicit custom themes.

<br/>

<a id="Releases"></a>
### Releases

We plan to release features fairly regularly (perhaps at most every 1-3 months).

odoc uses [**milestones**][milestones] for planned releases, with lists of
outstanding issues that they are to include. Note that many issues that have
already been resolved might not have been assigned to a milestone, but will
still be released.

If you'd like an issue to be added, please comment in it!

<br/>

<a id="Issue_organization"></a>
### Issue organization

- [**Milestones**][milestones] keep track of outstanding issues that definitely
  need to be done for a certain release.
- [**Projects**][projects] are long-term categories of issues. Visit each one,
  and you can see progress at a glance.
- We use several **labels** to give developers an idea of what each issue
  involves at a glance. See the [list of labels][labels], but they are really
  meant just to appear in the [issues list][issues] and be clickable.
- The [**good first issue**][easy-issues] label is meant to help new
  contributors find something they can get started with.

[milestones]: https://github.com/ocaml/odoc/milestones
[projects]: https://github.com/ocaml/odoc/projects
[labels]: https://github.com/ocaml/odoc/labels
[issues]: https://github.com/ocaml/odoc/issues
[easy-issues]: https://github.com/ocaml/odoc/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22
