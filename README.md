Real World OCaml
----------------

Dependencies
============
Scripts to build the book and the website are implemented in
OCaml. Once you have [OPAM](https://opam.ocaml.org/) installed, it is
recommended you do the following to install all necessary
dependencies:

```
opam switch rwo -A 4.04.1
cd /to/your/working/directory
opam pin add rwo .
opam install rwo --deps-only
```

The `switch` step is optional, but recommended if you are also working
on other OCaml projects that may have conflicting requirements.


Directory Structure
===================
* `book/`
  Main content of the book. An html file for each chapter along with
  supporting images and code blocks.

  - `pdf/` and `theme/`
    Copied from the O'Reilly sources. Unsure yet if these are needed.

* `lib/`
  OCaml code implementing all the functionality needed to build the
  book into various formats.

* `app/`
  Command line app, for convenient access to functions implemented in
  lib/.

* `archive/`
  Mostly stuff from the 1st edition of the book. Could delete all of
  this as it is accessible by going to tag
  [pre-2e](https://github.com/yminsky/Real-World-OCaml/releases/tag/pre-2e).
  However, leaving here for now for more clear access until we're sure
  we've ported everything needed.

* `exercises/`
  Unused exercises.
