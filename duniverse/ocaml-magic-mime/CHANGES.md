v1.2.0 2021-07-13
-----------------

* Sync MIME types with latest from Apache (#22 @avsm)
* Add `mjs` mapping to application/javascript manually (@avsm, request from @glondu)

v1.1.3 2021-01-04
-----------------

* Fix build system for cross-compilation (@TheLortex, #19).
* Fix README (@seliopou, #18).
* Fix opam metadata (@CraigFe, #17).

v1.1.2 2019-08-12
-----------------

* Actually port to dune by converting the `generator/` directory from
  jbuild to dune (@avsm).

v1.1.1 2018-12-21
-----------------

* Port build to Dune and fix the embedded compilation when
  the repository is included as a subdirectory in a larger
  Dune build.
* Update opam metadata to 2.0 format.
* Switch to using `dune-release` instead of `topkg` for releases.

v1.1.0 2017-06-20
-----------------

* Support a `map_file` which maps filenames onto MIME types, using
  a database based on GTKSourceView lang files.  This adds support
  for several `x-*` MIME type extensions as well which are unofficial
  but useful, including ones for common programming languages.
  (#4 by @zoggy)

* Sync mime.types with Apache SVN and note its source in the README.

v1.0.1 2017-05-25
-----------------

* clarify LICENSE (ISC)
* build and test against OCaml 4.03 and 4.04
* build via jbuilder

v1.0.0 (2015-02-08)
-------------------

* Initial public release.
