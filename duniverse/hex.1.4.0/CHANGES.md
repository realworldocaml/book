v1.4.0 2019-03-28
-----------------

* Drop the hard dependency on `unix` that is transitively
  brought in by cstruct/bigarray.  This is done by using
  the new `bigarray-compat` opam package that does the
  right thing on OCaml 4.07+ to not depend on unix.
  On older OCaml compilers the dependency will still be
  there and must be manually dropped with `-dontlink`, but
  we encourage users to use a newer compiler (#29 @TheLortex)

v1.3.0 2019-02-01
-----------------

* Add `to/of_bytes/bigstring` functions. (#27 @vbmithr)
* Install toplevel printers automatically in modern utop (@avsm)
* Port from jbuilder to dune (@avsm)
* Use `dune-release` instead of topkg (@avsm)
* Update opam metadata to 2.0 format (@avsm)
* Improve ocamldoc (@avsm)

v1.2.0 2017-11-05
-----------------

* Fix build with OCaml 4.06 (and -safe-string) (#25 @djs55)
* Add pretty-printers (#23 @vbmithr)
* Make jbuilder a build dependency (#22 @samoht)

v1.1.1 2017-05-26
-----------------

* Add topkg-jbuilder support.

v1.1.0 2017-05-23
-----------------

* Port build to [Jbuilder](https://github.com/janestreet/jbuilder) (#19 @avsm).
* Modernise Travis CI test matrix (#19 @avsm).
* Add `LICENSE` file (#18 @djs55).

v1.0.0 2015-10-13
-----------------

* Fix performance issues: make `of_string` less consy when `ignore` is empty
  (#13, fix by @yallop)
* Add missing `Bytes` dependency (#16)

v0.2.0 2015-05-04
------------------

* Add an `opam` file
* Add Travis CI files
* Add `Hex.of_cstruct` and `Hex.to_cstruct` to converters from and to cstructs
  (#5 by @trevorsummerssmith)
* Add `Hex.hexdump` to pretty-print an hex value (#6 by @trevorsummerssmith)
* Change the optional argument of `Hex.of_string` to take a list of characters
  to ignore (#6 by @trevorsummerssmith)

v0.1.0 2014-09-24
-----------------

* Initial release
