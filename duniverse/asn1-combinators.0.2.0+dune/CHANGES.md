## dev

* Port build to Dune from ocamlbuild.
* Upgrade opam description file to 2.0 format.
* Use level-2 section headings for odoc compatability.

## v0.2.0 (2017-11-13)
* `OID`s are now fully abstract, with a simpler interface.
* `OID`s have custom comparison and hasing.
* `Time` is gone in favor of `Ptime`.
* `IMPLICIT` silently becomes `EXPLICIT` when necessary.
* Parse errors are reported through `Result`.
* Syntaxes now live in their own module, `Asn.S`.
* Rewrote the parser; no new features, but looks nicer from a distance.
* Various performance improvements.
* Documented the interface.

## v0.1.3 (2016-11-12)
* relicense to ISC
* drop oasis
* fix a bug in tests on 32 bit

## v0.1.2 (2015-05-02)
* cstruct-1.6.0 compatibility

## v0.1.1 (2014-10-30)
* stricter decoding of ints in BER/DER tags and OIDs
* performance improvements

## v0.1.0 (2014-07-08):
* initial (beta) release
