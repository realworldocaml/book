## v0.2.2 (2020-01-29)
* packaging improvements: add lower bound to dune dependency, improve test
  invocation, remove version from dune-project
  (reported by @kit-ty-kate in ocaml/opam-repository#15757 fixed by @hannesm)

## v0.2.1 (2020-01-28)
* disallow various constructs as suggested by ITU-T Rec X.690 (by @pqwy)
  * redundant OID component forms (X.690 8.20.2)
  * redundant integer forms (X.690 8.3.2)
  * empty integer (X.690 8.3.1, reported in #23 by @emillon)
  * constructed strings in DER
* deeper implict -> explicit over choice (follow-up to v0.2.0 entry, by @pqwy)
* handle long-form length overflow (reported in #24 by @emillon, fixed by @pqwy)
* disallow primitive with indefinite length (introduced in the bugfix above,
  reported by @emillon, fixed in #32 by @hannesm)
* disallow nonsensical bitstring unused values (X690 8.6.2, reported in #26
  by @NathanReb, fixed by @pqwy)
* fix non-continuous bit_string_flags (X680 22.6, reported in #25 by @wiml,
  fixed by @pqwy)
* use Alcotest instead of oUnit for unit tests (by @pqwy)
* use dune as build system (by @pqwy, superseeds #22)
* use bigarray-compat (#27 by @TheLortex) and stdlib-shims (#29 by @XVilka)
* raise lower bound to OCaml 4.05.0 (#31 by @hannesm)

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
