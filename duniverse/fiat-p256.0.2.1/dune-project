(lang dune 1.10)
(using fmt 1.1)
(generate_opam_files true)
(name fiat-p256)
(source (github mirage/fiat))
(documentation "https://mirage.github.io/fiat/doc")
(license MIT)
(maintainers "Etienne Millon <me@emillon.org>")
(authors
 "Etienne Millon <me@emillon.org>"
 "Andres Erbsen <andreser@mit.edu>"
 "Google Inc."
 "Jade Philipoom <jadep@mit.edu> <jade.philipoom@gmail.com>"
 "Massachusetts Institute of Technology")
(package
 (name fiat-p256)
 (depends
  (alcotest :with-test)
  (asn1-combinators :with-test)
  (benchmark :with-test)
  bigarray-compat
  (cstruct (>= "3.5.0"))
  (dune (>= "1.10.0"))
  dune-configurator
  (eqaf (>= "0.5"))
  hex
  (ppx_deriving_yojson :with-test)
  (rresult :with-test)
  (stdlib-shims :with-test)
  (yojson (and :with-test (>= "1.6.0"))))
 (depopts mirage-xen-posix ocaml-freestanding)
 (conflicts
  (mirage-xen-posix (< 3.1.0))
  (ocaml-freestanding (< 0.4.1)))
 (synopsis "Primitives for Elliptic Curve Cryptography taken from Fiat")
 (description
   "\> This is an implementation of the ECDH over P-256 key exchange algorithm, using
   "\> code from Fiat (<https://github.com/mit-plv/fiat-crypto>).
   "\>
   "\> Cryptographic primitives should not be used in end applications, they are better
   "\> used as part of a cryptographic library.
 )
 ; tags are not included before (lang dune 2.0)
 ; so an opam template is necessary until then
 (tags (org:mirage)))
