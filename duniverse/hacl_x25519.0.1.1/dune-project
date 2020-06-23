(lang dune 1.10)
(name hacl_x25519)
(using fmt 1.1)
(generate_opam_files true)
(source (github mirage/hacl))
(documentation https://mirage.github.io/hacl/doc)
(license MIT)
(maintainers "Etienne Millon <me@emillon.org>")
(authors
 "Etienne Millon <me@emillon.org>"
 "INRIA and Microsoft Corporation")
(package
 (name hacl_x25519)
 (depends
  (benchmark :with-test)
  (cstruct (>= 3.5.0))
  (dune (>= 1.10.0))
  eqaf
  (hex :with-test)
  ocaml
  (ppx_blob :with-test)
  (ppx_deriving_yojson :with-test)
  (stdlib-shims :with-test)
  (yojson (and :with-test (>= 1.6.0))))
 (depopts mirage-xen-posix ocaml-freestanding)
 (conflicts
  (mirage-xen-posix (< 3.1.0))
  (ocaml-freestanding (< 0.6.0)))
 (synopsis "Primitives for Elliptic Curve Cryptography taken from Project Everest")
 (description
   "\> This is an implementation of the X25519 key exchange algorithm, using code from
   "\> Project Everest.
   "\>
   "\> Cryptographic primitives should not be used in end applications, they are better
   "\> used as part of a cryptographic library.
   ))
