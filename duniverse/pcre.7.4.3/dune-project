(lang dune 1.10)
(name pcre)

(generate_opam_files true)

(source (github mmottl/pcre-ocaml))
(license "LGPL-2.1+ with OCaml linking exception")
(homepage "https://mmottl.github.io/pcre-ocaml")
(documentation "https://mmottl.github.io/pcre-ocaml/api")

(maintainers "Markus Mottl <markus.mottl@gmail.com>")
(authors "Markus Mottl <markus.mottl@gmail.com>")

(package
  (name pcre)
  (synopsis "Bindings to the Perl Compatibility Regular Expressions library")
  (description "\
pcre-ocaml offers library functions for string pattern matching and
substitution, similar to the functionality offered by the Perl language.")
  (depends
    (ocaml (>= 4.08))
    (dune (>= 1.10))
    (conf-libpcre :build)
    (base :build)
    base-bytes
  )
)
