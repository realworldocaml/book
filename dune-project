(lang dune 2.4)
(name rwo)

(using mdx 0.1)

(formatting disabled)

(generate_opam_files true)

(source (github realworldocaml/book))
(license ISC)
(authors "Anil Madhavapeddy" "Yaron Minsky")
(maintainers "anil@recoil.org")

(package
 (name rwo)
 (synopsis "Real World OCaml book sources")
 (description "Scripts and sources to compile the Real World OCaml
second edition book. See https://realworldocaml.org/")
 (depends
  (ocaml (and (>= 4.09.0) (< 4.10.0))
)))

