(lang dune 2.9)
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
  (ocaml (and (>= 4.14.0) (< 4.15.0)))
  alcotest
  async
  async_graphics
  atdgen
  base
  (cmdliner (>= 1.1.0))
  cohttp-async
  conf-ncurses
  conf-tzdata
  (core (>= v0.15.0))
  core_bench
  ctypes
  ctypes-foreign
  fmt
  lambdasoup
  mdx
  ocaml-compiler-libs
  ocaml-print-intf
  patdiff
  pcre
  ppx_jane
  re
  sexp_pretty
  textwrap
  tls
  yojson
  conf-pandoc
  conf-texlive))
