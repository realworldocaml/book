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
  (ocaml (and (>= 4.10.0) (< 4.11.0)))
  async
  atdgen
  base
  cmdliner
  cohttp-async
  conf-ncurses
  core
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
  yojson))

