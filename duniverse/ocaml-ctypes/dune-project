(lang dune 2.6)
(name ctypes)
(formatting (enabled_for dune))

(generate_opam_files true)

(license MIT)
(maintainers "Jeremy Yallop <yallop@gmail.com>")
(authors "Jeremy Yallop")
(source (github ocamllabs/ocaml-ctypes))
(documentation "https://ocamllabs.github.io/ocaml-ctypes/")

(package
 (name ctypes)
 (tags (org:mirage org:ocamllabs))
 (depends
  (ocaml (>= 4.02.3))
  integers
  dune-configurator
  (bisect_ppx (>= 2.0.0))
  (ounit :with-test))
 (synopsis "Combinators for binding to C libraries without writing any C")
 (description "
ctypes is a library for binding to C libraries using pure OCaml. The primary
aim is to make writing C extensions as straightforward as possible.
The core of ctypes is a set of combinators for describing the structure of C
types -- numeric types, arrays, pointers, structs, unions and functions. You
can use these combinators to describe the types of the functions that you want
to call, then bind directly to those functions -- all without writing or
generating any C!

To install the optional `ctypes.foreign` interface (which uses `libffi` to
provide dynamic access to foreign libraries), you will need to also install
the `ctypes-foreign` package.

    opam install ctypes-foreign

This will make the `ctypes-foreign` ocamlfind subpackage available."))

(package
 (name ctypes-foreign)
 (tags (org:mirage org:ocamllabs))
 (depends
  (ocaml (>= 4.02.3))
  (ctypes (= :version))
  dune-configurator
  conf-pkg-config
  (bisect_ppx (>= 2.0.0))
  (lwt :with-test)
  (ounit :with-test)
  (conf-ncurses :with-test)
  (stdlib-shims :with-test)
  bisect_ppx)
 (synopsis "Dynamic access to foreign C libraries using Ctypes")
 (description "
This installs the `ctypes-foreign` interface which
uses `libffi` to provide dynamic access to foreign libraries."))
