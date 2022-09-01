(lang dune 1.10)
(name cppo)

(generate_opam_files true)

(source (github ocaml-community/cppo))
(license BSD-3-Clause)
(authors "Martin Jambon")
(maintainers
 "Martin Jambon <martin@mjambon.com>"
 "Yishuai Li <yishuai@upenn.edu>")
(documentation "https://ocaml-community.github.io/cppo")

(package
 (name cppo)
 (depends
  (ocaml (>= 4.02.3))
  (dune  (>= 1.10))
   base-unix)
 (synopsis "Code preprocessor like cpp for OCaml")
 (description "Cppo is an equivalent of the C preprocessor for OCaml programs.
It allows the definition of simple macros and file inclusion.

Cppo is:

* more OCaml-friendly than cpp
* easy to learn without consulting a manual
* reasonably fast
* simple to install and to maintain
"))

(package
 (name cppo_ocamlbuild)
 (depends
   ocaml
  (dune (>= 1.10))
   ocamlbuild
   ocamlfind)
 (synopsis "Plugin to use cppo with ocamlbuild")
 (description "This ocamlbuild plugin lets you use cppo in ocamlbuild projects.

To use it, you can call ocamlbuild with the argument `-plugin-tag
package(cppo_ocamlbuild)` (only since ocaml 4.01 and cppo >= 0.9.4).
"))
