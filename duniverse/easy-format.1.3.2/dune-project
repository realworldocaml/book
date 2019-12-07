(lang dune 1.10)
(name easy-format)

(source (github mjambon/easy-format))
(generate_opam_files true)
(license "BSD-3-Clause")
(maintainers "martin@mjambon.com" "rudi.grinberg@gmail.com")
(authors "Martin Jambon")
(implicit_transitive_deps false)
(documentation "https://mjambon.github.io/easy-format/")

(package
 (synopsis "High-level and functional interface to the Format module of the OCaml standard library")
 (description "
This module offers a high-level and functional interface to the Format module of
the OCaml standard library. It is a pretty-printing facility, i.e. it takes as
input some code represented as a tree and formats this code into the most
visually satisfying result, breaking and indenting lines of code where
appropriate.

Input data must be first modelled and converted into a tree using 3 kinds of
nodes:

* atoms
* lists
* labelled nodes

Atoms represent any text that is guaranteed to be printed as-is. Lists can model
any sequence of items such as arrays of data or lists of definitions that are
labelled with something like \"int main\", \"let x =\" or \"x:\".")
 (name easy-format)
 (depends
  (dune (>= 1.10))
  (ocaml (>= "4.02.3"))))
