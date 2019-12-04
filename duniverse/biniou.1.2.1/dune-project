(lang dune 1.10)
(name biniou)

(generate_opam_files true)
(license "BSD-3-Clause")
(maintainers "martin@mjambon.com")
(authors "Martin Jambon")
(implicit_transitive_deps false)
(source (github mjambon/biniou))
(documentation "https://mjambon.github.io/biniou/")

(package
 (name biniou)
 (synopsis
   "Binary data format designed for speed, safety, ease of use and backward compatibility as protocols evolve")
 (description "
Biniou (pronounced \"be new\") is a binary data format designed for speed, safety,
ease of use and backward compatibility as protocols evolve. Biniou is vastly
equivalent to JSON in terms of functionality but allows implementations several
times faster (4 times faster than yojson), with 25-35% space savings.

Biniou data can be decoded into human-readable form without knowledge of type
definitions except for field and variant names which are represented by 31-bit
hashes. A program named bdump is provided for routine visualization of biniou
data files.

The program atdgen is used to derive OCaml-Biniou serializers and deserializers
from type definitions.

Biniou format specification: mjambon.github.io/atdgen-doc/biniou-format.txt")
 (depends
  easy-format
  (dune (>= 1.10))
  (ocaml (>= "4.02.3"))))
