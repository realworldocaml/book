(lang dune 1.11)
(name graphics)

(generate_opam_files true)
(using fmt 1.2)

(license "LGPL-2.1 with OCaml linking exception")
(maintainers jeremie@dimino.org david.allsopp@metastack.com)
(authors "Xavier Leroy"
         "Jun Furuse"
         "J-M Geffroy"
         "Jacob Navia"
         "Pierre Weis")
(source (github ocaml/graphics))
(documentation https://ocaml.github.io/graphics/)

(package
 (name graphics)
 (depends
   "conf-libX11"
  ("ocaml" (>= "4.09.0~~")))
 (synopsis "The OCaml graphics library")
 (description "\
The graphics library provides a set of portable drawing
primitives. Drawing takes place in a separate window that is created
when Graphics.open_graph is called.

This library used to be distributed with OCaml up to OCaml 4.08.
"))
