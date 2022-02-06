1.2
---

* Fix for js-of-ocaml (@hhugo)
* Bump minimal OCaml version to 4.03.0
* Add [@@ocaml.inline] annotations where relevant
* Add [@@ocaml.deprecated] annotations on EndianString.set_* functions

1.1
---

* Add the OPAM support for building the documentation
* Use the correct bytes_set primitive for OCaml >= 4.07.0
  (issue #21 fixed in #22 @hhugo)
* Fix tests on big endian architectures
  (issue #20 reported by @TC01 and @olafhering)
* Fix documentation typo (@bobot)
* Change cppo to a build dependency (@TheLortex)
* Port to Dune from jbuilder (@avsm)
* Upgrade opam metadata to 2.0 format (@avsm)
* Remove code for OCaml <4.01 support, as the minimum
  supported version is now OCaml 4.02+ (@avsm)
* Build with jbuilder (unreleased, superseded by dune)

1.0
---------------

* Install generated .mli files
* Build documentation
* Fix README links

0.8
---------------

* Replace optcomp with cppo, removing hard dependency on camlp4.

0.7
---------------

* Fix dependencies.

0.6
---------------

* Port to OCaml 4.02 -safe-string: Add an EndianBytes module.
* Add unoptimized get_float, get_double, set_float and set_double to every modules.
* Add a native endian version of interfaces.

0.5
---------------

* Fix to avoid problems with integers outside of the range [0; 255] with set_int8.
* Add travis CI files.

0.4
---------------

* Fix ocamlfind dependency on optcomp

0.3
---------------

First release.
