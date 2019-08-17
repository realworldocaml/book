ppxlib's user manual
====================

Overview
--------

This is the user manual for ppxlib, the core of the ppx
meta-programming system for OCaml_ and its derivatives such as
Reason_. This manual is aimed at both users and authors of ppx
rewriters and contains everything one should know in order to use or
write ppx rewriters.

It is assumed in this manual that the user is familiar with the Dune_
build system. In particular, all the examples in this manual referring
to the build system will present Dune_ configurations files and
commands. It is possible to use ppxlib with other build systems,
however this is not covered by this manual.

.. _OCaml:  https://ocaml.org/
.. _Dune:   https://dune.build/
.. _Reason: https://reasonml.github.io/

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   what-is-ppx
   ppx-for-end-users
   ppx-for-plugin-authors

Indices and tables
------------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
