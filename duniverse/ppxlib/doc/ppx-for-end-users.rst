*****************
PPX for end users
*****************

This section describes how to use ppx rewriters for end users.

Using a ppx rewriter in your project
------------------------------------

To use one or more ppx rewriters written by you or someone else,
simply list them in the ``preprocess`` field of your ``dune`` file. For
instance:

.. code:: scheme

          (library
           (name my_lib)
           (preprocess (pps (ppx_sexp_conv ppx_expect))))

Some ppx rewriters takes parameters in the form of command line
flags. These can be specified using the usual convention for command
line flags: atoms starting with ``-`` are treated as flags and ``--``
can be used to separate ppx rewriter names from more command line
flags. For instance:

.. code:: scheme

          (library
           (name my_lib)
           (preprocess
            (pps (ppx_sexp_conv ppx_expect -inline-test-drop))))

          (library
           (name my_lib)
           (preprocess
            (pps (ppx_sexp_conv ppx_expect -- --cookie "x=42"))))

Once this is done, you can use whatever feature is offered by the ppx
rewriter.

Looking at the generated code
-----------------------------

At the time of writing this manual, there is no easy way to look at
the fully transformed input file in order to see exactly what will be
compiled by OCaml. You can however use the following method, which is
not great but works: run ``ocamlc -dsource
_build/default/<input-file-with-.pp.ml-extension>``. For instance to
see the transformed version of ``src/foo.ml``, run:

.. code:: sh

          $ ocamlc -dsource _build/default/src/foo.pp.ml

[@@deriving_inline]
-------------------

Ppxlib supports attaching the ``[@@deriving]`` attribute to type
declaration. This is used to generate code at compile time based on
the structure of the type. For this particular case, ppxlib supports
an alternative way to look at the generated code: replace
``[@@deriving <derivers>]`` by ``[@@deriving_inline
<derivers>][@@@end]``. Then run the following command:

.. code:: sh

          $ dune build --auto-promote

If you reload the file in your editor, you should now see the contents
of the generated code between the ``[@@deriving_inline]`` and
``[@@@end]`` attribute. This can help understanding what is provided
by a ppx rewriter or debug compilation errors.

Dropping ppx dependencies with [@@deriving_inline]
--------------------------------------------------

You might notice that the resulting file when using
``[@@deriving_inline]`` needs no special treatment to be compiled. In
particular, you can build it without the ppx rewriter or even
ppxlib. You only need them while developing the project, in order to
automatically produce the generated code but that's it. End users of
your project do not need to install ppxlib and other ppx rewriters
themselves.

Dune_ gracefully supports this workflow: simply replace ``preprocess``
in your ``dune`` file by ``lint``. For instance:

.. code:: scheme

          (library
           (name my_lib)
           (lint (pps (ppx_sexp_conv))))

Then to regenerate the parts between ``[@@deriving_inline]`` and
``[@@@end]``, run the following command:

.. code:: sh

          $ dune build @lint --auto-promote

.. _Dune:   https://dune.build/
