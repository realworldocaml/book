Mtime — Monotonic wall-clock time for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Mtime has platform independent support for monotonic wall-clock time
in pure OCaml. This time increases monotonically and is not subject to
operating system calendar time adjustments. The library has types to
represent nanosecond precision timestamps and time spans.

The additional Mtime_clock library provide access to a system
monotonic clock.

Mtime has a no dependency. Mtime_clock depends on your system library.
The optional JavaScript support depends on [js_of_ocaml][jsoo]. Mtime
and its libraries are distributed under the ISC license.

[jsoo]: http://ocsigen.org/js_of_ocaml/

Home page: http://erratique.ch/software/mtime  
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`

## Installation

Mtime can be installed with `opam`:

    opam install mtime
    opam install js_of_ocaml mtime  # mtime with jsoo support

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is automatically generated from
the source interfaces. It can be consulted [online][doc] or via
`odig doc mtime`.

[doc]: http://erratique.ch/software/mtime/doc


## Sample programs

If you installed mtime with `opam` sample programs are located in
the directory `opam config var mtime:doc`.

In the distribution sample programs and tests are located in the
`test*` directories. They can be built and run with:

    topkg build --tests true && topkg test

