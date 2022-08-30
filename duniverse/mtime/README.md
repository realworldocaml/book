Mtime — Monotonic wall-clock time for OCaml
===========================================
v1.4.0+dune2

Mtime has platform independent support for monotonic wall-clock time
in pure OCaml. This time increases monotonically and is not subject to
operating system calendar time adjustments. The library has types to
represent nanosecond precision timestamps and time spans.

The additional Mtime_clock library provide access to a system
monotonic clock.

Mtime has a no dependency. Mtime_clock depends on your system library
or JavaScript runtime system. Mtime and its libraries are distributed
under the ISC license.

Home page: http://erratique.ch/software/mtime  

# Installation

Mtime can be installed with `opam`:

    opam install mtime

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation and API reference is automatically generated from
the source interfaces. It can be consulted [online][doc] or via
`odig doc mtime`.

[doc]: http://erratique.ch/software/mtime/doc/

# Sample programs

If you installed mtime with `opam` sample programs are located in
the directory `opam config var mtime:doc`.
