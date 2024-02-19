Mtime — Monotonic wall-clock time for OCaml
===========================================
2.0.0+dune

Mtime has platform independent support for monotonic wall-clock time
in pure OCaml. This time increases monotonically and is not subject to
operating system calendar time adjustments. The library has types to
represent nanosecond precision timestamps and time spans.

The additional Mtime_clock library provide access to a system
monotonic clock.

Mtime has a no dependency. Mtime_clock depends on your system library
or JavaScript runtime system. Mtime and its libraries are distributed
under the ISC license.

Home page: <http://erratique.ch/software/mtime>  

# Installation

Mtime can be installed with `opam`:

    opam install mtime

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online] or via `odig doc mtime`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: http://erratique.ch/software/mtime/doc/
[OCaml forum]: https://discuss.ocaml.org/

# Sample programs

See [test/min_clock.ml](test/min_clock.ml).

If you installed mtime with `opam` sample programs are located in
the directory `opam var mtime:doc`.
