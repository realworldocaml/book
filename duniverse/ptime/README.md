Ptime — POSIX time for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Ptime has platform independent POSIX time support in pure OCaml. It
provides a type to represent a well-defined range of POSIX timestamps
with picosecond precision, conversion with date-time values,
conversion with [RFC 3339 timestamps][rfc3339] and pretty printing to a
human-readable, locale-independent representation.

The additional Ptime_clock library provides access to a system POSIX
clock and to the system's current time zone offset.

Ptime is not a calendar library.

Ptime depends on the `result` compatibility package. Ptime_clock
depends on your system library. Ptime_clock's optional JavaScript
support depends on [js_of_ocaml][jsoo]. Ptime and its libraries are
distributed under the ISC license.

[rfc3339]: http://tools.ietf.org/html/rfc3339
[jsoo]: http://ocsigen.org/js_of_ocaml/

Home page: http://erratique.ch/software/ptime  

## Installation

Ptime can be installed with `opam`:

    opam install ptime
    opam install js_of_ocaml ptime  # ptime with jsoo support

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc ptime`.

[doc]: http://erratique.ch/software/ptime/doc/

## Sample programs

If you installed Ptime with `opam` sample programs are located in
the directory `opam config var ptime:doc`.
