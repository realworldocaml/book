Logs — Logging infrastructure for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Logs provides a logging infrastructure for OCaml. Logging is performed
on sources whose reporting level can be set independently. Log message
report is decoupled from logging and is handled by a reporter.

A few optional log reporters are distributed with the base library and
the API easily allows to implement your own.

`Logs` has no dependencies. The optional `Logs_fmt` reporter on OCaml
formatters depends on [Fmt][fmt].  The optional `Logs_browser`
reporter that reports to the web browser console depends on
[js_of_ocaml][jsoo]. The optional `Logs_cli` library that provides
command line support for controlling Logs depends on
[`Cmdliner`][cmdliner]. The optional `Logs_lwt` library that provides
Lwt logging functions depends on [`Lwt`][lwt]

Logs and its reporters are distributed under the ISC license.

[fmt]: http://erratique.ch/software/fmt
[jsoo]: http://ocsigen.org/js_of_ocaml/
[cmdliner]: http://erratique.ch/software/cmdliner
[lwt]: http://ocsigen.org/lwt/

Home page: http://erratique.ch/software/logs

## Installation

Logs can be installed with `opam`:

    opam install logs
    opam install fmt cmdliner lwt js_of_ocaml logs # Install all opt libraries

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online][doc] or via `odig doc logs`. 

[doc]: http://erratique.ch/software/logs/doc/

## Sample programs

If you installed Logs with `opam` sample programs are located in
the directory `opam config var logs:doc`.


