# FIXMES

There are some workarounds (directories, links, -no-hygiene) for ocamlbuild, we should get rid of it.

A patch is needed to corebuild (adding -no-check to ppx):
`-tag "ppx(ppx-jane -as-ppx -no-check)"`

Some TODOS have been left in the book content.

The `compare_lib` example, using camlp4, didn't get ported. Building fails, what should be done with it?!

We should check chapter by chapter for differences, some information got lost in the conversion.


# RWO applications

Entrypoint is in [app/app.ml]().
It is a binary with two commands: build and validate.

They are implemented on top of rwo library from [lib/]() directory.

[app/rwo-deps.ml]() is an auxiliary binary supporting the build system by computing dependencies between book files in Makefile format.

# RWO library



## Modules

Book
:   Book processing. The book's files are in HTMLBook format with some
    custom variations, e.g. we support <link rel="import"> nodes to
    enable inclusion of code blocks in external files. This module
    supports the processing of these files.

Toc
:   Table of contents. Representation of the hierarchical structure of
    the book.

### Support utilities

Useful & reusable tools, not specific to this book.

Scripts
:   Collection of scripts.

Import
:   Import nodes. We support a custom HTML schema to express that some
    code should be imported from another file. The syntax is:

    {v
    <link rel="import" href="path" part="N">
    v}

    where ["path"] is a local
    path to a code file, ["N"] is a part number within that file. The
    [part] attribute is optional.

Ppre
:   HTML p tag followed by pre tag.

    O'Reilly's HTMLBook version of edition 1 represented code sections
    in the form

    {v <p></p><pre></pre> v}

    to encode some information about where code was imported from,
    index terms, and for the captions of the online version. We needed
    to parse such sections to extract the code from their source
    files, and may also need to generate such sections when converting
    back to pure HTMLBook.

Index
:   Index. HTMLBook denotes index terms with a somewhat bulky
    notation:

    {v <a data-type="indexterm" data-primary="foo" data-secondary="bar"> V}

    We support a lighter-weight notation:

    {v <idx>foo/bar</idx> v}

    This module supports conversion between the two forms.

### Generic utilities

Generic logic, nothing specific to the book.

Bash_script
:   Bash script parsing and evaluation.

HTML
:   HTML processing

### TODO

Pygments
:   Pygments support. Call out to the [pygmentize] command line
    tool.

Lang
:   Code languages. The language of code files which can be imported
    into the book is defined by its extension. The language dictates
    how the imports are processed, as follows:

    {b OCaml files:}

    - "ml": OCaml file, which will be parsed by Oloop.
    - "mli": OCaml file, which will be parsed by Oloop.
    - "mll": OCaml file, which will be parsed by Oloop.
    - "mly": OCaml file, which will be parsed by Oloop.
    - "mlpack"

    - "mlt": OCaml toplevel commands that will be auto-evaluated
      via Oloop.

    - "rawscript": OCaml toplevel script, which should not be
      auto-evaluated, e.g. because it contains some manually ellided
      code.

    - "syntax": OCaml syntax descriptions.


    {b Bash files:}

    - "cmd": Full bash scripts to be imported verbatim.

    - "sh": Lines of bash commands, which will be auto-evaluated
      and are expected to exit with zero.

    - "errsh": Lines of bash commands, which will be auto-evaluated
      and are expected to exit with non-zero.

    - "rawsh": Bash commands executed in terminal and the
      corresponding output. Such files will not be auto-evaluated, so
      thus can include manually ellided code.


    {b Other files:}

    - "ascii": Plain ascii file, used for ASCII art.
    - "atd"
    - "c"
    - "cpp"
    - "h"
    - "java"
    - "json"
    - "out": Console files as provided in O'Reilly source.
    - "S" or "s": GNU assembly code.
    - "scm"
    - "txt"
