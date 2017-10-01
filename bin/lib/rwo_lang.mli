(** Code languages. The language of code files which can be imported
    into the book is defined by its extension. The language dictates
    how the imports are processed, as follows:

    {b OCaml files:}

    - "ml": OCaml file, which will be parsed by Oloop.
    - "mli": OCaml file, which will be parsed by Oloop.
    - "mll": OCaml file, which will be parsed by Oloop.
    - "mly": OCaml file, which will be parsed by Oloop.
    - "mlpack"

    - "topscript": OCaml toplevel commands that will be auto-evaluated
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
*)
open Core

type t = private string
  [@@deriving sexp]

val of_string : string -> t Or_error.t
val to_string : t -> string

val of_filename : string -> t Or_error.t
(** Return lang based on extension of given filename. *)

val to_docbook_lang : t -> string Or_error.t
