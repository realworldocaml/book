(** The [Format.formatter] type from OCaml's standard library, exported here
    for convenience and compatibility with other libraries.

    The [Format] module itself is deprecated in Base. You may refer to it
    explicitly through [Caml.Format], though you may wish to search for other
    alternatives for constructing pretty-printers using the [Format.formatter]
    type. *)

type t = Caml.Format.formatter
