# ocaml-print-intf

Pretty prints a compiled interface file into the corresponding human-readable
OCaml signature.

This is a convenient alternative to `ocamlc -i` which does this on source .ml
files.

## Example

On this repository:

```
$ dune exec -- ocaml-print-intf ocaml_print_intf.ml
val root_from_verbose_output : string list -> string
val target_from_verbose_output : string list -> string
val build_cmi : string -> string
val print_intf : string -> unit
val version : unit -> string
val usage : unit -> unit
```
