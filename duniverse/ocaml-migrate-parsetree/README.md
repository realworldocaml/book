# OCaml-migrate-parsetree
Convert OCaml parsetrees between different major versions

This library converts between parsetrees of different OCaml versions.

Supported versions are 4.02, 4.03, 4.04, 4.05, 4.06, 4.07, 4.08, 4.09,
4.10, and 4.11. For each version, there is a snapshot of the parsetree
and conversion functions to the next and/or previous version.

## Asts

```ocaml
module Ast_{402,403,404,405,406,407,408,409,410,411} : sig

  (* Version specific copy of AST *)
  module Asttypes
  module Parsetree

  (* Magic numbers used for marshalling *)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end
```

These embed copies of AST definitions for each supported OCaml major version.

The AST matching the version of the OCaml toolchain will contain
equalities relating the copy of types to the definitions from
compiler-libs.  For instance, when installed with OCaml 4.04.x the
type `Ast_404.Parsetree.expression` is equal to the type
`Parsetree.expression` from the compiler libraries.

## Migration modules

For each pair of versions `$(n)` and `$(n+1)`, the two modules
`Migrate_parsetree_$(n)_$(n+1)` and `Migrate_parsetree_$(n+1)_$(n)`
convert the AST forward and backward.

The forward conversion is total while the backward conversion is
partial: when a feature is not available in a previous version of the
parsetree, a `Migrate_parsetree_def.Migration_error` exception is
raised detailing the failure case.

## Adding a new OCaml version

We use [Cinaps](https://github.com/janestreet/cinaps) to generate
boilerplate.  You can install it via opam: `opam install cinaps`.

Add the new version in
[src/cinaps_helpers/cinaps_helpers.ml](https://github.com/ocaml-ppx/ocaml-migrate-parsetree/blob/master/src/cinaps_helpers/cinaps_helpers.ml)
`supported_versions`.

Copy the last `src/ast_xxx.ml` file to `src/ast_<new_version>.ml`,
then go over the file and update each sub-module by replacing its
signature and implementation with the code from the compiler. For the
`Config` sub-module, update the two variables with the values in
`utils/config.mlp` in the compiler source tree.

Once this is done, call:

    $ dune exec tools/add_special_comments.exe src/ast_<new_version>.ml

Then diff the `src/ast_xxx.ml` and `src/ast_<new_version>.ml` and go
over the diff to make sure the difference are relevant. The `ast_...`
files require some adjustments which should pop up when you do this
diff. Port the old adjustments to the new file as required.

Add migration functions. In the commands below, set $OLD and $NEW to the
appropriate version numbers, e.g. 408 and 409:
- Manually compile the asts (`ocamlc -c src/ast_{$NEW,$OLD}.ml -I +compiler-libs -I _build/default/src/.migrate_parsetree.objs/byte/`)
- Using `tools/gencopy.exe` (`dune build tools/gencopy.exe`), generate copy code to and from previous version (assuming it is 408):
```
_build/default/tools/gencopy.exe -I . -I src/ -I +compiler-libs -map Ast_$NEW:Ast_$OLD Ast_$NEW.Parsetree.{expression,pattern,core_type,toplevel_phrase} > src/migrate_${NEW}_${OLD}.ml
_build/default/tools/gencopy.exe -I . -I src/ -I +compiler-libs -map Ast_$OLD:Ast_$NEW Ast_$OLD.Parsetree.{expression,pattern,core_type,toplevel_phrase} > src/migrate_${OLD}_${NEW}.ml
```
- Fix the generated code by implementing new cases

*TODO*: specialize and improve gencopy for these cases

At any time, you can expand boilerplate code by running `make cinaps`.

Update build system:
- make sure `make cinaps` reaches a fixed point :)
- `make` should succeed
