2.2.1 (2020-05-14)
------------------

* Upgrade to dune 2.0

2.2.0 (2020-09-03)
------------------

* Add support for merging double annotations (`<ocaml from="ProtoA"><ocaml predef>`)

* Add tests for annotation merging and target-specific annotations

2.1.0 (2019-12-3)
-----------------

* Fix bug preventing generated code from compiling when using
  json adapters on recursive types.

* Improve automatic error messages shown in case of failed validation.
  Now include the validator's name or code.

* Add support for json adapters in the bucklescript backend. (#153)

2.0.0 (2018-05-31)
------------------

* Add support for json adapters in OCaml (`<json adapter.ocaml=...>`)

* Add support for json enums with a catch-all case (`<json open_enum>`)

* Remove `<json tag_field=...>` and `<json untyped>`

1.13.0 (2018-03-27)
-------------------

* Introduce `atdgen-runtime` package. This package contains the runtime
  dependency introduced by the `atdgen` code generator. The old runtime
  library is deprecated

* Add `atdj` to set of released packages. `atdj` is a java code generator
  for .atd files.

* Improve generated code to emit ppx attributes to ignore harmless warnings

* `Ag_version` submodule has been replaced with `Version`.

* Transition `atd` aliases using the `(wrapped true)` mode of
  jbuilder. This is a breaking change for all of those who use `atd`
  the library. All modules are now accessible under the `Atd.` entry module.
