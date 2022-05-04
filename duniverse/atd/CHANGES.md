2.5.0 (2022-04-23)
------------------

* atdpy: make `atdpy --version` print the version of atdpy itself
  rather than the version of the `atd` library (#270)
* atdpy: fix handling of `nullable` and improve error message on
         `option` types used without optional fields (#277)
* Add TypeScript backend called atdts (#274)

2.4.1 (2022-03-25)
------------------

* atdpy: don't apply the `@dataclass` decorator twice if explicitly
  added by the user via an ATD annotation such as
  `<python decorator="dataclass(frozen=True)">` (#267)

2.4.0 (2022-03-24)
------------------

* atdpy: allow custom imports and class decorators to be added to the
  generated Python code.

2.3.3 (2022-03-16)
------------------

* Prevent incorrect validation errors for annotations of the form
  `<ocaml field_prefix=...>` and a few others (#258)

2.3.2 (2022-03-11)
------------------

* Fix package dependencies (#257)

2.3.1 (2022-03-10)
------------------

* Ensure that atdgen reports its own version rather than the version
  of the atd library.
* Fix version constraint on cmdliner.

2.3.0 (2022-03-10)
------------------

* Allow single-quoted strings as an alternative to double-quoted
  strings in ATD files (#239)
* Add Python backend called atdpy (#235)
* Add detection of misplaced annotations and misspelled annotation
  field names for atdgen targets and atdpy (#204, #227)
* atdpy: Downcase Python output files (#251)
* atdpy: Disable flake8 checks on generated code via a special comment (#252)
* atdgen: Add support for ppx attributes on individual type
  definitions (#238)
* (BREAKING) atdgen: change encoding of int64 values to string (#231)
* other enhancement and fixes (see git log)

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
