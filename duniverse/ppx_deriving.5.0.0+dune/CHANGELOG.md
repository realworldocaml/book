Changelog
=========

4.4
---

* Restore support for OCaml 4.02.3
  #188
  (ELLIOTTCABLE)
* workaround Location.input_filename being empty
  when using reason-language-server
  #196
  (Ryan Artecona)
* Add support for OCaml 4.08.0
  #193, #197, #200
  (Gabriel Scherer)

4.3
---

* use Format through Ppx_deriving_runtime to avoid deprecation warning
  for users of JaneStreet Base
  (Stephen Bastians and Gabriel Scherer, review by whitequark)
* silence a ambiguous-field warning (41) in generated code
  #163
  (Étienne Millon, review by Gabriel Scherer)
* use dune
  #170
  (Rudi Grinberg, Jérémie Dimino)
* silence an unused-value warning for show
  #179
  (Nathan Rebours)

4.2.1
-----

  * Add support for OCaml 4.06.0
    #154, #155, #156, #159
    (Gabriel Scherer, Fabian, Leonid Rozenberg)
  * Consider { with_path = false } when printing record fields
    #157
    (François Pottier)

4.2
---

  * Add support for OCaml 4.05.0.
  * Use the `ocaml-migrate-parsetree` library to support multiple
    versions of OCaml.
  * Fix comparison order of fields in records (#136).
  * Silence an `unused rec flag` warning in generated code (#137).
  * Monomorphize comparison function for builtin types (#115)
  * Raise an error when `type nonrec` is encountered (#116).
  * Display an error message when dynamic package loading fails.
  * Add a `with_path` option to `@@deriving` to skip the module path
    in generated code (#120).

The homepage for the project has now moved to:
<https://github.com/ocaml-ppx/ppx_deriving>

4.1
---

  * Fix type error with inheritied polymorphic variant type in
    [@@deriving map].
  * Fix incorrect handling of multi-argument constructors in
    [@@deriving show].
  * Add API hooks for ppx_type_conv.

4.0
---

  * Show, eq, ord, map, iter, fold: add support for `Result.result`.
  * Ppx_deriving.Arg: use Result.result instead of polymorphic variants.
  * Ppx_deriving.sanitize: parameterize over an opened module.
  * Add support for `[@@deriving]` in module type declarations.
  * Add support for loading findlib packages instead of just files in
    ppx_deriving_main.
  * Treat types explicitly qualified with Pervasives also as builtin.
  * Compatibility with statically linked ppx drivers.

3.1
---

  * Show, eq, ord: hygienically invoke functions from referenced modules
    (such as X.pp for X.t when deriving show) to coexist with modules
    shadowing ones from standard library.
  * Iter, map, fold: hygienically invoke List and Array functions.

3.0
---

  * Implement hygiene: Ppx_deriving.{create_quoter,quote,sanitize,with_quoter}.
  * Show, eq, ord: add support for `lazy_t`.
  * Add support for `[@nobuiltin]` attribute.
  * Add Ppx_deriving.hash_variant.
  * Remove allow_std_type_shadowing option.
  * Remove Ppx_deriving.extract_typename_of_type_group.

2.1
---

  * Fix breakage occurring with 4.02.2 w.r.t record labels
  * Fix prefixed attribute names (`[@deriving.foo.attr]` and `[@foo.attr]`).
  * Add allow_std_type_shadowing option for eq and show.

2.0
---

  * Add support for open types.

1.1
---

  * New plugin: create.
  * Show, eq, ord: handle `_`.
  * Show, eq, ord, map, iter, fold: handle inheriting from a parametric
    polymorphic variant type.
  * Make `Ppx_deriving.poly_{fun,arrow}_of_type_decl` construct functions
    in correct order. This also fixes all derivers with types with
    more than one parameter.
  * Add `Ppx_deriving.fold_{left,right}_type_decl`.

1.0
---

  * Make deriver names lowercase.
  * Remove Findlib+dynlink integration. All derivers must now be
    explicitly required.
  * Allow shortening [%derive.x:] to [%x:] when deriver x exists.
  * Make `Ppx_deriving.core_type` field optional to allow ignoring
    unsupported [%x:] shorthands.
  * Add support for [@@deriving foo { optional = true }] that does
    not error out if foo is missing, useful for optional dependencies.
  * Rename ~name and ~prefix of `Ppx_deriving.attr` and
    `Ppx_deriving.Arg.payload` to `~deriver`.
  * Renamed `Ppx_deriving.Arg.payload` to `get_attr`.
  * Add `Ppx_deriving.Arg.get_expr` and `get_flag`.

0.3
---

  * Show, Eq, Ord, Iter, Fold: handle ref.
  * Show: handle functions.
  * Show: include break hints in format strings.
  * Show: pull fprintf into local environment.
  * Show: add `[@polyprinter]` and `[@opaque]`.
  * Add `Ppx_deriving.Arg.expr`.

0.2
---

  * New plugins: Enum, Iter, Map, Fold.
  * All plugins: don't concatenate affix if type is named `t`.
  * Add `[%derive.Foo:]` shorthand.
  * Show, Eq, Ord: add support for list, array, option.
  * Show: include full module path in output, including for types with manifest.
  * A lot of changes in `Ppx_deriving interface`.

0.1
---

  * Initial release.
