0.10.0 (11/21/2019)
-------------------

- Do not produce a suprious empty correction when deriving_inline
  expands into an extension that undergoes further expansion (#86,
  @aalekseyev)

- Add `Ppxlib.Quoter`. This module allows to generate hygienic code fragments in
  the spirit of ppx_deriving. (#92, @rgrinberg)

- Allow for registering derivers on module type declarations. (#94, fix #83,
  @rgrinberg)

- Fix parsing long idenitifiers. (#98, @NathanReb)

0.9.0
-----

- Bump AST to 4.08 (#80, @xclerc)

0.8.1
-----

### Fixed

- Report errors according to the value of `OCAML_ERROR_STYLE` and
  `OCAML_COLOR` in the standalone driver (#83, @NathanReb)

0.6.0
-----

- Set `Location.input_name` to the original filename when reading a
  binary AST (#.., @diml)

0.5.0
-----

- Add an `(** @inline *)` to the include generated when silencing
  warning 32 (#58, @trefis)

- Add `Ppxlib.mk_named_sig` and `Ppxlib.is_polymorphic_variant` (#57,
  @trefis)

0.4.0
-----

- Do not report errors about dropped or uninterpreted attributes
  starting with `_` (#46, fix #40, @diml)

- Fix he `special_function` rule for dotted operators and allow
  `Longident.parse` to parse dotted operators (#44, @Octachron)

- Port to `dune` and remove use of bash (#45, @rgrinberg)

- Ignore all attribites starting with `_` (#46, @diml)

- Reserve the `reason` and `refmt` namespaces (#46, @diml)

- Reserve the `metaocaml` namespace (#50, @rgrinberg)

- Fix attribute extraction for Otag/Rtag (#51, @xclerc)

- Do not relocate files unless `-loc-filename` is passed (#55, @hhugo)

- Perserve the filename in the output (#56, @hhugo)

0.3.1
-----

- Add `Attribute.declare_with_name_loc` (#33, @diml)

- Let the tool name pass throught when used as a -ppx (#41, @diml)

- Update the AST to 4.06 (#8, @xclerc)

0.3.0
-----

- Update the AST to 4.06 (#8, @xclerc)

- Deprecate old references to type_conv in argument and rewriter names
  and add new ones mentioning deriving instead (#7, #9 @xclerc)

- Fix compatibility with `-safe-string` (#10, @hhugo)

- Restore tests (#11, @xclerc)

- Allow to set the suffix of corrected files (#15, @diml)

- Restore compatibility with OCaml 4.04.x (#16, @xclerc)

0.2.0
-----

- Make sure to import command line arguments registered with
  ocaml-migrate-parsetree (#5, @diml)

- Fix an issue where cookies set from the command line sometimes
  disappeared (#6, @diml)

0.1.0
-----

Initial release.
