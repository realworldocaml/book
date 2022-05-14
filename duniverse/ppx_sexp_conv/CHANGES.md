## v0.11

- Depend on ppxlib instead of (now deprecated) ppx\_core, ppx\_driver and
  ppx\_metaquot.

## v0.10

- Added new `[@@deriving sexp]` record-field attribute, `[@sexp.omit_nil]`, for
  a field that is omitted if its sexp representation is `()`.

- Improved `[%sexp_of: 'a]` and `[%of_sexp: 'a]` to not expose variable names
  intended for internal use.

## v0.9

## 113.43.00

- Fix generator for polymorphic types where var names clashes with type name: `type 't t = ...`

## 113.33.00

- Clean up the documentation for sexplib, modernizing it to include
  `ppx_sexp_conv`, and breaking up the documentation between sexplib and
  `ppx_sexp_conv`.  Also changed the formatting to use org-mode, so it
  will render properly on github.  Markdown doesn't render well by
  default, unless you use quite different conventions about linebeaks.

## 113.24.00

- Trying to improve the tests in ppx\_sexp\_conv because they are a mess.
  At least all tests are automatic now. And more things are tested like
  the sexpification of exceptions.

- Update to follow `Type_conv` and `Ppx_core` evolution.

- Make ppx\_sexp\_conv correctly handle aliases to polymorphic variants:

  type t = ` `A ` `@@deriving sexp`
  type u = t `@@deriving sexp`
  type v = ` u | `B ` `@@deriving sexp`

  Before, `v_of_sexp` would never manage to read `B.  This problem is
  now fixed if you use `sexp_poly` on `u` instead of `sexp`, and if you
  don't, you get an "unbound value __u_of_sexp__".  People should use
  `sexp_poly` when they have a polymorphic variant type that is not
  syntactically a polymorphic variant, but in practice it's simpler to
  replace `sexp` by `sexp_poly` when faced with the error above.

  The need for `sexp_poly` should happen only in one new case: an
  implementation says `type u = t `@@deriving sexp`` but the interface
  says `type u = ``A` `@@deriving sexp``.  (the old case where it was
  already needed is when you have an interface that says `type u = t
  `@@deriving sexp`` and in some other implementation you try to say
  `type t = ` That_module.t | `A ` `@@deriving sexp``).
