## v0.11

Depend on ppxlib instead of (now deprecated) ppx\_core, ppx\_driver,
ppx\_metaquot, ppx\_traverse and ppx\_type\_conv.

## 113.43.00

- use the new context-free API

## 113.24.00

- OCaml makes no distinctions between "foo" and
  `{whatever|foo|whatever}`. The delimiter choice is simply left to the
  user.

  Do the same in our ppx rewriters: i.e. wherever we accept "foo", also
  accept `{whatever|foo|whatever}`.

- Fix missing location in errors for broken custom printf example like:

      printf !"%{sexp: int" 3;;

- Update to follow `Ppx_core` evolution.
