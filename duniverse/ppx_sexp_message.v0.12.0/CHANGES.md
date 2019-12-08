## v0.11

- Depend on ppxlib instead of (now deprecated) ppx\_core, ppx\_driver and
  ppx\_metaquot.

## v0.10

- Added syntax `%message.omit_nil`, which is like `%message`, but omits
  expressions whose sexp is `()`.

## v0.9

## 113.43.00

- Expand `%message` to: `List []`

- Allow to use `sexp_option` in more places in ppx\_sexp\_message, to make it
  to display information only some of the time.
