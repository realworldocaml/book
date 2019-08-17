## v0.11

- Completly changed the syntax to make this a proper ppx (and not a -pp as it
  previously was).
  The old syntax is now available in `ppx_optcomp_old`.

- Depend on ppxlib instead of (now deprecated) ppx\_core and ppx\_driver.

## 113.43.00

- Make it easier to share a .h and .mlh

  Ppx_optcomp was modified to accept `defined(X)` only if `X` has been
  seen, either in a `#define` or `#undef`. This allow to share config.h
  files between C and OCaml and still be protected against typos in .ml
  files.

## 113.33.00

- Install standalone ppx-optcomp program that can be run through `-pp`

## 113.24.00

- Change the way optcomp resolve filenames in #import directives

  Do the same as cpp, i.e. for relative filenames, consider they are
  relative to the directory of the file being parsed. This doesn't
  matter internally as build commands are always executed from the
  current directory, but it matters for the public release as everything
  is executed from the root.
