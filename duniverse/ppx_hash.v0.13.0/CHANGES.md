## v0.10

- Fixed `[@@deriving hash]` on nested tuples

- Added `@compare.ignore` record-field attribute; `ppx_compare` and `ppx_hash`
  skip record fields annotated with `@compare.ignore`.

- Changed `[@@deriving hash]` and `[%hash]` on atomic types to use `hash` rather
  than `hash_fold`. E.g. `[%hash: M.t]` now expands to `M.hash`.

- Renamed `@hash.no_hashing` to `@hash.ignore`.

- Depend on ppxlib instead of (now deprecated) ppx\_core, ppx\_driver and
  ppx\_metaquot.

