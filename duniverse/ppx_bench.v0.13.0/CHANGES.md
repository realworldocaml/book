## v0.11

Depend on ppxlib instead of (now deprecated) ppx\_core

## 113.43.00

- use the new context-free API

## 113.33.00

- Add an attribute `@name_suffix` to `let%bench_module`. This is an arbitrary
  expression that gets concatenated to the name of the bench module. It's
  useful to have this when using `ppx_bench` inside a functor, to distinguish
  each functor application in the output.

## 113.24.00

- Update to follow `Ppx_core` evolution.

- Mark attributes as handled inside explicitly dropped pieces of code.

  So that a `@@deriving` inside a let%test dropped by
  ppx\_inline\_test\_drop doesn't cause a failure.
