## 113.43.00

- Change `typerep_lib` to use the type `lazy_t` rather than `Lazy.t`.  The
  ocaml_plugin library's `Ocaml_compiler` compiles modules in an
  environment where the compiler cannot determine that `Lazy.t` and `lazy_t`
  are the same - thus with the current version of typerep_lib plugins
  effectively cannot use `@@deriving typerep`.

## 113.24.00

- Add whether record fields are mutable.

## 112.24.00

- Remove unused "bin_proj" rewriter.

## 112.17.00

- Split out typerep_extended which is now using core_kernel

## 111.06.00

- Renamed `Typerep` libraries for more consistency with the rest of
  the framework.

    ```ocaml
    Typerep_kernel --> Typerep_lib
    Typerep_core   --> Typerep_extended
    Typereplib     --> Typerep_experimental
    ```

