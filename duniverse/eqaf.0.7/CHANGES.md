### v0.7 2020-04-16 Paris (France)

- Add `find_uint8` (@dinosaure, @cfcs, #20)
- Add `exists_uint8` (@dinosaure, @cfcs, #20)

### v0.6 2020-03-11 Paris (France)

- remove build dependency on dune (@CraigFe, #16)
- add bigarray-compat and optional dependencies (@hannesm, #17)
- add `select_int`, `one_if_not_zero`, `zero_if_not_zero` (@cfcs, @dinosaure, #19, #18)

### v0.5 2019-07-01 Paris (France)

- Delete `min` and use `<>` operator to compare length on `equal` function
- Implementation of `compare_{be,le}{,with_len}` function (@cfcs, @hannesm, @dinosaure)
- Test on `compare` function (@dinosaure)
- Unit test on `compare` (@dinosaure)
- Fuzz test on `compare` (@dinosaure)
- Documentation (@dinosaure, @cfcs)

### v0.4 2019-05-24 Paris (France)

- Distribution integrate an attack example
- Fuzzer to test `equal` function
- Unroll internal loop over 16 bits integers instead 32 bits
- Put x86 ASM output in implementation (and audit)
- Do second check even if first on fails (bad r²)
- Avoid indirection to `Pervasives` functions

### v0.3 2019-05-02 Paris (France)

- Provide `Eqaf_bigstring`
- Provide `Eqaf_cstruct`
- New check tool and delete any dependencies on `eqaf` package (@dinosaure, @hannesm, @cfcs)

NOTE: This version is buggy, you MUST use v0.2 or v0.4

### v0.2 2018-10-15 Paris (France)

* _Dunify_ project
* Update OPAM file
* Avoid `core_bench` dependency
* Make benchmark to test constant-time on `eqml`
* __Move `equal` function to the OCaml implementation__ (instead C implementation)
* Port benchmark on Windows and Mac OSX

### v0.1 2018-08-31 Paris (France)

* First release
