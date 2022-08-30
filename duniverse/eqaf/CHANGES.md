### v0.9 2022-07-24 Paris (France)

- Add support of OCaml 5.00 (@kit-ty-kate, #37)
- Add support for current-bench and fix bad r² for unequal strings (@Zined-Ada, @art-w, #38)
- Add benchmark with `bechamel` (@Zineb-Ada, @art-w, #38)

### v0.8 2021-08-06 Paris (France)

- Fix the check tool on 4.11.0 (@dinosaure, @cfcs, @stedolan, #30)
  The compilation on 4.11 triggers a case where the locality of the expected
  value when we test `exists_uint8` must be the same. Otherwise, the access to
  this value can have a cost which faults our result.
- Add several utility functions (@cfcs, @dinosaure, #26)
  * `bytes_of_hex` & `string_of_hex`, hex decoding
  * `hex_of_bytes` & `hex_of_string`, hex encoding
  * `divmod`, unsigned `int32` division with small divisors
  * `ascii_of_int32`, conversion from `int32` to decimal `string`
    representation
  * `lowercase_ascii` & `uppercase_ascii`, _constant-time_ implementation of
    `String.{lower,upper}case_ascii`
  * `select_a_if_in_range`, like `select_int` but only supporting positive
    ranges
  * `int_of_bool` & `bool_of_int`, _constant-time_ of `Bool.to_int`

  A documentation exists for each function. The _constant-time_ is checked only
  systematically for `divmod`.
- Merge optional sub-packages (@kit-ty-kate, @hannesm, @dinosaure, #27)
  `cstruct` becomes a required dependency of `eqaf`
- Fix FreeBSD support and remove support of < OCaml 4.07 and remove the
  dependency to `bigarray-compat` (@hannesm, @dinosaure, #32)
- Add a CI on FreeBSD (@dinosaure, @hannesm, #33)
- Remove the test `check/check.exe` (@dinosaure, #31)
  The test `check/check.exe` is really volatile and should be executed into a
  controlled environment (for instance, with `nice -n19` and a _bare-metal_
  computer). We still require the test for any improvement of `eqaf` but it is
  executed separately from our CI.

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
