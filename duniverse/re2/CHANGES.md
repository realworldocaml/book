## v0.11

- Re2 is now a "stdless" library:
  + one doesn't need to `open Re2.Std` anymore
  + `Re2.Std.Re2` is now simply `Re2`
  + `Re2.Std.Parser` is now simply `Re2.Parser`
  + `Re2.Regex` is now simply `Re2`
  + `Re.Parser` is left unchanged.

## v0.10

- Changed `Parser.Decimal.int` to require at least one digit, i.e. to disallow
  zero digits.

- Added module `Regex.Multiple`, an efficient way to ask which of several
  regexes matches a string.

## v0.9

## 113.24.00

- Switched to PPX.

- Add `Re2.Parser.any_string` combinator.

  There are no tests because `any_string` is constructed only from the tested
  API and there's almost no interesting properties of it that can be verified.

## 113.00.00

- Improved `Re2.find_submatches` on big patterns with many submatches
  unmatched, e.g. =(ABC)|(DEF)|(GHI)|(KLM)|...=.

    Without the fix:

    +-------------------------------------------------------+--------------+------------+----------+----------+------------+
    | Name                                                  |     Time/Run |    mWd/Run | mjWd/Run | Prom/Run | Percentage |
    +-------------------------------------------------------+--------------+------------+----------+----------+------------+
    | [re2_internal.ml] find_submatches with many Nones:5   |     406.81ns |     30.00w |          |          |      0.08% |
    | [re2_internal.ml] find_submatches with many Nones:10  |   2_385.11ns |    207.00w |          |          |      0.47% |
    | [re2_internal.ml] find_submatches with many Nones:50  |  12_772.97ns |  2_072.00w |    0.33w |    0.33w |      2.53% |
    | [re2_internal.ml] find_submatches with many Nones:100 |  43_196.95ns |  7_191.00w |    2.03w |    2.03w |      8.56% |
    | [re2_internal.ml] find_submatches with many Nones:200 | 504_884.95ns | 29_316.00w |   16.05w |   16.05w |    100.00% |
    +-------------------------------------------------------+--------------+------------+----------+----------+------------+

    With it:

    +-------------------------------------------------------+--------------+-----------+----------+----------+------------+
    | Name                                                  |     Time/Run |   mWd/Run | mjWd/Run | Prom/Run | Percentage |
    +-------------------------------------------------------+--------------+-----------+----------+----------+------------+
    | [re2_internal.ml] find_submatches with many Nones:5   |     408.24ns |    30.00w |          |          |      0.12% |
    | [re2_internal.ml] find_submatches with many Nones:10  |   1_607.67ns |   163.00w |          |          |      0.48% |
    | [re2_internal.ml] find_submatches with many Nones:50  |   3_223.89ns |   563.00w |          |          |      0.96% |
    | [re2_internal.ml] find_submatches with many Nones:100 |   5_288.09ns | 1_063.00w |    0.20w |    0.20w |      1.58% |
    | [re2_internal.ml] find_submatches with many Nones:200 | 334_107.81ns | 2_063.00w |    0.79w |    0.79w |    100.00% |
    +-------------------------------------------------------+--------------+-----------+----------+----------+------------+

- Fixed build on FreeBSD.

    Excise direct mention of g++ from re2 Makefile, preferring the inbuilt CXX
    macro. This fixes the build on FreeBSD (yes, really).

- Added an applicative interface to building/using regular expressions.

- Made Re2 depend only on `Core_kernel`, not `Core`.

    Fixes janestreet/re2#6

## 112.35.00

- Fixed a bug in `Re2.find_all_exn`, extant since 2014-01-23, in which
  it returns spurious extra matches.

    Using pattern `b` and input `aaaaaaaaaaaab` is expected to return
    a single match at the end of the input but instead returned the
    match multiple times, approximately as many times as
    `input length / min(match length, 1)`.

    Added tests for this function and also `get_matches` which uses the
    same code.

- Updated to new version of upstream library.

## 111.08.00

- Upgraded to upstream library version 20140304.

    Added options `Dot_nl` and `Never_capture`.

## 111.06.00

- Added `Re2.Std`, so that one should now use `Re2` via `module Re2 =
  Re2.Std.Re2`.

    At some future date, we will rename the `Regex` module to
    `Re2_internal` to force the stragglers to update to the new
    convention.

## 111.03.00

- Fixed a bug with `replace_exn` and anchoring.

    Fixed this bug:

        $ R.replace_exn ~f:(fun _ -> "a") (R.create_exn "^") "XYZ";;
        - : string = "aXaYaZa"

        $ R.replace_exn ~f:(fun _ -> "a") (R.create_exn "^X") "XXXYXXZ";;
        - : string = "aaaYXXZ"

## 109.53.00

- Bump version number

## 109.52.00

- Fixed bugs in `Re2.Regexp.find_all` and `Re2.Regexp.find_first`.
    * memory leaks on errors
    * unlikely garbage in their return values, or even segfaults
      (especially unlikely for `find_first`)

## 109.32.00

- Fixed a bug in the C bindings that could cause a segfault.

    Fixed a bug where `mlre2__create_re` in C can give OCaml a freed C string.

    The bug was in:

    ```c
    if (!compiled->ok()) {
      compile_error = compiled->error().c_str();
      delete compiled;
      compiled = NULL;
      caml_raise_with_string(*caml_named_value("mlre2__Regex_compile_failed"),
          compile_error);
    }
    ```

    This is in `mlre2__create_re` if we fail to compile the regular
    expression.  Notice how we delete the re object before we use its'
    error string.  (Notice that in C++ `c_str()` returns a pointer to
    the internal data of the string object it does NOT create a copy
    and `error()` just returns a reference to the regular expression
    objects error string member `*error_`).

    So if `caml_raise_with_string` has to allocate on the heap to create
    the exception or the copy of the string that might invalidate the ptr
    before we will copy it.

