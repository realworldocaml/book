## 113.43.00

- Converted `bin_prot` to use `%expect` tests. No functional changes.

- Added `Maximum` and `Minimum` submodules to `Size` so to allow for
  easy estimation of the space required for simple bin_prot'ed types.

- `Binable.S_only_functions`

- Fix various bin_prot issue in 32bit architectures

## 113.24.00

- Bin\_prot can be configured to use the primitives to read/write
  integers from bigarrays. This was never enabled due to missing tests
  that selecting this code path doesn't change the format.

  This version add these tests and enable the use of the fast
  primitives.

- Add benchmarks for all exposed bin\_prot read/write functions.
  These are intended to check performance regressions.

- Remove most use of cpp in bin\_prot.

  Replace the pre-processor conditionals by runtime one. This make the
  code a little less obfuscated.

- Remove big literals so that the compiler does not complain in 32bit

## 113.00.00

- Switched build to use =config.h= rather than the command-line for
  preprocessor variables.

- Switched from `ARCH_SIXTYFOUR` to `JSC_ARCH_SIXTYFOUR`.

- Fixed to support 32-bit integers, which are used in `js_of_ocaml`.

    Do not make too many assumptions on integer size.
    Integers are 32bit in Javascript.

    Do not use the "get_float_offset" hack on 32bit
    as it cannot be implemented in javascript.

## 112.35.00

- Sped up `bin_io` of `float array`.

    `Bin_prot` already had special fast handling for `float array`'s but
    `with bin_io` did not use it except for the special type
    `float_array`.  Now, there is fast handling for `float array` and
    its aliases, for example `price array` when `type price = float`.

    - Changed `Size.bin_size_array`, `Write.bin_write_array` and
      `Read.bin_read_array` short circuit to the fast path when it detects
      that `float array` is handled.  Each of these functions receives
      a function for handling array elements and short circuits when the
      function for handling elements is equal to the function for handling
      floats, using physical equality of closures.

    - To cause short circuiting for aliases of `float`, changed `bin_io`
      so that aliased `bin_io` functions are equal the the `bin_io`
      functions of the original type.  That is an optimization for itself
      regardless whether it's used for `float`.  Before this change, every
      function generated for aliases were eta-expanded leading to
      different closures at runtime for each type.

    Short circuiting needs to apply to the handling function rather than
    to the value at hand because:

    * the value is available only in `size` and `write`, and we need
      a way to make `read` work as well.

    * even when the value is a float at runtime, the handling of
      a specific float alias may have been overridden by a custom one.

    Made a slight improvement to `bin_read_float_array`: since the array
    is going to be filled with read values, there is no need to fill it
    with `0.` after allocation:

            let next = pos + size in
            check_next buf next;
        -|  let arr = Array.create len 0. in
        +|  let arr = Array.make_float len in
            unsafe_blit_buf_float_array buf arr ~src_pos:pos ~dst_pos:0 ~len;
            pos_ref := next;

    The difference in speed when optimal and non optimal way of handling
    floats is used:

    | Name                                       |    Time/Run |   mWd/Run | mjWd/Run |
    |--------------------------------------------|-------------|-----------|----------|
    | [bench.ml:float array] size    non optimal |  3_403.80ns | 2_000.00w |          |
    | [bench.ml:float array] size    float_array |      5.55ns |           |          |
    | [bench.ml:float array] size  Price.t array |      6.18ns |           |          |
    | [bench.ml:float array] write   non optimal |  7_839.89ns | 2_000.00w |          |
    | [bench.ml:float array] write   float_array |    292.42ns |           |          |
    | [bench.ml:float array] write Price.t array |    293.16ns |           |          |
    | [bench.ml:float array] read    non optimal |  9_665.06ns | 2_002.00w |   1.00kw |
    | [bench.ml:float array] read    float_array |    461.01ns |     2.00w |   1.00kw |
    | [bench.ml:float array] read  Price.t array |    449.43ns |     2.00w |   1.00kw |

    There is no observed speed penalty for runtime check for short
    circuiting.  The following benchmark shows the speed of handling
    `int array` without and with the check:

    | Name                                       |    Time/Run |   mWd/Run | mjWd/Run |
    |--------------------------------------------|-------------|-----------|----------|
    | [bench.ml:float array] int array  size     |  3_910.64ns |           |          |
    | [bench.ml:float array] int array write     |  6_548.40ns |           |          |
    | [bench.ml:float array] int array  read     | 14_928.11ns |     2.00w |   1.00kw |

    | Name                                       |    Time/Run |   mWd/Run | mjWd/Run |
    |--------------------------------------------|-------------|-----------|----------|
    | [bench.ml:float array] int array  size     |  3_906.86ns |           |          |
    | [bench.ml:float array] int array write     |  5_874.63ns |           |          |
    | [bench.ml:float array] int array  read     | 14_225.06ns |     2.00w |   1.00kw |

## 112.24.00

Minor commit: comments.

## 112.17.00

- Added `Bin_prot.Blob`, formerly known as `Core_extended.Wrapped`,
  which has efficient handling of size-prefixed bin-io values in cases
  where serialization can be bypassed.

## 112.06.00

- Sped up `float` and `float array` operations.
- Removed a use of `Obj.magic` in code generated by `pa_bin_prot` for
  polymorphic variants that led to memory unsafety.

    Previously, `pa_bin_prot` generated this kind of code for
    polymorphic variants:

        match Obj.magic (read_int buf pos) with
        | `A as x -> x
        | `B as x -> x
        | `C -> `C (read_float buf pos)
        | _ -> fail

    and this caused the compiler to assume the result is an immediate
    value.  To fix this we removed the `as x -> x` and used the computed
    integer hash.

## 112.01.00

- In `Write`, improved some OCaml macros to name values and avoid
  calling C functions multiple times.

## 111.03.00

- Fixed build on ARM.

## 109.53.00

- Bump version number

## 109.47.00

- Compilation fix for 32-bit systems

## 109.44.00

- Remove "unwrapped" pointers used by `Bin_prot`, with the bug from
  109.41 fixed.

    Unwrapped pointers cannot coexist with the remove-page-table
    optimization.

    Removed all the C stubs for reading/writing and used instead either
    the new primitives of the next OCaml or standard OCaml code
    reading/writing integers byte by byte.

    Since we don't have unsafe/safe functions anymore but only safe
    ones, removed all the `bin_{read,write}_t_` functions.

    Also renamed `bin_read_t__` to `__bin_read_t__` for the same reason
    as sexplib: to avoid confusion with the function generated for `t_`
    and hide it in the toplevel.

## 109.42.00

- Backed out the changes introduced in 109.41

## 109.41.00

- Remove all uses of "unwrapped" pointers

    Unwrapped pointers cannot coexist with the remove-page-table
    optimization.

    Removed all the C stubs for reading/writing and used instead either
    the new primitives of the next OCaml or standard OCaml code
    reading/writing integers byte by byte.

    Since we don't have unsafe/safe functions anymore but only safe ones,
    removed all the `bin_{read,write}_t_` functions.

    Also renamed `bin_read_t__` to `__bin_read_t__` for the same reason as
    sexplib: to avoid confusion with the function generated for `t_` and
    hide it in the toplevel.

## 109.10.00

- Improved error messages in presence of GADTs.

## 2012-07-15

- Rewrote README in Markdown and improved documentation.
- Eliminated new warnings available in OCaml 4.00.

## 2012-02-28

- Improved portability by better supporting the C99-standard and
  non-GNU compilers.

## 2011-11-10

- Improved portability to older glibc distributions.

## 2011-09-15

- Fixes to improve package dependency resolution.

## 2011-07-04

- Internal updates to sync with Jane Street.

## 2011-06-29

- Fixed bigstring layout bug, which should only affect value
  comparisons with OCaml 3.12.1 or later.
- Made 64-bit detection more reliable on Mac OS X.

## 2010-03-20

- Fixed linking of toplevels to require bigarrays.
- Improved compilation on Mac OS X.

## 2010-03-17

- Fixed small name capture bug.

## 2009-12-21

- Updated contact information.

## 2009-09-19

- Added missing type cases for supporting variant types.
- Fixed handling of variance annotations.

## 2009-07-27

- Fixed build problem with gcc 4.4 due to stricter checking
  for empty macro arguments.

    Thanks to Nobuyuki Tomiza <nobuyuki.tomizawa@gmail.com>
    for the patch!

## 2009-07-20

- Merged tiny Jane Street improvements.

## 2009-07-03

- Made byte swapping more portable.

## 2009-07-02

- Added support for network byte order integers.

## 2009-04-22

- Added macro support for all kinds of vectors (vec,
  float32_vec, float64_vec) and matrices (mat, float32_mat,
  float64_mat), and for bigstrings (bigstring).

## 2009-04-16

- Fixed a bug leading to an exception when writing
  extremely large values (>4 GB buffer size).  Does not cause
  data corruption.

