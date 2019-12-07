## v0.10

- Added library `Textutils_kernel`, created by splitting out the
  platform-independent code from the `Textutils` library.

## 113.24.00

- Switched to PPX.

- Fixed a bug where the computation of cell heights could cause division by zero
  in some cases.

- Expose the constructors of `Ascii_table.Align.t` so that we can write

    Column.create ~align:Left ...

  instead of

    Column.create ~align:Align.left

## 112.17.00

- Added a `~narrow` argument to `Text_graph.render`

## 112.01.00

- Added `Ascii_table.Table_char`, to expose the special table border
  characters.

## 111.28.00

- Moved `Text_block` from `Core_extended` into `Textutils`.

## 111.03.00

- Changed `Textutils.Console` to not reference `Async.Log`, so that
  building inline benchmark runners no longer requires building `Async`.

    The change makes `Textutils`, and by extension `Core_extended` and
    `Core_bench`, not depend on `Async`.

## 109.53.00

- Bump version number

## 109.52.00

- Added `Console.Log` module for writing colorized `Async.Log`
  messages.

## 109.36.00

- In `Ascii_table` module, added support for displaying table using ASCII characters instead of Unicode.

    This is motivated by the need to use `Core_bench` in contexts where
    the extended ASCII character set is not suitable for displaying
    tables.

    The default style is the following:

    ```
    ┌─────────────────┬───────────┬──────────┐
    │ Name            │ Time (ns) │ % of max │
    ├─────────────────┼───────────┼──────────┤
    │ quick_stat      │     93.11 │   100.00 │
    │ counters        │     33.24 │    35.70 │
    │ allocated_bytes │     37.03 │    39.77 │
    └─────────────────┴───────────┴──────────┘
    ```

    The new style is as follows:

    ```
    $ ./test_bench.exe gc -q 0.5 -ascii
    Estimated testing time 1.5s (change using -quota SECS).

      Name              Time (ns)   % of max
    ----------------- ----------- ----------
      quick_stat            93.17     100.00
      counters              34.56      37.09
      allocated_bytes       37.06      39.78
    ```

## 109.35.00

- Added new module `Textutils.Text_graph` for plotting text graphs on
  a terminal.

    Here is an example density plot for `minor_words` from
    `mcquote-stats.data`.

    ```
    (354 (3%) values outside the range, bucket size is 38.58)
        74.00     3786 |----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----|
       112.58        0 |
       151.17        0 |
       189.75        0 |
       228.33        0 |
       266.92        0 |
       305.50        0 |
       344.08        0 |
       382.67        0 |
       421.25        0 |
       459.83     3653 |----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+-
       498.42        4 |
       537.00     1055 |----+----1----+----2----+--
       575.58      308 |----+---
       614.17       70 |-
       652.75      404 |----+----1
       691.33       71 |-
       729.92       32 |
       768.50       18 |
       807.08      151 |---
       845.67       17 |
       884.25        4 |
       922.83        8 |
       961.42       66 |-
      1000.00        0 |
    ```

    Each `-` is approximately 37.860 units.

