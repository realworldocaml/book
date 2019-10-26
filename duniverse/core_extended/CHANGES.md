## 113.43.00

- Export the interval map module type.

- Ocaml interface to the Unix pseudo-terminal functions

- `Extended_list.fold_left_term` is no longer needed - we now have `List.fold_until`.

## 113.33.00

- Update to follow `core[_kernel]` evolution.

## 113.24.00

N.B. Some interface changes occured in Core which are repercuted in this
package, they are not all list in this file though.

- Switched to PPX.

- Upgrade `Interval_map.t` with monad operations.

- Update the `interval_map_intf.ml` file to try to make the documentation
  clearer.

  This mostly constitutes splitting out the core operations into a separate
  module type earlier in the file, so that their documentation occurs before
  the various more specific module types in reading order.

  Various bits of documentation have been tweaked with examples or laws.

- Add underscores to color print names
  Improve and uniformize the behavior of colorprintf functions at the cost of changing the type slightly

- Fix core_extended stubs on openbsd

  Closes #7
  Closes #2

- Move `Core_extended.Std.Sys.home` to `Core.Std.Sys.home_directory`.

- Add a module whose type `'a t` acts as a container of ordered items of type 'a
  (morally, a `'a list`) but which supports efficient append operations.

  Sometimes called a Rope, or Concatenable_list.

- Expose the constructors of `Ascii_table.Align.t` so that we can write

    Column.create ~align:Left ...

  instead of

    Column.create ~align:Align.left

- Fix sexp diffing on records
  The wrong comparaison was leading to huge diff as soon as one field was
  missing on one side

## 113.00.00

- Added to `Interval_map` a more complete set of operations.

- Removed `Core_extended.Sexp.filter_record`, which has been superseded by
  `Core.Std.Sexp.of_sexp_allow_extra_fields`.

- Added to `Interval_map` an `Interval` module, with the type of intervals used
  in an interval map.

- In `Color_print`, added `sprintf` functions, and changed formatting to
  compose properly.

## 112.35.00

- Removed the `Stats_reporting` module.
- Renamed `Quickcheck` module to `Quickcheck_deprecated`. It's replaced
  by Janecheck, which for now is a separate library in the core_extended
  package, but will soon be merged into core.
- Moved the `Selector` module to its own library. This is for internal
  reasons related for code review; it is included as a library within the
  core_extended package for now, but may move to another home in the
  future.
- Added `Extended_unix.terminal_width : int Lazy.t`.
- Added `Interval_map` module.
- Added to `Sendmail.send` additional optional arguments:
  `?message_id:string`, `?in_reply_to:string`.

## 112.24.00

- Added to `Shell.set_defaults` a `?preserve_euid:bool` argument, which causes
  `Shell` to use `bash -p`.
- Removed `Array.Access_control`, now that there is
  `Core.Std.Array.Permissioned`.
- Removed `Fast_int_div`.

## 112.17.00

- Added functions to `Low_level_debug` to get a sexp or string
  representation of any type.

  This could  be handy when debugging polymorphic code.
- Renamed `String.is_substring` to `is_substring_deprecated`.  Use
  `Core.String.is_substring` instead.
- Fixed a bug in `Bin_io_utils.save`.
- Made `Unix.Mac_address` match `Hashable.S`.

## 112.06.00

- Sped up `String.is_substring` by replacing the OCaml implementation
with a call to libc `memmem`.

    `memmem` runs in 20% of the time, incurs minimal GC pressure, is
    portable among UNIXen that we target, AND it's clearer than the ML
    version.

- Made `Float_ref` support `bin_io` and `sexp`.
- Removed `gettid`, which is now available in `Core.Unix`.
- Added `Fast_int_div` module, which speeds up integer division by
  a fixed divisor.
- Moved `Sexp.of_sexp_allow_extra_fields` to core_kernel.

## 112.01.00

- Added `Float_ref` module, which is like `float ref` but faster for
  sets due to bypassing the write barrier.

  Benchmark results on Sandy Bridge:

  | [float\_ref.ml:] float ref set  |    2\_886.94ns |   8.00w |            |
  | [float\_ref.ml:] Float\_ref.set |       355.76ns |   6.00w |            |
  | [float\_ref.ml:] float ref get  |       415.52ns |   6.00w |            |
  | [float\_ref.ml:] Float_ref.get  |       416.19ns |   6.00w |            |
- Added `Bin_io_utils.Wrapped.t`, which defines an `'a t with bin_io`
  that supports size-prefixed serialization and deserialization.

  `Wrapped` has two useful submodules, `Opaque` and `Ignored`, for
  efficient handling of size-prefixed bin-io values in cases where
  serialization can be bypassed.  See the comments in the module for
  more details.

## 111.28.00

- Implemented `Int.gcd` using binary GCD in C, for improved performance.
- Added `Bin_io_utils.Serialized`, which stores a value in memory as its
  bin-io representation.

    Writing such a value just blits the value.
- Moved `Text_block` from `Core_extended` into `Textutils`.
- Added modules `Hashtbl2` and `Hashtbl2_pair`.

## 111.25.00

- Moved `Quickcheck` from `core`.
- Added [Int.gcd].

## 111.17.00

- Added some functions to `Splay_tree`:
  * `length`
  * `keys`
  * `data`
  * `to_alist`
  * `delete_{after,before}`
  * `map`
  * `map_range`
  * `split`.

## 111.13.00

- Moved `Patience_diff` out of `Core_extended` into its own library.

## 111.11.00

- For `Flang`, added ordering to fields, and added `abs`, `min`, and
  `max` to the language.
- Removed `Loggers` module.

## 111.03.00

- Added `Set_lang`, a DSL for sets with constants, union, intersection,
  and difference.
- In `Process`, use `epoll` rather than `select` when possible,

    This prevents errors when selecting on file descriptors numbered
    greater than `FD_SETSIZE` (1024).

- Removed `Syslog` module. There is now `Unix.Syslog` in core instead;
  the APIs are not compatible, but they are similar.

## 109.58.00

- Cleaned up the `Stats_reporting` module

## 109.55.00

- Added `Service_command.acquire_lock_exn`, for acquiring a service's lock.

## 109.53.00

- Fixed `Flang` and `Olang` to round-trip via sexps, i.e. `(t_of_sexp
  (sexp_of_t t)) = t`.

## 109.52.00

- Removed `Sexp.load_sexp_with_includes`; one should use the new
  `Sexplib.Macro` functions.
- Added `Blang`-like languages `Flang` and `Olang`.
  * `Flang` -- terms over a field.
  * `Olang` -- predicates over an ordered set.

## 109.45.00

- Fixed `Core_extended.Sys.groups` to use `Unix.Group.getbygid` rather
  than `Unix.Group.getbygid_exn`.

  This handles when a group is deleted and its gid remains in the
  cache, which causes `Unix.Group.getbygid_exn` to fail because the
  gid no longer resolves to a group.

## 109.40.00

- Added `Stats_reporting.Delta`, for recording deltas of values.

## 109.36.00

- In `Sexp` module, added ability to expand and compress bash-like brace wildcards.

## 109.35.00

- Added stable versions of types contained in the `Selector` module.

## 109.34.00

- Improved `Sexp.Diff`.

  Labeled arguments, put them in the right order (old before new), and
  rework the code to follow the same convention, and produce the output
  where deletions precede insertions.

## 109.28.00

- In `Shell` functions, made the amount of captured stderr/stdout
  configurable.

## 109.27.00

- In module `Sexp`, changed and renamed `load_includes_in_sexp`.

  From:

  ```ocaml
  val load_includes_in_sexp : ?max_depth:int -> Sexp.t -> Sexp.t
  ```

  to:

  ```ocaml
  val load_sexp_with_includes: ?max_depth:int -> ?buf:string -> string -> Sexp.t
  ```
- Added function `Sexp.Diff.to_string`.
- Previously the only option was to print to `Out_channel`.

