## git version

- Renamed `Result.ok_fst` to `Result.to_either` (old name remains as
  deprecated alias).  Added analogous `Result.of_either` function.

- Removed deprecated values `Array.truncate`, `{Obj_array,
  Uniform_array}.unsafe_truncate`, `Result.ok_unit`, `{Result,
  Or_error}.ignore`.

- Changed the signature of `Hashtbl.equal` to take the data equality
  function first, allowing it to be used with `[%equal: t]`.

- Remove deprecated function `List.dedup`.

- Remove deprecated string mutation functions from the `String` module.

- Removed deprecated function `Monad.all_ignore` in favor of
  `Monad.all_unit`.

- Deprecated `Or_error.ignore` and `Result.ignore` in favor of
  `Or_error.ignore_m` and `Result.ignore_m`.

- `Ordered_collection_common.get_pos_len` now returns an `Or_error.t`

- Added `Bool.Non_short_circuiting`.

- Added `Float.square`.

- Remove module `Or_error.Ok`.

- module `Ref` doesn't implement `Container.S1` anymore.

- Rename parameter of `Sequence.merge` from `cmp` to `compare`.

- Added `Info.of_lazy_t`

- Added `List.partition_result` function, to partition a list of `Result.t`
  values

- Changed the signature of `equal` from `'a t -> 'a t -> equal:('a -> 'a ->
  bool) -> bool` to `('a -> 'a -> bool) -> 'a t -> 'a t -> bool`.

- Optimized `Lazy.compare` to check physical equality before forcing the lazy
  values.

- Deprecated `Args` in the `Applicative` interface in favor of using `ppx_let`.

- Deprecated `Array.replace arr i ~f` in favor of using `arr.(i) <- (f (arr.(i)))`

- Rename collection length parameter of `Ordered_collection_common` functions
  from `length` to `total_length`, and add a unit argument to `get_pos_len` and
  `get_pos_len_exn`.

- Removed functions that were deprecated in 2016 from the `Array` and `Set`
  modules.

- `Int.Hex.of_string` and friends no longer silently ignore a suffix
  of non-hexadecimal garbage.

- Added `?backtrace` argument to `Or_error.of_exn_result`.

- `List.zip` now returns a `List.Or_unequal_lengths.t` instead of an `option`.

- Remove functions from the `Sequence` module that were deprecated in 2015.

- `Container.Make` and `Container.Make0` now require callers to either provide a
  custom `length` function or request that one be derived from `fold`.
  `Container.to_array`'s signature is also changed to accept `length` and `iter`
  instead of `fold`.

- Exposed module `Int_math`.

## v0.11

- Deprecated `Not_found`, people who need it can use `Caml.Not_found`, but its
  use isn't recommended.

- Added the `Sexp.Not_found_s` exception which will replace `Caml.Not_found` as
  the default exception in a future release.

- Document that `Array.find_exn`, `Array.find_map_exn`, and `Array.findi_exn`
  may throw `Caml.Not_found` _or_ `Not_found_s`.

- Document that `Hashtbl.find_exn` may throw `Caml.Not_found` _or_
  `Not_found_s`.

- Document that `List.find_exn`, and `List.find_map_exn` may throw
  `Caml.Not_found` _or_ `Not_found_s`.

- Document that `List.find_exn` may throw `Caml.Not_found` _or_ `Not_found_s`.

- Document that `String.lsplit2_exn`, and `String.rsplit2_exn` may throw
  `Caml.Not_found` _or_ `Not_found_s`.

- Added `Sys.backend_type`.

- Removed unnecessary unit argument from `Hashtbl.create`.

- Removed deprecated operations from `Hashtbl`.

- Removed `Hashable.t` constructors from `Hashtbl` and `Hash_set`, instead
  favoring the first-class module constructors.

- Removed `Container` operations from `Either.First` and `Either.Second`.

- Changed the type of `fold_until` in the `Container` interfaces. Rather than
  returning a `Finished_or_stopped_early.t` (which has also been removed), the
  function now takes a `finish` function that will be applied the result if `f`
  never returned a `Stop _`.

- Removed the `String_dict` module.

- Added a `Queue` module that is backed by an `Option_array` for efficient and
  (non-allocating) implementations of most operations.

- Added a `Poly` submodule to `Map` and `Set` that exposes constructors that
  use polymorphic compare.

- Deprecated `all_ignore` in the `Monad` and `Applicative` interfaces in favor
  of `all_unit`.

- Deprecated `Array.replace_all` in favor of `Array.map_inplace`, which is the
  standard name for that sort of operation within Base.

- Document that `List.find_exn`, and `List.find_map_exn` may throw
  `Caml.Not_found` _or_ `Not_found_s`.

- Make `~compare` a required argument to `List.dedup_and_sort`, `List.dedup`,
  `List.find_a_dup`, `List.contains_dup`, and `List.find_all_dups`.

- Removed `List.exn_if_dup`. It is still available in core_kernel.

- Removed "normalized" index operation `List.slice`. It is still available in
  core_kernel.

- Remove "normalized" index operations from `Array`, which incluced
  `Array.normalize`, `Array.slice`, `Array.nget` and `Array.nset`. These
  operations are still available in core_kernel.

- Added `Uniform_array` module that is just like an `Array` except guarantees
  that the representation array is not tagged with `Double_array_tag`, the tag
  for float arrays.

- Added `Option_array` module that allows for a compact representation of `'a
  optoin array`, which avoids allocating heap objects representing `Some a`.

- Remove "normalized" index operations from `String`, which incluced
  `String.normalize`, `String.slice`, `String.nget` and `String.nset`. These
  operations are still available in core_kernel.

- Added missing conversions between `Int63` and other integer types,
  specifically, the versions that return options.

- Added truncating versions of integer conversions, with a suffix of
  `_trunc`.  These allow fast conversions via bit arithmetic without
  any conditional failure; excess bits beyond the width of the output
  type are simply dropped.

- Added `Sequence.group`, similar to `List.group`.

- Reimplemented `String.Caseless.compare` so that it does not
  allocate.

- Added `String.is_substring_at string ~pos ~substring`.  Used it as
  back-end for `is_suffix` and `is_prefix`.

- Moved all remaining `Replace_polymorphic_compare` submodules from Base
  types and consolidated them in one place within `Import0`.

- Removed `(<=.)` and its friends.

- Added `Sys.argv`.

- Added a infix exponentation operator for int.

- Added a `Formatter` module to reexport the `Format.formatter` type and updated
  the deprecation message for `Format`.

## v0.10

(Changes that can break existing programs are marked with a "\*")

### Bugfixes

- Generalized the type of `Printf.ifprintf` to reflect OCaml's stdlib.

- Made `Sequence.fold_m` and `iter_m` respect `Skip` steps and explicitly bind
  when they occur.

- Changed `Float.is_negative` and `is_non_positive` on `NaN` to return `false`
  rather than `true`.

- Fixed the `Validate.protect` function, which was mistakenly raising exceptions.

### API changes

- Renamed `Map.add` as `set`, and deprecated `add`. A later feature will add
  `add` and `add_exn` in the style of `Hashtbl`.

- A different hash function is used to implement `Base.Int.hash`.
  The old implementation was `Int.abs` but collision resistance is not enough,
  we want avalanching as well.
  The new function is an adaptation of one of the
  [Thomas Wang](http://web.archive.org/web/20071223173210/http://www.concentric.net/~Ttwang/tech/inthash.htm)
  hash functions to OCaml (63-bit integers), which results in reasonably good avalanching.


- Made `open Base` expose infix float operators (+., -., etc.).

* Renamed `List.dedup` to `List.dedup_and_sort`, to better reflect its existing behavior.

- Added `Hashtbl.find_multi` and `Map.find_multi`.

- Added function `Map.of_increasing_sequence` for constructing a `Map.t` from an
  ordered `Sequence.t`

- Added function `List.chunks_of : 'a t -> length : int -> 'a t t`, for breaking
  a list into chunks of equal length.

- Add to module `Random` numeric functions that take upper and lower inclusive
  bounds, e.g. `Random.int_incl : int -> int -> int`.

* Replaced `Exn.Never_elide_backtrace` with `Backtrace.elide`, a `ref` cell that
  determines whether `Backtrace.to_string` and `Backtrace.sexp_of_t` elide
  backtraces.

- Exposed infix operator `Base.( @@ )`.

- Exposed modules `Base.Continue_or_stop` and `Finished_or_stopped_early`, used
  with the `Container.fold_until` function.

- Exposed module types Base.T, T1, T2, and T3.

- Added `Sequence.Expert` functions `next_step` and
  `delayed_fold_step`, for clients that want to explicitly handle `Skip` steps.

- Added `Bytes` module.
  This includes the submodules `From_string` and `To_string` with blit
  functions.
  N.B. the signature (and name) of `unsafe_to_string` and `unsafe_of_string` are
  different from the one in the standard library (and hopefully more explicit).

- Add bytes functions to `Buffer`.
  Also added `Buffer.content_bytes`, the analog of `contents` but that returns
  `bytes` rather than `string`.

* Enabled `-safe-string`.

- Added function `Int63.of_int32`, which was missing.

* Deprecated a number of `String` mutating functions.

- Added module `Obj_array`, moved in from `Core_kernel`.

* In module type `Hashtbl.Accessors`, removed deprecated functions, moving them
  into a new module type, `Deprecated`.

- Exported `sexp_*` types that are recognized by `ppx_sexp_*` converters:
  `sexp_array`, `sexp_list`, `sexp_opaque`, `sexp_option`.

* Reworked the `Or_error` module's interface, moving the `Container.S` interface
  to an `Ok` submodule, and adding functions `is_ok`, `is_error`, and `ok` to
  more closely resemble the interface of the `Result` module.

- Removed `Int.O.of_int_exn`.

- Exposed `Base.force` function.

- Changed the deprecation warning for `mod` to recommend `( % )` rather than
  `Caml.( mod )`.

### Performance related changes

- Optimized `List.compare`, removing its closure allocation.

- Optimized `String.mem` to not allocate.

- Optimized `Float.is_negative`, `is_non_negative`, `is_positive`, and
  `is_non_positive` to avoid some boxing.

- Changed `Hashtbl.merge` to relax its equality check on the input tables'
  `Hashable.t` records, checking physical equality componentwise if the records
  aren't physically equal.

- Added `Result.combine_errors`, similar to `Or_error.combine_errors`, with a
  slightly different type.

- Added `Result.combine_errors_unit`, similar to `Or_error.combine_errors_unit`.

- Optimized the `With_return.return` type by adding the `[@@unboxed]` attribute.

- Improved a number of deprecation warnings.


## v0.9

Initial release.
