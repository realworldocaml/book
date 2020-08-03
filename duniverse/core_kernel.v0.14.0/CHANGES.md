As Core\_kernel is built on top of [Base](https://github.com/janestreet/base),
you might want to have a look at Base's
[changelog](https://github.com/janestreet/base/blob/master/CHANGES.md).

## git version

- Deprecated `failwithp` in favor of `failwiths`, and made
  `failwiths`'s `~here` argument required.

- Renamed functions in `Fqueue` to make it consistent with `Queue` and
  `Fdeque`.  `top`, `top_exn`, and `discard_exn` remain as deprecated
  aliases to `peek`, `peek_exn`, and `drop_exn`.

- Removed deprecated values `Fqueue.{enqueue_top, bot_exn, bot}`.

- Removed `Obj_array` in favor of `Uniform_array`.

- Remove deprecated `Std` module.

## v0.12

- Added `Digit_string_helpers.read_int63_decimal`.

- Added `List.zip_with_remainder` which zips as many elements as possible and
  then returns the unzipped remained of the longest input, if the input lists
  have different lengths.

- Added `Bigbuffer.add_bin_prot` to append the bin-protted representation of a
  value at the end of the buffer.

- Reexport `Base.Buffer`.

- Added labels to function parameters in the `Quickcheck` module.

- Deprecate `Timing_wheel_ns.alarm_upper_bound` in favor of
  `Timing_wheel_ns.max_allowed_alarm_time`.

- Deprecated `Array.replace arr i ~f` in favor of using `arr.(i) <- (f (arr.(i)))`

- Removed functions that were deprecated in 2016 and 2017 from the `Array` and
  `Set` modules.

- Removed `Int_replace_polymorphic_compare` in favor of
  `Int.Replace_polymorphic_compare`.

## v0.11

- Add `Bigstring.unsafe_resize` to allow reallocating in place.

- Moved `Splittable_random` to its own library.
  Available at http://github.com/janestreet/splittable_random

- Remove `Time.*.Stable`, leaving `Time.Stable` as the only submodule
  that exports stable conversions.
  Clients of `Time` should refer to stable modules as `Time.Stable.X`
  where they were previously using `Time.X.Stable`.

- Added `Md5.digest_bin_prot`.
  This gives an easy way to write `t -> Md5.t` for any Binable value.

- Added a `Stable_comparable.V1` module type.

- Changed `fold_until`'s interface: instead of returning a
  `Finished_or_stopped_early.t` it now takes a `finish` callback.

- Deprecated the [Std] module.

## v0.10

- Renamed `Float.to_string` as `to_string_12`, to reflect its 12-digit
  precision. And introduce a new `Float.to_string` with the behavior of
  `Float.to_string_round_trippable`.

- Many changes to `Set_once`, including requiring `%here` at calls to `get_exn`,
  `set`, and `set_exn`

- Improved `Quickcheck`'s interface for giving explicit length values or ranges
  for random lists and strings.

- Removed `Stable_workaround` modules that are no longer necessary since we
  upgraded to OCaml 4.04.

- Exposed `Date.add_days_skipping`, a generalization of `add_business_days` and
  `add_weekdays`

- Added `Total_map.data`, that just calls `Map.data` on the underlying map

- Deprecated `Array.empty`, which was already deprecated in Base.

- Added function `Gc.add_finalizer_last`, which is like `add_finalizer`, except
  that the function is only called when the value has become unreachable for the
  last time.

- Added to module `Maybe_bound`: `[@@deriving bin_io]` and module `Stable`.

- Added module `Optional_syntax`, with interfaces `S`, `S1`, and `S2`, used by
  modules that expose an `Optional_syntax` submodule for use with
  `match%optional.`

- Switched `Weak_hashtbl` to use `Gc.add_finalizer_last`, rather than
  `Weak_pointer`'s emulation based on `Ephemeron.`

- Re-implemented `Weak_pointer` directly in terms of OCaml's `Weak` module,
  rather than using `Ephemeron.`

- Moved `Bigstring.map_file` to `Core`, since it depends `on` `Unix.`

- Optimized `Heap.merge_pairs` by removing a closure allocation.

- Fixed `String_id`'s no-whitespace-on-edge check for `String_id.Set` etc.

- Changed `Version_util` to store build_time as a `Time.t` rather than using a
  `Date.t` and a Time.Ofday.t.

- Added functor `String_id.Make_with_validate`, so that we can create
  identifiers that perform custom validation on creation.

- Added `Core_kernel.print_s`, for pretty printing a sexp to stdout.
  (jane/Core.print_s)

- Added to `Bigstring` a number of bounds-checked versions of functions dealing
  with integers, corresponding to existing unsafe versions.

- Added `String.take_while` and `rtake_while.`

- Improved `Heap.sexp_of_t` and `Heap.Removable.sexp_of_t.`

- Added `Md5` module, a wrapper around OCaml's `Digest` module.

- Added stable serialization of `Time`, in `Time.Stable.With_utc_sexp.V2.`

- In `Univ_map`, exposed `Type_equal.Id.t` for `Key.t` types.

- Changed `Gc.Stat.sexp_of_t` so it no longer drops precision in the
  `minor_words`, promoted_words, and `major_words` fields.

- Added `Map.merge_skewed` function that only traverses one of its arguments,
  unlike `Map.merge`, which traverses both.

- Improved `Obj_array` inlining.

- Optimized `Deque.clear` to only walk the queue, rather than the entire backing
  array.

- Optimized `Bus` by adding `writeN` functions that can be inlined.

- Removed `Bus`'s variable arity write function, making write be for arity-1
  buses only, with `write2`, `write3`, and `write4` for other arities.

- Added `Core_kernel_stable.Time`, which includes stable types for `Time.Span.t`
  and `Time.With_utc_sexp.t..`

- In `Unique_id.Id`, changed `Hashable` to `Hashable.S_binable.`

- Exposed `Core_kernel.ifprintf.`

- Added module `Queue.Stable.`

- Changed `Bus.create`'s `allow_subscription_after_first_write` argument to
  `on_subscription_after_first_write` and added a choice that causes the bus to
  remember the last value written to it and send it to new subscribers.

- Renamed `Flags.subset` to `is_subset`, for `consistency.`

- Made module `Core_kernel.Bytes` be an extension of `Base.Bytes.`

- Add `bytes` functions to `Bigstring` and `Bigbuffer.`

- Exposed type `Host_and_port.t` as a record type.

- Added function `Total_map.for_all.`

- Added function `Date.add_years`, which just calls `Date.add_months` d `12*n`.

- Moved `Time.Ofday.of_string` parsing into a separate module so that it can
  eventually be shared between `Time` and `Time_ns.` Made `of_string` reject
  nonsense inputs (e.g 0:00:0 and 1:-0:3e1).

- Reworked `Time.Ofday.to_string` to avoid using to_parts, improving its
  performance.

- Made `Time.Span.Parts` and `Time_ns.Span.Parts` the same by adding the ns
  field to `Time.Span.Parts.` Updated create functions for `Span` and `Ofday` to
  accept `?ns`. Fixed `Time_ns.Ofday.create` to be precise rather than
  round-tripping through `Time.Ofday`.

- Optimized `Pool`, `Thread_safe_queue`, `Time_ns`, and `Timing_wheel_ns` by
  moving some error branches into `[@inline never]` functions.

- Merged `Heap.Removable` into `Heap`.

- Changed `Heap.remove`'s implementation to use the usual pairing-heap delete
  algorithm, which has amortized `O(log(n))` complexity the same complexity as
  removing the min value, without the memory problem of the current
  implementation.

- Added module `Heap.Unsafe`, with non-allocating alternatives to `Elt.t`.

- Removed `Time`'s and `Time_ns`'s `Ofday.end_of_day` value, and added
  `start_of_next_day` and `approximate_end_of_day` as replacements.

- Deprecated `blit` functions that mutate strings.

- Switched `Core_kernel` to `-safe-string`.

- Fixed `String`'s quickcheck generator to use `size` as an upper bound.

- Added function `Sequence.merge_all`, which uses `Fheap`.

- Added module type `Identifiable.S_plain`, which is like `Identifiable.S` but
  does not export `t_of_sexp`.

- Deprecated `Bigsubstring`'s and `Substring`'s `of_string` and `of_bigstring`
  functions. One should use create for sharing.

- Added `Date.Days` module, a date representation optimized for linear
  arithmetic (e.g. `add_days`) rather than for extracting year/month/day.

- Changed `Hashtbl`, `Hash_set`, `Map`, and `Set` generic creation functions to
  use a first-class module like `Bas`e rather than a `comparator` or `hashable`.

- Added `Quickcheck.test_or_error`, an `Or_error`-based version of test.

- Split `Time.Zone.shift_epoch_time` into `absolute_time_of_relative_time` and
  `relative_time_of_absolute_time` to make its uses clearer.

## v0.9

## 113.43.00

- This feature implements `String.Caseless.is_prefix` and
  `String.Caseless.is_suffix` functions which check if some string is
  prefix or suffix of another ignoring case.

- `Hash_set` now supports the intersection operation

- Add functions to create Maps and Sets from Hashtbls or a Hash_sets.  Existing
  code that do that sort of things usually end up going through an intermediate
  assoc list, which is not particularly efficient.  The friction of inlining
  something better at the app level feels just too verbose so usually it's not
  done.  We hope that by offering the right util in core, those call sites can be
  updated for something both shorter and more efficient.

  Note: we do not currently carry `'cmp cmp` witnesses into our Hashtbls and
  Hash_sets the same way we do it for Maps and Sets.  There exists cases where the
  following code would actually raise:

  let map_of_hashtbl (hashtbl : (M.t, 'a) Hashtbl.t) =
    hashtbl
    |> Hashtbl.to_alist
    |> M.Map.of_alist_exn
  ;;

  If the hashtbl and the M.Map do not use the same compare function, and there
  exists some keys a1, and a2 such that:

  (Hashtbl.hashable hashtbl).compare a1 a2 <> 0 && M.compare a1 a2 = 0

  So, in the context of that feature there was a choice to be made.  Either
  `Map.of_hashtbl` can silently replace previous binding while folding over the
  hashtbl, or can raise.

  Conservatively, the choise to raise was taken, thus the function has the usual
  `_exn` suffix: `Map.of_hashtbl_exn`

  For sets, the context is suffisiently different so as to deliberatly not apply
  the same approach.  Like when using `Set.of_list` one wants to aggreate values
  from a container into a set.  Hashtbl keys and Hash_set values are just a
  different kind of container than a list, but the added value of raising in that
  function in case the hash set or the hashtbl have dups is not clear, so that
  path was not pursued.

  With more work, we could (and someday maybe will) have `Hashtbl` and `Hash_set`
  carry comparison witnesses and create versions of those functions that cannot
  raise and reuse the comparison and the compare witness of the hashtbl or
  hash_set.

  In the process of implementing `Map.of_hashtbl_exn` it appeared that
  `Map.of_alist` was inefficiently doing the lookup twice for each element to be
  inserted.  The feature fixes this.

- Name the non-`t` arguments to `Error.tag` and similar functions to allow
  easier partial application.

- Name the non-`t` arguments to `Error.tag` and similar functions to allow
  easier partial application.

- Map.Stable

  Added Map.Stable, including a Make functor for making stable map types.

- Binary search by time for `Queue_ts`.

- Automatic, randomized testing based on Haskell's "Quickcheck" library.

- Introduce `Quickcheck.Generator.geometric` and add/modify functions based on it:

  - rename `Generator.size` to `Generator.small_non_negative_int`
  - add `Generator.small_positive_int`
  - document the above in terms of `Generator.geometric`

- The following segfaults:

    open Core.Std;;
    let s = Stack.create();;
    Stack.push s 1.0;;
    Stack.push s 2.0;;
    Stack.push s 3.0;;

  This is because we put floats together with non-floats in the same
  array without care.

  In particular, if you call `Array.init ~f` such that `f 0` returns a
  float, then ocaml will decide to create an unboxed float array (tagged
  with `Double_array`). It will then proceed to call `f i` and try to
  unbox each assuming they all are pointers to floats. If `f i` happens
  to return an immediate (such as `Obj.magic ()`) instead of a pointer,
  this segfaults.

  `Queue` and `Deque` don't seem to suffer from the same problem because
  they both create arrays initially populated with immediates so the
  arrays end up not tagged with `Double_array`. It happens that it's
  safe to put floats into such arrays, so let's use the same trick in
  `Stack`. The plan is to eventually use (a safety wrapper over)
  `Obj_array` in all of `Stack`, `Queue`, `Deque` (`jane/stack-segfault`
  feature).

- Added Set.Stable, including a Make functor for making stable set types.

- Renamed the "Stable" module type to "Stable_without_comparator", in anticipation
  of requiring a comparator and comparator witness in the module type called
  "Stable".

  This is the first in a chain of features which will push us towards
  including comparator witnesses in stable type definitions, so that
  defining stable sets and maps is easier.

- Added back a "Stable" module type that now includes a comparator
  witness type and corresponding comparator value.

  Defining the comparator stuff in a stable way will allow us to define
  stable set and map types that are equivalent to their non-stable
  counterparts.  (See child features)

  Along the way, added `Identifiable.Make_using_comparator` to the
  family of `_using_comparator` functors, to help with this task.

- Introduce a Blang submodule which has infix operators and other convenient shortcuts.

  In a world where increasingly we are writing configuration as OCaml code, it seems right
  that we should focus not only on the sexp DSL but the OCaml one as well.

- Moved the Stable `Comparable.V1.Make` into `comparable.ml` (as usual)
  rather than in `stable_containers.ml`.

- Renamed:

    lib/core_kernel/test --> lib/core_kernel/test-bin

  since these directories contain executables to run rather than
  libraries with standard unit tests.  This is in preparation for moving
  the standard unit tests to a more "normal" test directory.

- Move unit tests from `core_kernel/src` to `core_kernel/test`.

- Split out a sub-signature of `Binable.S` containing only functions, for
  use in the definition of recursive modules.

- Split out a `*_using_comparator` variant of the functor
  `Comparable.Map_and_set_binable`.

- Automatic, randomized testing based on Haskell's "Quickcheck" library.

- Add a flag to `Quickcheck.test_no_duplicates` to allow some percentage of values to be
  duplicates.

  This is primarily in preparation for changing the "no-duplicates" tests for random
  function generation to be extensional (based on results for a fixed set of inputs) rather
  than intensional (based on sexps constructed by Quickcheck).  This design change is also a
  good axis of flexibility in general.

- Remove the `exception` declarations in quickcheck.ml and use `Error.raise_s` and
  ``%message`` instead.

- In quickcheck.ml, swap the order of `module Observer` and `module Generator`.  This
  feature just swaps them and makes no other change.  This is in preparation for an upcoming
  feature that will introduce dependency of `Generator` on `Observer`, which will be easier
  to read as an incremental diff.

- Add a top-level filter option to `Quickcheck.test`.  The behavior of top-level filter is a
  lot easier to reason about than nested recursive filters, especially with respect to
  attempts-vs-failures.  This is in preparation for removing generator "failure" as a
  first-order concept and simplifying the model of generators.

- Add `Container.S0` + a `sub` function to substring stuff

- Adds phantom type to Validated as witness of invariant

  The witness prevents types from different applications of the Validated functors
  from unifying with one another.

- Added Int.Stable to Core.Stable.

- Added `Fqueue.of_list`, an inverse of `Fqueue.to_list`.

- Added Int.Stable.V1, implementing Comparable.Stable.V1.S, so that one
  can use Int.V1.Map.t and Int.V1.Set.t in stable types.

- Remove the `Quickcheck.Generator.fn_with_sexp` type and all the sexp arguments to
  `Quickcheck.Observer.t` constructors.

- Added String.Stable.V1, implementing Comparable.Stable.V1.S, so that
  one can use String.V1.Map.t and String.V1.Set.t in stable types.

- Added to `Monad.Syntax.Let_syntax`:

    val return : 'a -> 'a t

  so that when one does:

    open Some_random_monad.Let_syntax

  `return` is in scope.

  Most of the diff is the addition of `let return = return` in the
  necessary places.  The rest is changing uses of `return` to
  `Deferred.return` in contexts where some other monad was `open`ed,
  shadowing `Deferred.return`.

- Implement `Sequence.of_lazy` to allow entirely lazily-computed sequences
  (rather than just lazily computing the elements).

- Add Array.random_element

- Container.fold_result-and-until

  `Containers` learned to fold using a `f` that returns a `Result.t`, bailing out early if necessary

  `Containers` also learned to `fold_until`: fold using a `f` that returns

    `Continue of 'a | Stop of 'b`

  terminating the fold when `f` returns `Stop _`.

  `fold_until` evaluates to
  `Finished      of 'a` if `f` never returns `Stop _`
  `Stopped_early of 'b` when the `f` returns `Stop _`

- Deprecates most of the `In_channel` and `Out_channel` equivalents in
  Pervasives, deleting a little bit of garbage along the way

- Emulate 63bit integers on 32bit platform so that we have same semantic
  in 32bit and 64bit arch.
   - same bin_prot
   - same max_value/min_value

  We use the same kind of encoding as native int on 64bit architecture (with a
  twist). A 63bit integer is a 64bit integer with its bits shifted to the left.
  (In OCaml, in 64 bit, an int is a 64 bit integer shifted to the left, with the
  immediate bit set to 1).

- Add `Fqueue.map` implementation.

- This is the followup to the earlier deprecation of `Map.iter` and
  `Hashtbl.iter`.

  * Changes the deprecated `Map.iter` and `Hashtbl.iter` and
    `Map.filter` functions to iterate over values only instead of both
    keys and values. (For the old behavior, use the non-deprecated
    `iteri` or `filteri` functions instead).

  * Analogous changes have been made to `Deferred_map`, `Multi_map`,
    `Total_map`, `Fold_map`, `Extended_hashtbl`, `Pooled_hashtbl`,
    `Bounded_int_table`, `Imm_hash`.

  This may break code that upgrades directly to this version from
  before these functions were deprecated, or code that continued to
  use `Map.iter` or `Hashtbl.iter` or `Map.filter` after
  deprecation. As mentioned above, use `iteri` and `filteri` instead.

  Additionally:

  * Deprecates `Hashtbl.iter_vals`. (Use `Hashtbl.iter` instead.)

  * Adds some missing functions to a few of the minor associative
    container classes.

- Add popcount (count # of 1 bits in representation) operation for int types.

    ┌───────────────────────────────────────┬──────────┬────────────┐
    │ Name                                  │ Time/Run │ Percentage │
    ├───────────────────────────────────────┼──────────┼────────────┤
    │ `int_math.ml` popcount_bench_overhead │   2.11ns │     57.12% │
    │ `int_math.ml` int_popcount            │   3.17ns │     85.72% │
    │ `int_math.ml` int32_popcount          │   3.70ns │     99.95% │
    │ `int_math.ml` int64_popcount          │   3.70ns │     99.96% │
    │ `int_math.ml` nativeint_popcount      │   3.70ns │    100.00% │
    └───────────────────────────────────────┴──────────┴────────────┘

- Fix `Time_ns.next_multiple` to use integer division instead of floating point division.

- Move the contents of `time_ns.mli` to `time_ns_intf.ml` in both core and core\_kernel,
  and clean up the presentation of the signatures a bit in both.

  Also adds `Int63` to `Std_internal`.

- Move `Unit_of_time` out of `Core.Time.Span` and into `Core_kernel`.
  Move `Core.Time_ns.Span.{to,of}_unit_of_time` into `Core_kernel.Time_ns.Span`.

- Add a stable submodule to Byte_units

- Deleted `Core_kernel.Flat_queue`, which is unused.

- Deprecated `Array.empty`, in favor of ` `||` `.

- Add some useful functions to Or_error

- Improve the interface and error message of `Quickcheck.test_no_duplicates`.

  The function no longer supports equality-based duplicate tests, which were
  unused and inefficient.  It only supports compare-based tests.

  The error message now groups values with the number of duplicates produced,
  sorted in descending order so the most common duplicates come early.  It also
  includes all duplicates generated up to the maximum trial count, rather than
  stopping as soon as the cutoff threshold is reached.

- Now that the Decimal module no longer means "decimal" (it's only
  functionality is to change sexp converters and bin-io to rejecting nan
  and inf values), rename it as `Float_with_finite_only_serialization`, to better
  befit its semantics.

  In addition to a pile of renames, this also changes many references to
  `Decimal.t` or `decimal` in mlis to `float`, since the decimal type
  didn't really convey any extra semantics about the type; just about
  the serializers.

- Prompted by the embarrassing Stack segfault bug we decided to put the
  unsafe `Obj.magic` stuff present in various array-backed data
  structures (Queue, Deque, Stack) in a single place.

  We introduce the following new modules:

  * `Uniform_array`: a wrapper on top of `Obj_array` that makes the
    elements homogeneous. It's equivalent to `Array` in semantics, but
    differs in performance and in how it interacts with `Obj.magic`.

  * `Option_array`: `'a Option_array.t` is semantically equivalent to
    `'a Option.t Array.t`, but avoids allocation of `Some` values,
    instead representing `None` by `Obj.magic`'ing a distinguished
    value.

  On top of making things safer, this feature happens to improve
  performance:

  Original benchmarks:

    $ ./array_queue_old.exe -quota 2
    Estimated testing time 1.43333m (43 benchmarks x 2s). Change using -quota SECS.
    ┌────────────────────────────────────┬─────────────────┬─────────────┬───────────────┬──────────┬────────────┐
    │ Name                               │        Time/Run │     mWd/Run │      mjWd/Run │ Prom/Run │ Percentage │
    ├────────────────────────────────────┼─────────────────┼─────────────┼───────────────┼──────────┼────────────┤
    │ enqueue_dequeue_mixed              │ 36_016_512.46ns │ 999_835.00w │ 2_096_659.14w │    7.14w │    100.00% │
    │ pipeline                           │        175.89ns │             │               │          │            │
    │ blit_transfer 0                    │          7.39ns │             │               │          │            │
    │ blit_transfer 1                    │         78.26ns │             │               │          │            │
    │ blit_transfer 2                    │         93.91ns │             │               │          │            │
    │ blit_transfer 4                    │        127.53ns │             │               │          │            │
    │ blit_transfer 8                    │        195.41ns │             │               │          │            │
    │ blit_transfer 16                   │        351.20ns │             │               │          │            │
    │ blit_transfer 32                   │        647.15ns │             │               │          │            │
    │ blit_transfer 64                   │      1_168.12ns │             │               │          │            │
    │ blit_transfer 128                  │      2_288.00ns │             │               │          │            │
    │ enqueue 10                         │        440.79ns │      42.00w │               │          │            │
    │ enqueue 1000000                    │ 23_268_213.72ns │     526.00w │ 2_096_658.71w │    6.71w │     64.60% │
    │ Queue.enqueue + dequeue:1          │         15.59ns │             │               │          │            │
    │ Queue.enqueue + dequeue:2          │         14.38ns │             │               │          │            │
    │ Queue.enqueue + dequeue:4          │         15.23ns │             │               │          │            │
    │ Queue.enqueue + dequeue:8          │         16.40ns │             │               │          │            │
    │ Queue.enqueue + dequeue:16         │         16.33ns │             │               │          │            │
    │ Queue.enqueue + dequeue:32         │         16.44ns │             │               │          │            │
    │ Queue.enqueue + dequeue:64         │         16.54ns │             │               │          │            │
    │ Queue.enqueue + dequeue:128        │         15.18ns │             │               │          │            │
    │ Queue.enqueue + dequeue:256        │         16.58ns │             │               │          │            │
    │ Queue.enqueue + dequeue:512        │         16.65ns │             │               │          │            │
    │ Linked_queue.enqueue + dequeue:1   │         53.33ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:2   │         53.95ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:4   │         54.64ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:8   │         55.18ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:16  │         55.57ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:32  │         55.78ns │       3.00w │         3.03w │    3.03w │            │
    │ Linked_queue.enqueue + dequeue:64  │         55.67ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:128 │         56.64ns │       3.00w │         3.03w │    3.03w │            │
    │ Linked_queue.enqueue + dequeue:256 │         56.28ns │       3.00w │         3.03w │    3.03w │            │
    │ Linked_queue.enqueue + dequeue:512 │         56.15ns │       3.00w │         3.03w │    3.03w │            │
    │ Deque.enqueue + dequeue:1          │         16.36ns │             │               │          │            │
    │ Deque.enqueue + dequeue:2          │         15.06ns │             │               │          │            │
    │ Deque.enqueue + dequeue:4          │         15.07ns │             │               │          │            │
    │ Deque.enqueue + dequeue:8          │         17.13ns │             │               │          │            │
    │ Deque.enqueue + dequeue:16         │         17.22ns │             │               │          │            │
    │ Deque.enqueue + dequeue:32         │         17.37ns │             │               │          │            │
    │ Deque.enqueue + dequeue:64         │         17.23ns │             │               │          │            │
    │ Deque.enqueue + dequeue:128        │         16.69ns │             │               │          │            │
    │ Deque.enqueue + dequeue:256        │         16.99ns │             │               │          │            │
    │ Deque.enqueue + dequeue:512        │         17.52ns │             │               │          │            │
    └────────────────────────────────────┴─────────────────┴─────────────┴───────────────┴──────────┴────────────┘

  New benchmarks:

    ./array_queue.exe -quota 2;
    Estimated testing time 1.43333m (43 benchmarks x 2s). Change using -quota SECS.
    ┌────────────────────────────────────┬─────────────────┬─────────────┬───────────────┬──────────┬────────────┐
    │ Name                               │        Time/Run │     mWd/Run │      mjWd/Run │ Prom/Run │ Percentage │
    ├────────────────────────────────────┼─────────────────┼─────────────┼───────────────┼──────────┼────────────┤
    │ enqueue_dequeue_mixed              │ 28_693_436.79ns │ 999_835.00w │ 2_096_658.82w │    6.82w │    100.00% │
    │ pipeline                           │        163.08ns │             │               │          │            │
    │ blit_transfer 0                    │          7.40ns │             │               │          │            │
    │ blit_transfer 1                    │         78.14ns │             │               │          │            │
    │ blit_transfer 2                    │         86.72ns │             │               │          │            │
    │ blit_transfer 4                    │        105.65ns │             │               │          │            │
    │ blit_transfer 8                    │        143.44ns │             │               │          │            │
    │ blit_transfer 16                   │        219.64ns │             │               │          │            │
    │ blit_transfer 32                   │        386.75ns │             │               │          │            │
    │ blit_transfer 64                   │        692.08ns │             │               │          │            │
    │ blit_transfer 128                  │      1_331.16ns │             │               │          │            │
    │ enqueue 10                         │        393.34ns │      42.00w │               │          │            │
    │ enqueue 1000000                    │ 18_775_585.92ns │     526.00w │ 2_096_658.55w │    6.55w │     65.44% │
    │ Queue.enqueue + dequeue:1          │         14.53ns │             │               │          │            │
    │ Queue.enqueue + dequeue:2          │         13.73ns │             │               │          │            │
    │ Queue.enqueue + dequeue:4          │         10.61ns │             │               │          │            │
    │ Queue.enqueue + dequeue:8          │         14.56ns │             │               │          │            │
    │ Queue.enqueue + dequeue:16         │         12.14ns │             │               │          │            │
    │ Queue.enqueue + dequeue:32         │         11.10ns │             │               │          │            │
    │ Queue.enqueue + dequeue:64         │         11.22ns │             │               │          │            │
    │ Queue.enqueue + dequeue:128        │         11.52ns │             │               │          │            │
    │ Queue.enqueue + dequeue:256        │         11.52ns │             │               │          │            │
    │ Queue.enqueue + dequeue:512        │         11.00ns │             │               │          │            │
    │ Linked_queue.enqueue + dequeue:1   │         52.42ns │       3.00w │         3.03w │    3.03w │            │
    │ Linked_queue.enqueue + dequeue:2   │         52.52ns │       3.00w │         2.98w │    2.98w │            │
    │ Linked_queue.enqueue + dequeue:4   │         52.68ns │       3.00w │         2.98w │    2.98w │            │
    │ Linked_queue.enqueue + dequeue:8   │         53.21ns │       3.00w │         2.98w │    2.98w │            │
    │ Linked_queue.enqueue + dequeue:16  │         69.40ns │       3.00w │         2.97w │    2.97w │            │
    │ Linked_queue.enqueue + dequeue:32  │         55.23ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:64  │         54.24ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:128 │         55.25ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:256 │         55.10ns │       3.00w │         3.02w │    3.02w │            │
    │ Linked_queue.enqueue + dequeue:512 │         55.47ns │       3.00w │         3.02w │    3.02w │            │
    │ Deque.enqueue + dequeue:1          │         11.73ns │             │               │          │            │
    │ Deque.enqueue + dequeue:2          │         11.63ns │             │               │          │            │
    │ Deque.enqueue + dequeue:4          │         11.65ns │             │               │          │            │
    │ Deque.enqueue + dequeue:8          │         12.86ns │             │               │          │            │
    │ Deque.enqueue + dequeue:16         │         12.86ns │             │               │          │            │
    │ Deque.enqueue + dequeue:32         │         12.99ns │             │               │          │            │
    │ Deque.enqueue + dequeue:64         │         12.86ns │             │               │          │            │
    │ Deque.enqueue + dequeue:128        │         12.91ns │             │               │          │            │
    │ Deque.enqueue + dequeue:256        │         12.81ns │             │               │          │            │
    │ Deque.enqueue + dequeue:512        │         12.85ns │             │               │          │            │
    └────────────────────────────────────┴─────────────────┴─────────────┴───────────────┴──────────┴────────────┘

- `Array.truncate` makes it really hard to use `unsafe_get` safely: even
  things in `Array` module itself fail to do it properly.

  For example, this thing reliably segfaults:

      let a = Array.create ~len:2 "foo" in
      Array.iter a ~f:(fun s ->
        printf "%s" s;
        Array.truncate a ~len:1)

  We should rename `truncate` to `unsafe_truncate` and stop claiming
  that the array length is allowed to change over time.

- `Byte_units.Stable` uses type substitution, not equality, so there
  is no `Byte_units.Stable.V1.t`

  Fix this by changing to type equality

- Remove `Quickcheck.Generator.failure`.

  This is in preparation for moving to a model of generators for which there is
  no notion of "failure".

- Change `Int*.gen*` to use a uniform distribution.

  A simple distribution is easier to reason about, and does not require tricky
  tuning.  The previous distribution was tuned to hit some notion of "common
  values", and so that it might frequently produce duplicates with itself, which
  was arbitrary, hard to tune, and meant it spent a lot of time generating only
  border cases.

  While some use cases might still want some border-case tuning, uniform
  distributions are probably a better basic building block, and are much easier to
  implement.

## 113.33.01

- Fix segfault in [Stack]

- Fix BSD build problem related to endian.h

## 113.33.00

- Rename Tuple.T2.map1 and Tuple.T2.map2 to `map_fst` and `map_snd`,
  following the convention of the accessors.  Usually, numeric suffixes
  mean higher arities, not different fields.

- After discussion, rather than checking for overflow in `Time_ns`
  arithmetic, clarify that it's silently ignored.  (Subsequent
  conversions may or may not notice.)

  We did identify the set of functions to document:

      Time_ns.Span.((+), (-), scale_int, scale_int63, create, of_parts)
      Time_ns.(add, sub, diff, abs_diff, next_multiple)

  Added `Core_int63.(add_with_overflow_exn, abs_with_overflow_exn, neg_with_overflow_exn)`
  in the course of abandoned work on overflow detection in `Time_ns`.  These may
  be useful.  `mul_with_overflow_exn` was abandoned because
    1. it's a lot of work to review
    2. there's a better approach: Go to the C level and check the high word of
       the product from the IMUL instruction, which is both simpler and faster.

- Changes related to Float.sign

  Float.sign currently maps -1e-8 and nan to Zero.  Some users don't
  expect this.  This feature addresses that via the following changes:

  - `Float.sign` renamed to `Float.robust_sign`.  (`Float.sign` is
    still present, but deprecated.)

  - `Float.sign_exn` introduced which does not use robust comparison,
    and which raises on `nan`.

  - `Sign` pulled out of `Float` and made its own module, and
    `sign : t -> Sign.t` added to `Comparable.With_zero`.  In particular,
    `Int.sign : int -> Sign.t` now exists.  (In looking at existing uses
    of `Float.sign`, there was at least one case where the user was
    converting an int to a float to get its sign.  That ended up being
    deleted, but it still seems desirable to have `Int.sign`.
    There were also record types with a field `sign : Float.Sign.t` where
    logically the type has no real connection to `Float`.)

  - Uses of `Float.robust_sign` revisited to make sure that's the behavior
    we want.

- Added Quickcheck tests for `Hashtbl` functions, using `Map` as a point of comparison.
  A lot of `Hashtbl` functions did not have tests, so I used an interface trick to
  require every function to show up in the test module.  The easiest way to write a readable
  test for every function was to compare it to a similar datatype, so I went with `Map`.

- Allow clients of `Hashtbl.incr` and `Hashtbl.decr` to specify entries should be removed if the value is 0

- The type of `symmetric_diff` has a typo in it.

- Switched `Timing_wheel_ns` to use `%message` and `%sexp`.

- Improved the error message raised by `Timing_wheel.add` and
  `reschedule` if the supplied time is before the start of the current
  interval.

  Previously, the error message was something like:

    ("Timing_wheel.Priority_queue got invalid key"
     (key ...) (timing_wheel ...))

  Now, it will be like:

    ("Timing_wheel cannot schedule alarm before start of current interval"
     (at ...) (now_interval_num_start ...))

  The old message was confusing because `key` is less understandable
  than `at`, and because it didn't make clear that this is a usage
  error, as opposed to a Timing_wheel bug.

  Implementing this efficiently required adding a field to timing wheel:

    mutable now_interval_num_start : Time_ns.t

  so that the check done by `add` is fast.

- Add `Comparable.Make_using_comparator`.

  Since `Map` and `Set` already have `Make_using_comparator` functors,
  there's no particular reason for `Comparable` not to have one.

  More concretely, this will be useful when (if) we add stable
  containers to core: we can add a stable version of `Comparable.Make`,
  then pass the resulting comparator into the unstable functor to get
  equal types.

- Add a `Total_map.Make_using_comparator` functor to allow the creation
  of total maps which are type equivalent to regular maps.

- Change default major heap increments to be % of the heap size instead of a constant increment

  Also changed type of overhead parameters to Percent.t

- Remove modules `Core.Std.Sexp.Sexp_{option,list,array,opaque}`, which
  used to allow binability for types `sexp_option`, `sexp_list`, etc.,
  but now serve no purpose.

- Change the signature of the output of
  `Comparable.Make{,_binable}_using_comparator` to not include `comparator_witness`.

  This is so that code like this will compile:

    include T
    include Comparable.Make_using_comparator (T)

- Changed `Timing_wheel_ns` so that it only supports times at or after
  the epoch, i.e. only non-negative `Time_ns.t` values.  Times before
  the epoch aren't needed, and supporting negative times unnecessarily
  complicates the implementation.

  Removed fields from `Timing_wheel_ns.t` that are now constants:

    ; min_time         : Time_ns.t
    ; max_time         : Time_ns.t
    ; min_interval_num : Interval_num.t

- In `Timing_wheel.t`, cache `alarm_upper_bound`, which allows us to
  give an improved error message when `Timing_wheel.add` is called with
  a time beyond `alarm_upper_bound`.

- Add `Sequence.merge_with_duplicates`

    (** `merge_with_duplicates_element t1 t2 ~cmp` interleaves the elements of `t1` and `t2`.
        Where the two next available elements of `t1` and `t2` are unequal according to `cmp`,
        the smaller is produced first. *)
    val merge_with_duplicates
      :  'a t
      -> 'a t
      -> cmp:('a -> 'a -> int)

    module Merge_with_duplicates_element : sig
      type 'a t =
        | Left of 'a
        | Right of 'a
        | Both of 'a * 'a
      `@@deriving bin_io, compare, sexp`
    end

- Add `Set.merge_to_sequence`

    (** Produces the elements of the two sets between `greater_or_equal_to` and
        `less_or_equal_to` in `order`, noting whether each element appears in the left set,
        the right set, or both.  In the both case, both elements are returned, in case the
        caller can distinguish between elements that are equal to the sets' comparator.  Runs
        in O(length t + length t'). *)

    val merge_to_sequence
      :  ?order               : ` `Increasing (** default *) | `Decreasing `
      -> ?greater_or_equal_to : 'a
      -> ?less_or_equal_to    : 'a
      -> ('a, 'cmp) t
      -> ('a, 'cmp) t
      -> 'a Merge_to_sequence_element.t Sequence.t

    module Merge_to_sequence_element : sig
      type 'a t = 'a Sequence.Merge_with_duplicates_element.t =
        | Left of 'a
        | Right of 'a
        | Both of 'a * 'a
      `@@deriving bin_io, compare, sexp`
    end

- Make `Hashtbl.merge_into` take explicit variant type

    type 'a merge_into_action = Remove | Set_to of 'a

    val merge_into
    :  f:(key:'k key -> 'a1 -> 'a2 option -> 'a2 merge_into_action)
    -> src:('k, 'a1) t
    -> dst:('k, 'a2) t
    -> unit

  The `f` used to return 'a2 option, and it was unclear whether None
  meant do nothing or remove.  (It meant do nothing.)

- Some red-black tree code showed a 15-20% speedup by inlining the
  comparison, rather than relying on the `caml_int_compare` external
  call. I've tried to cleanly apply it to `Core_int` (though it can't
  really be done without an Obj.magic), though this may be a better fit
  for a compiler patch to treat int comparisons as an intrinsic.

  Testing
  -------
  Added an inline test for boundary cases. Presently it returns the
  identical values to `caml_int_compare`, though probably it should only
  be held to same sign results.

- Rename `Hashtbl.[filter_]replace_all[i]` to `Hashtbl.[filter_]map[i]_inplace`.

- Import `debug.ml` from incremental.

- `Float_intf` used 'float' sometimes where it means 't'.

- Added `Identifiable.Make_with_comparator`, for situations where you want
  to preserve the already known comparator, for example to define stable
  sets and maps that are type equivalent to unstable types and sets.

## 113.24.00

- Add `Container.Make0` for monomorphic container types.

- Improved the performance of the implementation of `Bounded_int_table.find`.

- Switched to ppx

- Remove references to `Core_list` from `Sequence`.

- Added functions to `Bigstring` and `Iobuf` for reading unsigned 64-bit
  integers.

- Move `Comparable.bound` to `Maybe_bound.t`.  The purpose is to break up
  dependencies between the two.

- `Doubly_linked` allocated during iteration. This became a large source of
  allocation for simple benchmarks like TCP pingpong (`async/bench/pingpong`).
  Some unnecessary allocations have been removed.

- Added `Timing_wheel.next_alarm_fires_at_exn`, which is useful to avoid
  allocation when you know the timing wheel isn't empty.

- Make versions of `Binary_searchable.Make*` that don't require a `For_test` argument.
  This allows `Binary_searchable.Make` to be used for types that don't easily convert from arrays.

- Add `Quickcheckable` interface to Core and move generators/observers into type modules.

  Renames core\_list.ml to core\_list0.ml, then adds a new core\_list.ml with quickcheck
  generators and observers.  This allows quickcheck.ml to use core\_list0.ml without a
  dependency cycle.

  The feature also moves the contents of quickcheck.mli into quickcheck\_intf.ml.

- Made `Core.Unpack_buffer.Unpack_one.t` be a unary type rather than a
  binary one, by hiding its `partial_unpack` type under an existential.

  This makes it possible to make `Unpack_one` into a monad because we
  can combine two `Unpack_one.t`'s with different `partial_unpack` types
  into a new `Unpack_one.t` with a different `partial_unpack` type.

- https://github.com/janestreet/core\_kernel/pull/20
  Core.Std module is not found when compiling lib\_test/pool\_caml\_modify\_check.ml.

- Added an optional argument `?key_order` for specifying the order of
  Map.to\_alist output: either `Increasing or `Decreasing.

  The default key order is no longer left unspecified: we're now
  committed to the \`Increasing, which was the old behavior.

- Add Sexpable.Of\_sexpable2 functor, for symmetry with Binable.Of\_binable2.
  Add sexpable.mli

- Added a function for sequencing computations stored in a total map:
  `Total_map.sequence`.

- Added `Core.Bus`, a publisher/subscriber system within the memory
  space of the program.  This is a synchronous version of `Async.Bus`.

- Added `Core_map.fold2` (fold based on the contents of two maps side-by-side).

- `Core.Interfaces` defines the `Unit` module type to be `sig end`.
  Increase uniformity with its other definitions by defining it to be
  `Unit.S` instead.

- Adapt `Core_random.int` to accept larger values than `1 lsl 30`.

- Mark the `Sexpable.Of_*` and `Binable.Of_*` functors as stable.

- In `Core_char.int_is_ok`, used by `of_int` and `of_int_exn`, use int compare
  instead of polymorphic compare.

- Fix a few files where toplevel side effects might not be running
  when we don't pack libraries anymore and use -no-alias-deps.

- In `Char.For_quickcheck`, memoize construction of the filtered chars generators, since if
  they are used once, they are likely to be used many times, and the construction is costly
  compared to generating a single char.

- Extend `Core_map` to implement quickcheckable
  Extend `Core_set` to implement quickcheckable

- In `Avltree.add`, replace `?(replace = true)` with `~replace`.  This both makes the
  behavior more explicit, and saves some allocation occasionally.

- Reimplement `Avltree.iter` directly, rather than using `fold`, which requires allocating
  a closure.  This winds up being expensive during `Hashtbl.iter`.

- Add a function in Blang to deal with boolean expressions E representing the
  membership of elements in a set, given a universe U and a function projecting
  each atoms of E to a subset of U.

  Example:
  --------

  {`
  Blang.eval_set ~universe:js_tech resolve_named_set
    ("(or (and has-blue-eyes has-brown-hair) (and has-brown-eyes has-blue-hair))"
    |> Sexp.of_string
    |> t_of_sexp)
  `}


- Expose more functions in univ_map interface

- Made `Random.self_init` by default raise if used in inline tests.
  One can opt out by passing `~allow_in_tests:true`.

- In core\_hashtbl.ml, `maybe_resize_table` allocates the same closure in each iteration of a
  for loop.  Allocate it just once.

- `Hashtbl.remove_one` and `Hashtbl.remove_multi` are the same function, written twice.
  Remove `remove_one` and replace uses with `remove_multi`.

- `Bigstring.unsafe_{get,set}-{,u}int8` used the generic bigarray
  access function without a type annotation. As a result the compiler
  generated a call to the generic C function.

  Fixed this by adding type annotations.

- Add new functions to map that add common missing functionality and/or that makes the interface more uniform and consistent with other container modules.

- Made `Unpack_buffer.Unpack_one` monadic so that users can easily
  compose file parsers

  Added a couple simple parsers as examples and for testing.

- Avoid use of polymorphic compare in Quickcheck.
  Make `Quickcheck.Generator.bind_choice` lazy: do not eagerly descend into all branches.

  Reduces memory overhead by setting a threshold on the probability of choices that are
  remembered and discarded by `Quickcheck.iter` and friends.

  Motivation: Currently, `Quickcheck.iter` and related functions guarantee never to repeat
  a choice from a generator.  This winds up recording every choice ever made, which for a
  lot of generators is a prohibitive cost in space, and most of the recorded values are very
  unlikely to be repeated anyway.

  Implementation: This feature sets a probability threshold below which choices will not be
  remembered.  Choosing a fairly low, but still non-zero, threshold means values are still
  very unlikely to be repeated, but memory usage stays low.

  As of this version, the benefits of "forgetting" unlikely-to-repeat values:

  ┌──────────────────────────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
  │ Name                                     │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
  ├──────────────────────────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
  │ `quickcheck.ml:Quickcheck.iter` remember │  20.26ms │ 16.33Mw │ 100.85kw │ 100.85kw │    100.00% │
  │ `quickcheck.ml:Quickcheck.iter` forget   │  17.65ms │ 16.21Mw │  34.83kw │  34.83kw │     87.10% │
  └──────────────────────────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘

- Optimizations to:
   + various `Float.t` validation functions
   + various functions in `Validate`
   + `List.fold_right`


- Made the type of `Option.compare` compatible with `@@deriving compare`.

- Fixed an example code fragment in a comment in applicative_intf.ml

- In `Core_hashtbl`, the `add_worker` function used a `bool ref` both internally and to pass
  to `Avltree` to track whether a new key is added.  This was allocated on every
  call to `add` or `set`, and `set` didn't even use its contents.

  This version pre-allocates the `bool ref` inside each `Core_hashtbl.t` and reuses it. It
  still can't be a `mutable` field because it does need to be passed to `Avltree`.

  After change:

    ┌───────────────────────────────────────────────────┬──────────────┬────────────┬────────────┬────────────┬────────────┐
    │ Name                                              │     Time/Run │    mWd/Run │   mjWd/Run │   Prom/Run │ Percentage │
    ├───────────────────────────────────────────────────┼──────────────┼────────────┼────────────┼────────────┼────────────┤
    │        Hashtbl.set `no collisions`                │      84.73ns │      3.00w │      0.83w │      0.83w │      0.01% │
    │        Hashtbl.set `w/ collisions`                │     112.46ns │            │            │            │      0.02% │
    │        Hashtbl.change `no collisions`             │      82.74ns │      3.50w │      0.53w │      0.53w │      0.01% │
    │        Hashtbl.change `w/ collisions`             │     191.50ns │      4.56w │      1.15w │      1.15w │      0.03% │
    │        Hashtbl.merge `no collisions`              │ 292_976.43ns │ 26_669.00w │ 15_381.62w │ 12_305.62w │     48.52% │
    │        Hashtbl.merge `w/ collisions`              │ 603_822.86ns │ 33_001.00w │ 20_037.22w │ 16_961.22w │    100.00% │
    │        Hashtbl.add_exn `no resize, no collisions` │  80_992.57ns │  3_088.00w │  4_102.63w │  3_077.63w │     13.41% │
    │        Hashtbl.add_exn `no resize, w/ collisions` │ 178_080.05ns │  4_621.00w │  5_668.61w │  4_643.61w │     29.49% │
    │        Hashtbl.add_exn `w/ resize, no collisions` │ 176_442.98ns │ 16_403.00w │  9_222.64w │  6_148.64w │     29.22% │
    │        Hashtbl.add_exn `w/ resize, w/ collisions` │ 297_577.29ns │ 19_472.00w │ 12_292.13w │  9_218.13w │     49.28% │
    └───────────────────────────────────────────────────┴──────────────┴────────────┴────────────┴────────────┴────────────┘

  Before change:

    ┌───────────────────────────────────────────────────┬──────────────┬────────────┬────────────┬────────────┬────────────┐
    │ Name                                              │     Time/Run │    mWd/Run │   mjWd/Run │   Prom/Run │ Percentage │
    ├───────────────────────────────────────────────────┼──────────────┼────────────┼────────────┼────────────┼────────────┤
    │        Hashtbl.set `no collisions`                │     104.88ns │      5.00w │      1.26w │      1.26w │      0.02% │
    │        Hashtbl.set `w/ collisions`                │     114.33ns │      2.00w │            │            │      0.02% │
    │        Hashtbl.change `no collisions`             │      85.79ns │      4.50w │      0.58w │      0.58w │      0.02% │
    │        Hashtbl.change `w/ collisions`             │     198.75ns │      5.56w │      1.28w │      1.28w │      0.04% │
    │        Hashtbl.merge `no collisions`              │ 307_857.59ns │ 31_787.00w │ 15_380.91w │ 12_304.91w │     58.19% │
    │        Hashtbl.merge `w/ collisions`              │ 529_054.02ns │ 38_119.00w │ 20_015.32w │ 16_939.32w │    100.00% │
    │        Hashtbl.add_exn `no resize, no collisions` │  77_708.20ns │  5_135.00w │  4_101.83w │  3_076.83w │     14.69% │
    │        Hashtbl.add_exn `no resize, w/ collisions` │ 180_950.23ns │  6_668.00w │  5_638.77w │  4_613.77w │     34.20% │
    │        Hashtbl.add_exn `w/ resize, no collisions` │ 177_492.82ns │ 19_476.00w │  9_237.07w │  6_163.07w │     33.55% │
    │        Hashtbl.add_exn `w/ resize, w/ collisions` │ 285_298.72ns │ 22_545.00w │ 12_330.90w │  9_256.90w │     53.93% │
    └───────────────────────────────────────────────────┴──────────────┴────────────┴────────────┴────────────┴────────────┘


- In `Core_hashtbl.add_worker`, removed a `match` that avoided calling `Avltree.add`, but
  actually did hurt performance overall.

  Perhaps at some point before cross-module inlining, this was a helpful optimization.
  Right now it bypasses the mutation inside `Avltree`, so replacing a value in a
  non-colliding bucket (a `Leaf`) causes unnecessary re-allocation of the leaf.

  After changes:

    ┌───────────────────────────────────────────────────┬──────────────┬────────────┬────────────┬────────────┬────────────┐
    │ Name                                              │     Time/Run │    mWd/Run │   mjWd/Run │   Prom/Run │ Percentage │
    ├───────────────────────────────────────────────────┼──────────────┼────────────┼────────────┼────────────┼────────────┤
    │        Hashtbl.set `no collisions`                │      52.19ns │      2.00w │            │            │            │
    │        Hashtbl.set `w/ collisions`                │     112.04ns │      2.00w │            │            │      0.02% │
    │        Hashtbl.change `no collisions`             │      87.25ns │      4.50w │      0.58w │      0.58w │      0.02% │
    │        Hashtbl.change `w/ collisions`             │     195.85ns │      5.56w │      1.29w │      1.29w │      0.04% │
    │        Hashtbl.merge `no collisions`              │ 308_164.10ns │ 31_787.00w │ 15_380.91w │ 12_304.91w │     58.48% │
    │        Hashtbl.merge `w/ collisions`              │ 526_914.80ns │ 38_119.00w │ 20_013.81w │ 16_937.81w │    100.00% │
    │        Hashtbl.add_exn `no resize, no collisions` │  76_983.60ns │  5_135.00w │  4_100.44w │  3_075.44w │     14.61% │
    │        Hashtbl.add_exn `no resize, w/ collisions` │ 174_712.92ns │  6_668.00w │  5_667.47w │  4_642.47w │     33.16% │
    │        Hashtbl.add_exn `w/ resize, no collisions` │ 176_681.57ns │ 19_476.00w │  9_231.75w │  6_157.75w │     33.53% │
    │        Hashtbl.add_exn `w/ resize, w/ collisions` │ 280_448.62ns │ 22_545.00w │ 12_293.32w │  9_219.32w │     53.22% │
    └───────────────────────────────────────────────────┴──────────────┴────────────┴────────────┴────────────┴────────────┘

  Before changes:

    ┌───────────────────────────────────────────────────┬──────────────┬────────────┬────────────┬────────────┬────────────┐
    │ Name                                              │     Time/Run │    mWd/Run │   mjWd/Run │   Prom/Run │ Percentage │
    ├───────────────────────────────────────────────────┼──────────────┼────────────┼────────────┼────────────┼────────────┤
    │        Hashtbl.set `no collisions`                │     104.88ns │      5.00w │      1.26w │      1.26w │      0.02% │
    │        Hashtbl.set `w/ collisions`                │     114.33ns │      2.00w │            │            │      0.02% │
    │        Hashtbl.change `no collisions`             │      85.79ns │      4.50w │      0.58w │      0.58w │      0.02% │
    │        Hashtbl.change `w/ collisions`             │     198.75ns │      5.56w │      1.28w │      1.28w │      0.04% │
    │        Hashtbl.merge `no collisions`              │ 307_857.59ns │ 31_787.00w │ 15_380.91w │ 12_304.91w │     58.19% │
    │        Hashtbl.merge `w/ collisions`              │ 529_054.02ns │ 38_119.00w │ 20_015.32w │ 16_939.32w │    100.00% │
    │        Hashtbl.add_exn `no resize, no collisions` │  77_708.20ns │  5_135.00w │  4_101.83w │  3_076.83w │     14.69% │
    │        Hashtbl.add_exn `no resize, w/ collisions` │ 180_950.23ns │  6_668.00w │  5_638.77w │  4_613.77w │     34.20% │
    │        Hashtbl.add_exn `w/ resize, no collisions` │ 177_492.82ns │ 19_476.00w │  9_237.07w │  6_163.07w │     33.55% │
    │        Hashtbl.add_exn `w/ resize, w/ collisions` │ 285_298.72ns │ 22_545.00w │ 12_330.90w │  9_256.90w │     53.93% │
    └───────────────────────────────────────────────────┴──────────────┴────────────┴────────────┴────────────┴────────────┘

- Add new functions to hashtbl that add common missing functionality and/or that makes the interface more uniform and consistent with other container modules.

- Add a bunch of functions to list and array that add common missing functionality and/or that make their interfaces more uniform and consistent with other container modules.

- Rewrite `Hashtbl.merge` to be simpler and faster.

  After changes:

    ┌──────────────────────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
    │ Name                                 │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
    ├──────────────────────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
    │        Hashtbl.merge `no collisions` │ 172.57us │ 17.44kw │   9.22kw │   7.69kw │     48.76% │
    │        Hashtbl.merge `w/ collisions` │ 284.55us │ 20.61kw │  11.53kw │   9.99kw │     80.41% │
    │ Pooled_hashtbl.merge `no collisions` │ 260.57us │  5.20kw │  19.18kw │   3.09kw │     73.63% │
    │ Pooled_hashtbl.merge `w/ collisions` │ 353.88us │  5.20kw │  19.18kw │   3.09kw │    100.00% │
    └──────────────────────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘

  Before changes:

    ┌──────────────────────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
    │ Name                                 │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
    ├──────────────────────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
    │        Hashtbl.merge `no collisions` │ 309.59us │ 31.79kw │  15.38kw │  12.30kw │     48.91% │
    │        Hashtbl.merge `w/ collisions` │ 526.67us │ 38.12kw │  19.97kw │  16.90kw │     83.21% │
    │ Pooled_hashtbl.merge `no collisions` │ 469.41us │  7.32kw │  35.29kw │   3.12kw │     74.16% │
    │ Pooled_hashtbl.merge `w/ collisions` │ 632.96us │  7.32kw │  35.29kw │   3.12kw │    100.00% │
    └──────────────────────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘

- Make `Hashtbl` functions raise an exception if a callback passed in as an argument mutates
  one of the hash tables being worked on.

  Usually, though not always, this comes up for iteration functions.  Once a hash table has
  been mutated, it is unsafe to continue operating on it, as its structure may have changed.
  Buckets and their contents may have been moved or resized; continuing may result in
  skipping key/value pairs, repeating key/value pairs, or executing unsafe operations.

  This feature adds a `mutation_allowed` flag to hash tables.  Each mutating operation first
  checks the flag, and raises if it is not set.  Each operation with callbacks that must not
  mutate unsets the flag before calling the callbacks, and restores the flag's original
  value when it finishes.

  We compared the timing of this implementation to an alternate implementation using a
  mutation counter, and the time and space used for this implementation was much better for
  iteration and within epsilon of the other for single-key operations like `set`.


- Array function names related to zipping are all over the place. Make
  them match List, which has a nice uniform naming scheme.

  * Rename `combine` -> `zip_exn`
  * Rename `split` -> `unzip`
  * (`zip` remains named as `zip`)

- Add `~key` and `~data` labels to `Hashtbl.filteri_inplace`

- Added `Hash_set.to_hashtbl`, by analogy to `Set.to_map`.

- Since we are mutating avltrees in place, make sure the compiler sees
  the type parameters as invariant.

  Tested that a segfaulting example doesn't compile anymore.


- Add label `f` to Hashtbl.change, Map.change, & family.

  Introduce the new function `update` in those modules, which enforces
  statically the presence of a resulting value

  Example:

  -|val Hashtbl.change : 'a t -> key -> ('a option -> 'a option) -> unit

  +|val Hashtbl.change : 'a t -> key -> f:('a option -> 'a option) -> unit
  +|val Hashtbl.update : 'a t -> key -> f:('a option -> 'a) -> unit

  The motivation for the introduction of `update` is that in an overwhelming
  majority of the places where `Hashtbl.change` is used in our codebase, it is
  statically known that a new value shall be computed and stored.  The use of the
  dynamism offered by `change`, which can return an option, is error prone.

  The addition of the label is considered acceptable in consideration to external
  libraries depending on core, because a missing label is just a warning, and we
  do not guarantee stability in the presence of -warn-error = true.

- Changed `Source_code_position.t` from:

    `@@deriving bin_io, sexp`

  to:

    `@@deriving sexp_of`

  and made `sexp_of` use the human-readable format, `"FILE:LINE:COL"`,
  rather than the unreadable format.  Removed
  `Source_code_position.t_hum`, which is now obsolete.

  If one wants a serialized source-code position, one can use
  `Source_code_position.Stable`.

- Added `Ref.set_temporarily`, for temporarily setting a ref to a value for
  the duration of a thunk.

    val set_temporarily : 'a t -> 'a -> f:(unit -> 'b) -> 'b

- Add the function `singleton : 'a -> 'a t` in the stack containers.  It cannot be
  added to `Container.S` directly because some container never have exactly 1
  element.

- Made `Core.Array` match `Invariant.S1`.

- Change the interface of `Make_iterable_binable*` to give the control
  back to the user when deserializing Bin\_protted data.

  Improve the bin\_prot deserialization of `Map`s and `Set`s.
  We construct a balanced tree directly instead of relying on `Map.add` / `Set.add`.
  This is possibile because the size of the map is known and elements are sorted.

  The complexity goes down from n.log(n) to n.

  In case the comparison function changes (and the invariant is not respected),
  there is a fallback to reconstruct the whole map from scratch.

- Add a function to blit a `Rope.t` into a `Buffer.t`.

- Hashtbl differs from some other core containers with idiosyncratic naming of iteration functions. Change to be consistent and to more closely match the conventions for List and Array.

  Hashtbl:
  * Copy `iter` -> `iteri`.
  * Add a deprecation tag to `iter`.


- Made `Bag.invariant` and `Doubly_linked.invariant` match `Invariant.S1`.

- Map differs from some other core containers with idiosyncratic naming of iteration functions. The current Map name conventions are also internally inconsistent as well (ex: current `Map.iter` vs `Map.map` vs `Map.mapi`). Change to be consistent and to more closely match the conventions for List and Array.

  Map:
  * Copy `filter` -> `filteri`.
  * Add a deprecation tag to `filter`.

- Map differs from some other core containers with idiosyncratic naming
  of iteration functions. The current Map name conventions are also
  internally inconsistent as well (ex: current `Map.iter` vs `Map.map`
  vs `Map.mapi`). Change to be consistent and to more closely match the
  conventions for List and Array.

  Map:
  * Copy `iter` -> `iteri`.
  * Add a deprecation tag to `iter`.

- Made `Core.Set_once` match `Invariant.S1`.

- Add `Bigstring.concat`.

- For `Core.Unique_id`, exposed `@@deriving typerep`.

- Expose Hashtbl.hashable, analogous to Map.comparator.

- Adds a constant-time `val mem_elt : 'a t -> 'a Elt.t -> bool` to Doubly_linked and Bag

- Add `Ordering.to_int` which can be useful when one is writing a comparison
  function.  Instead of dealing with the int directly, one can return Ordering.t
  values and transform them later into ints.

- `Float.int_pow`: Fast computation of `x ** n` when n is an integer.

- Make `Core_kernel.Std.Nothing.t` enumerable.  There's no particular reason
  not to.

- Minor improvements to queue interface

- Call `Caml.Pervasives.do_at_exit` before printing an exception and exiting

  The default ocaml uncaught exception handler does this. It is
  especially useful for curses applications as the `at_exit` handler has
  a chance to put back the terminal in a good state before printing the
  exception and backtrace.

  Do the same in Core and Async.

- Removed big literals so that the compiler does not complain in 32bit

- Add `List.range'`, a generalization of `List.range`.

- Add some functions to `Map` that are present in `Hashtbl`:
  - `remove_multi`
  - `partition_tf`
  - `partitioni_tf`
  - `partition_map`
  - `partition_mapi`

- Add a `Map.nth_exn` as a missing complementary function to nth

- Renamed `Validate.fail_sexp` as `fail_s`, to follow our new `*_s`
  convention for `Sexp.t`-taking functions.

- `Sequence.split_n_eagerly` returns a pair of sequences, but every element of the
  first sequence has already been evaluated by the time it returns. This feature
  just makes the first component of the tuple a list instead of a sequence, and
  renames `split_n_eagerly` to `split_n`.

  Additionally, this feature adds a new `chunks_exn` function, which just applies
  `split_n` until the input sequence is empty.

- Removed `Timing_wheel`'s default `alarm_precision`, to force people to
  think about the precision they want when they create a timing wheel.

- In `Timing_wheel.Config.sexp_of_t`, used `@sexp_drop_default` with
  `level_bits`.

- Write a better-performing `Array.filter_mapi` function, and implement
  `Array.filter_map`, `Array.filter_opt`, `Array.partitioni_tf`, and
  `Array.partition_tf` in terms of it.

  Slightly worse for zero-length input arrays, about unch'd if we're filtering out
  almost everything (`eq_zero`), better on most everything else.

  ┌────────────────────────────────────────────────────┬─────────────────┬─────────────┬─────────────┬─────────────┬────────────┐
  │ Name                                               │        Time/Run │     mWd/Run │    mjWd/Run │    Prom/Run │ Percentage │
  ├────────────────────────────────────────────────────┼─────────────────┼─────────────┼─────────────┼─────────────┼────────────┤
  │ `core\_array.ml:filter` old-filter-even:0           │         12.37ns │       9.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-even:1           │         77.44ns │      15.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-even:10          │        207.10ns │      36.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-even:100         │      1\_699.41ns │     261.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-even:1000        │     56\_320.50ns │   1\_009.00w │   2\_506.01w │   1\_004.01w │      0.30% │
  │ `core\_array.ml:filter` old-filter-even:10000       │    469\_134.89ns │  10\_009.00w │  25\_007.38w │  10\_005.38w │      2.46% │
  │ `core\_array.ml:filter` old-filter-even:100000      │  4\_421\_742.22ns │ 100\_009.00w │ 250\_130.09w │ 100\_128.09w │     23.17% │
  │ `core\_array.ml:filter` new-filter-even:0           │         13.87ns │      14.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-even:1           │         57.64ns │      18.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-even:10          │        196.28ns │      35.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-even:100         │      1\_361.04ns │     215.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-even:1000        │     21\_473.76ns │   1\_014.00w │   1\_001.02w │             │      0.11% │
  │ `core\_array.ml:filter` new-filter-even:10000       │    204\_033.12ns │  10\_014.00w │  10\_001.14w │       0.14w │      1.07% │
  │ `core\_array.ml:filter` new-filter-even:100000      │  2\_058\_144.47ns │ 100\_014.00w │ 100\_002.00w │       1.00w │     10.78% │
  │ `core\_array.ml:filter` old-filter-eq\_zero:0        │         12.21ns │       9.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-eq\_zero:1        │         71.23ns │      15.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-eq\_zero:10       │        174.80ns │      24.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-eq\_zero:100      │      1\_212.70ns │     114.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-eq\_zero:1000     │     23\_347.51ns │      13.00w │   1\_007.00w │       6.00w │      0.12% │
  │ `core\_array.ml:filter` old-filter-eq\_zero:10000    │    210\_509.83ns │      13.00w │  10\_007.00w │       6.00w │      1.10% │
  │ `core\_array.ml:filter` old-filter-eq\_zero:100000   │  1\_912\_253.91ns │      13.00w │ 100\_007.01w │       6.01w │     10.02% │
  │ `core\_array.ml:filter` new-filter-eq\_zero:0        │         13.70ns │      14.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-eq\_zero:1        │         56.56ns │      18.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-eq\_zero:10       │        179.42ns │      27.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-eq\_zero:100      │      1\_254.49ns │     117.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-eq\_zero:1000     │     20\_968.06ns │      16.00w │   1\_001.02w │             │      0.11% │
  │ `core\_array.ml:filter` new-filter-eq\_zero:10000    │    204\_299.82ns │      16.00w │  10\_001.13w │       0.13w │      1.07% │
  │ `core\_array.ml:filter` new-filter-eq\_zero:100000   │  2\_019\_283.81ns │      16.00w │ 100\_001.91w │       0.91w │     10.58% │
  │ `core\_array.ml:filter` old-filter-neq\_zero:0       │         12.14ns │       9.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-neq\_zero:1       │         32.72ns │      11.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-neq\_zero:10      │        219.18ns │      48.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter-neq\_zero:100     │      1\_902.76ns │     408.00w │       0.12w │       0.12w │            │
  │ `core\_array.ml:filter` old-filter-neq\_zero:1000    │     82\_032.44ns │   2\_007.00w │   3\_998.20w │   1\_997.20w │      0.43% │
  │ `core\_array.ml:filter` old-filter-neq\_zero:10000   │    850\_234.44ns │  20\_007.00w │  40\_014.86w │  20\_013.86w │      4.46% │
  │ `core\_array.ml:filter` old-filter-neq\_zero:100000  │  7\_345\_941.05ns │ 200\_007.00w │ 400\_407.82w │ 200\_406.82w │     38.49% │
  │ `core\_array.ml:filter` new-filter-neq\_zero:0       │         13.66ns │      14.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-neq\_zero:1       │         18.26ns │      14.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-neq\_zero:10      │        201.04ns │      43.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-neq\_zero:100     │      1\_404.33ns │     313.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter-neq\_zero:1000    │     22\_829.70ns │   2\_012.00w │   1\_001.02w │             │      0.12% │
  │ `core\_array.ml:filter` new-filter-neq\_zero:10000   │    218\_872.52ns │  20\_012.00w │  10\_001.21w │       0.21w │      1.15% │
  │ `core\_array.ml:filter` new-filter-neq\_zero:100000  │  2\_121\_340.68ns │ 200\_012.00w │ 100\_002.77w │       1.77w │     11.12% │
  │ `core\_array.ml:filter` old-filter\_map-int:0        │          9.58ns │       5.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-int:1        │         68.46ns │      11.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-int:10       │        191.66ns │      32.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-int:100      │      1\_492.60ns │     257.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-int:1000     │     57\_155.42ns │   1\_005.00w │   2\_507.01w │   1\_005.01w │      0.30% │
  │ `core\_array.ml:filter` old-filter\_map-int:10000    │    522\_177.50ns │  10\_005.00w │  25\_008.54w │  10\_006.54w │      2.74% │
  │ `core\_array.ml:filter` old-filter\_map-int:100000   │  5\_945\_405.67ns │ 100\_005.00w │ 250\_170.69w │ 100\_168.69w │     31.15% │
  │ `core\_array.ml:filter` new-filter\_map-int:0        │         12.03ns │      10.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-int:1        │         53.63ns │      14.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-int:10       │        164.16ns │      31.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-int:100      │      1\_263.42ns │     211.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-int:1000     │     23\_113.12ns │   1\_010.00w │   1\_001.02w │             │      0.12% │
  │ `core\_array.ml:filter` new-filter\_map-int:10000    │    218\_152.23ns │  10\_010.00w │  10\_001.15w │       0.15w │      1.14% │
  │ `core\_array.ml:filter` new-filter\_map-int:100000   │  2\_217\_307.86ns │ 100\_010.00w │ 100\_002.11w │       1.11w │     11.62% │
  │ `core\_array.ml:filter` old-filter\_map-float:0      │          9.32ns │       5.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-float:1      │         66.68ns │      13.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-float:10     │        182.86ns │      42.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-float:100    │      1\_496.56ns │     357.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-float:1000   │     76\_479.74ns │   2\_005.00w │   3\_507.02w │   2\_005.02w │      0.40% │
  │ `core\_array.ml:filter` old-filter\_map-float:10000  │    694\_999.59ns │  20\_005.00w │  35\_011.08w │  20\_009.08w │      3.64% │
  │ `core\_array.ml:filter` old-filter\_map-float:100000 │  8\_694\_669.26ns │ 200\_005.00w │ 350\_476.44w │ 200\_474.44w │     45.56% │
  │ `core\_array.ml:filter` new-filter\_map-float:0      │         12.29ns │      10.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-float:1      │         58.24ns │      16.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-float:10     │        142.67ns │      41.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-float:100    │      1\_119.41ns │     311.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-float:1000   │     14\_262.66ns │   2\_010.00w │   1\_001.02w │             │      0.07% │
  │ `core\_array.ml:filter` new-filter\_map-float:10000  │    136\_448.05ns │  20\_010.00w │  10\_001.23w │       0.23w │      0.71% │
  │ `core\_array.ml:filter` new-filter\_map-float:100000 │  1\_282\_005.01ns │ 200\_010.00w │ 100\_003.14w │       2.14w │      6.72% │
  │ `core\_array.ml:filter` old-filter\_map-boxed:0      │          9.48ns │       5.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-boxed:1      │         71.16ns │      13.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-boxed:10     │        197.40ns │      42.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-boxed:100    │      1\_762.40ns │     357.00w │             │             │            │
  │ `core\_array.ml:filter` old-filter\_map-boxed:1000   │     86\_220.67ns │   2\_005.00w │   3\_507.02w │   2\_005.02w │      0.45% │
  │ `core\_array.ml:filter` old-filter\_map-boxed:10000  │    828\_291.42ns │  20\_005.00w │  35\_011.84w │  20\_009.84w │      4.34% │
  │ `core\_array.ml:filter` old-filter\_map-boxed:100000 │  7\_955\_395.61ns │ 200\_005.00w │ 350\_441.44w │ 200\_439.44w │     41.68% │
  │ `core\_array.ml:filter` new-filter\_map-boxed:0      │         14.43ns │      10.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-boxed:1      │         59.24ns │      16.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-boxed:10     │        198.19ns │      41.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-boxed:100    │      1\_580.21ns │     311.00w │             │             │            │
  │ `core\_array.ml:filter` new-filter\_map-boxed:1000   │     52\_045.31ns │   2\_010.00w │   2\_011.01w │   1\_010.01w │      0.27% │
  │ `core\_array.ml:filter` new-filter\_map-boxed:10000  │    479\_239.44ns │  20\_010.00w │  20\_012.42w │  10\_011.42w │      2.51% │
  │ `core\_array.ml:filter` new-filter\_map-boxed:100000 │  4\_389\_392.06ns │ 200\_010.00w │ 200\_135.09w │ 100\_134.09w │     23.00% │
  │ `core\_array.ml:filter` old-partition\_tf:0          │         16.55ns │      16.00w │             │             │            │
  │ `core\_array.ml:filter` old-partition\_tf:1          │        128.08ns │      29.00w │             │             │            │
  │ `core\_array.ml:filter` old-partition\_tf:10         │        554.15ns │     111.00w │             │             │            │
  │ `core\_array.ml:filter` old-partition\_tf:100        │      4\_853.58ns │     921.00w │       0.46w │       0.46w │      0.03% │
  │ `core\_array.ml:filter` old-partition\_tf:1000       │    201\_289.06ns │   5\_016.00w │   9\_015.21w │   5\_010.21w │      1.05% │
  │ `core\_array.ml:filter` old-partition\_tf:10000      │  1\_796\_749.87ns │  50\_016.00w │  90\_040.96w │  50\_035.96w │      9.41% │
  │ `core\_array.ml:filter` old-partition\_tf:100000     │ 19\_084\_871.85ns │ 500\_016.00w │ 902\_187.67w │ 502\_182.67w │    100.00% │
  │ `core\_array.ml:filter` new-partition\_tf:0          │         28.29ns │      23.00w │             │             │            │
  │ `core\_array.ml:filter` new-partition\_tf:1          │        103.78ns │      31.00w │             │             │            │
  │ `core\_array.ml:filter` new-partition\_tf:10         │        504.10ns │      96.00w │             │             │            │
  │ `core\_array.ml:filter` new-partition\_tf:100        │      3\_869.52ns │     726.00w │       0.23w │       0.23w │      0.02% │
  │ `core\_array.ml:filter` new-partition\_tf:1000       │    122\_807.29ns │   4\_023.00w │   5\_013.04w │   2\_010.04w │      0.64% │
  │ `core\_array.ml:filter` new-partition\_tf:10000      │  1\_197\_596.39ns │  40\_023.00w │  50\_020.05w │  20\_017.05w │      6.28% │
  │ `core\_array.ml:filter` new-partition\_tf:100000     │ 10\_458\_344.09ns │ 400\_023.00w │ 500\_590.94w │ 200\_587.94w │     54.80% │
  └────────────────────────────────────────────────────┴─────────────────┴─────────────┴─────────────┴─────────────┴────────────┘


- Added `Binable.Of_sexpable` functor.

- Install the sexp exception printer sooner so that we can get proper
  `%test_result ...` errors in things that come before `core_kernel`.

- In `Stable_unit_test.Make` functors, include all test failures rather
  than just the first.  This is useful for updating batches of expected
  `bin_io` results when stabilizing a module.

- Remove an unnecessary cast in or_error.ml

## 113.00.00

- Added `Float.int63_round_nearest_exn`.

    val int63_round_nearest_exn : t -> Core_int63.

- Changed `Hashtbl.sexp_of_t` so that keys are sorted in increasing order.

    This also applies to the `sexp_of_t` produced by `Hashtbl.Make` and
    `Make_binable`. Sorting by key is nice when looking at output, as well as
    in tests, so that the output is deterministic and so that diffs are
    minimized when output changes.

- Added to `Info`, `Error`, and `Or_error` a `Stable.V2` module, whose `bin_io`
  is the same as the unstable `bin_io`.

- Replaced `Map.prev_key` and `next_key` with `closest_key`.

    val closest_key
      :  ('k, 'v, 'cmp) t
      -> [ `Greater_or_equal_to
         | `Greater_than
         | `Less_or_equal_to
         | `Less_than
         ]
      -> 'k
      -> ('k * 'v) option

- Shared code between `Monad.Make{,2}` and `Applicative.Make{,2}`.

- Added tests to make sure `round_nearest` and `int63_round_nearest_exn`
  don't allocate.

- Added `Lazy.T_unforcing` module, with a custom `sexp_of_t` that doesn't
  force.

    This serializer does not support round tripping, i.e. `t_of_sexp`.  It
    is intended to be used in debug code or `<:sexp_of< >>` statements.  E.g:

      type t =
        { x : int Lazy.T_unforcing.t
        ; y : string
        }
      with sexp_of

- Extended `Map.to_sequence` and `Set.to_sequence` to take any combination of
  upper bound, lower bound, and direction.

- Added `Map.split`.

- Added `Timing_wheel.fire_past_alarms`, which fires alarms in the current time
  interval's bucket whose time is in the past.

- Added a `Total_map` module, for maps where every value of the key type is
  present in the map.

- Added `Bigstring.compare` and `Bigstring.equal`.

- Split `monad.ml` into three files: `monad.ml`, `monad.mli`, and `monad_intf.ml`.

- Removed the last remaining dependence of `Core_kernel` on Unix, moving
  `Time_ns.pause` functions to `Core`.

- Added optional arguments to `Hash_queue.create`, `?growth_allowed` and
  `size`, which then get passed to `Hashtbl.create`.

- Added a `?strict:unit` argument to functions that ordinarily create lazy
  sexps, like `failwiths`.

      Info.create
      Error.create
      Error.failwiths
      Error.failwithp
      Or_error.error

    This makes it easy to force a use to be strict, which is sometimes
    useful to accurately capture the state of a mutable data structure at
    the time the error happens, lest it change by the time the error is
    rendered.

- Removed `Interned_string` module.

- In `Pooled_hashtbl`, avoid trying to create arrays bigger than
  `Sys.max_array_length`.

    The problem affected 32-bit platforms.

- Added `Quickcheck` module.

    Supports automated testing with randomly-generated inputs in the style of
    Haskell's Quickcheck library.  Our adaptation supports flexible probability
    distributions for values of a given type and uniqueness guarantees for
    generated values.

- Made `Set.to_sequence` and `Set.split` have the same interface as
  `Map.to_sequence` and `Map.split`, respectively.

- Fixed `Float` and `Timing_wheel` to compile on 32-bit platforms.

- Added `Lazy.Stable.V1`.

- Added `List.reduce_balanced`, which is like `reduce`, but relies on
  associativity of `f` to make nesting of calls to `f` logarithmic rather than
  linear in the input list length.

- Added `String_id.Make_without_pretty_printer`.

- Restricted `Time_ns.Span` values to be less than 135 years, which ensures the
  corresponding `float` `Time.Span` values have microsecond precision.

    Fixed a `Time_ns` test that recently started failing due to crossing
    the 135-year boundary.

    Reducing the range of `Time_ns.Span` required adjusting the implementation
    of `Core.Time_ns.Option.Stable.V1`, which (accidentally, incorrectly)
    incorporated the (unstabilized) `Core_kernel.Time_ns.Span.min_value` as the
    representation of `bid_none` and `.max_value` as `ask_none`.  The prior
    representation is preserved, but some previously allowed values are no
    longer allowed and now raise exceptions!

- Added `Rope` module, the standard data structure for efficient string
  manipulation.

- Added `Sequence.unfold_with_and_finish`, a variant of `unfold_with` that can
  continue the sequence after the inner sequence finishes.

- Replaced `Sequence.cycle` with `Sequence.cycle_list_exn`, to work around a
  bug in `Sequence.cycle` raising on the empty sequence.

    Sequence.cycle can cause an infinite loop if its input is empty. It is
    problematic to check whether the input sequence is empty.

      * If we check it eagerly, we have to turn `cycle` into
        `cycle_eagerly_exn`, and it will evaluate the first element twice.

      * If we check it lazily, we might raise an exception in a seemingly
        unrelated part of the code, and the usually-good habit of wrapping a
        function like `cycle_exn` in `try .. with ..`  would not catch it.

    To get around these issues, [cycle] is changed to accept only lists as
    inputs, not sequences. It is now called [cycle_list_exn].

- Fixed assumptions about the size of integers, to support compiling to
  Javascript, where integers are 32-bit.

- Fixed build on Mac OSX.

    Fix build when LINUX_EXT or TIMERFD are undefined.

- Added `Caml.Bytes`.

    Add an alias for Bytes in Caml. Fixes janestreet/core_kernel#46.

- In `Container`, exposed polymorphic functions individually building container functions using `fold` or `iter`.

    Exposed polymorphic functions in `Core_kernel.Container` for
    individually building each of the `Container` functions using `fold`
    or `iter`.  E.g.:

      type ('t, 'elt, 'accum) fold =
        't -> init:'accum -> f:('accum -> 'elt -> 'accum) -> 'accum

      type ('t, 'elt) iter = 't -> f:('elt -> unit) -> unit

      val length : fold:('t,  _, int ) fold -> 't -> int
      val exists : iter:('t, 'a) iter -> 't -> f:('a -> bool) -> bool

- Added container.mli, which was sorely missing.

- Added `Doubly_linked.to_sequence`.

- Added `Hash_queue.sexp_of_t`.

## 112.35.00

- Added an Applicative interface to Core
  (a.k.a. idioms or applicative functors)
- Generalized the signature of `Hashtbl.merge_into` to allow the types
  of `src` and `dst` to be different.
- Made `Day_of_week.of_string` accept additional formats (integers 0-6,
  full day names).
- Added `Day_of_week.to_string_long`, which produces the full day name.
- Changed `Hashtbl.add_exn` to not create a new exception constructor
  when it raises due to a duplicate key.
- Added `Map.nth`, which returns the nth element of a map, ordered by
  key rank.
- Added `Binable.Of_binable` functors, similar to `Sexpable.Of_sexpable`

    One should use `Binable.Of_binable` rather than the functionally
    equivalent `Bin_prot.Utils.Make_binable`.

- Added `Either` module, with
  `type ('a, 'b) t = First  of 'a | Second of 'b`.
- Added to `Univ_map` a functor that creates a new `Univ_map` type in
  which the type of data is a function of the key's type, with the type
  function specified by the functor's argument.

    Normally, a `Univ_map.t` stores `('a Key.t * 'a)` pairs.  This feature
    lets it store `('a Key.t * 'a Data.t)` pairs for a given
    `('a Data.t)`.

- Made `Day_of_week.Stable` be `Comparable` and `Hashable`.
- Fixed a couple `Exn` unit tests that mistakenly relied on the global
  setting of `Printexc.get_backtrace`.

    Now the tests locally set it to what they need.

    This avoids unit-test failures when running with no
    `OCAMLRUNPARAM` set:

        File "exn.ml", line 130, characters 2-258: clear_backtrace threw "Assert_failure exn.ml:133:4".
            in TEST_MODULE at file "exn.ml", line 127, characters 0-1057

- Renamed `Monad.ignore` as `Monad.ignore_m`, while preserving
  `ignore = ignore_m` in existing modules (e.g. `Deferred`)
  that used it.

    We can later consider those modules on a case-by-case basis to see
    whether we want to remove `ignore`.

- Added `Set.symmetric_diff`.
- Added `Timing_wheel.reschedule`, which reschedules an existing alarm.
- Added `Applicative.S2`, analogous to `Monad.S2`.
- Added combinators to `Either`.
- Added `Hashtbl.add_or_error` and `create_with_key_or_error`, which use
  `Or_error` and are more idiomatic ways of signalling duplicates.
- Added `Sexpable.Of_sexpable1` functor, for one-parameter type
  constructors.
- Made `Timing_wheel_ns` keys be `Int63.t` rather than `int`, so that
  behavior is consistent on 32-bit and 64-bit machines.

  Also, made `Timing_wheel.Interval_num` an abstract type.
- Hid the `bytes` type in `Core.Std`, so that type errors refer to
  `string` rather than `bytes`.

    Added `Bytes` module so that people can say `Bytes.t` if they
    need to.

    Now we get reasonable error messages:

        String.length 13
        -->
        Error: This expression has type int but an expression was expected of type
                string

        "" + 13
        -->
        Error: This expression has type string but an expression was expected of type
                int

- Modernized the coding style in `Timing_wheel`.
- Replaced `Unpack_buffer.unpack` with `unpack_into` and `unpack_iter`,
  to avoid allocation.

    `Unpack_buffer.unpack` created a (vector-backed) `Core.Std.Queue`
    for each call.  When unpacking a buffer containing many values,
    resizing of the buffer can be costly and in some cases leads to
    promotions of short-lived data to the major heap.

    The new functions avoid allocating the queue:

        val unpack_into : ('value, _) t -> 'value Queue.t     -> unit Or_error.t
        val unpack_iter : ('value, _) t -> f:('value -> unit) -> unit Or_error.t

- Cleaned up the implementation of `Gc.tune`.
- Change `Unit` implementation to use `Identifiable.Make` instead of
  applying functors separately.
- Added `val random: unit -> int` to `Int63`.
- Reworked `Float.iround_*_exn` functions to not allocate in the common case.
- Added `Fqueue.singleton` and `Fdeque.singleton`.
- Moved `Unix.tm` and `Unix.strftime` from `Core_kernel` to `Core`.

    Added external time formatting:

        float (* seconds *)-> string (* format *) -> string = "..."

- Made `String_id.Make` call `Pretty_printer.Register`.
- Changed `String_id` to allow the pipe character in identifiers.
- Made `List.compare` have the usual type from `with compare`,
  `val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int`.

    Previously, `List.compare`'s type was:

        val compare : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> int

- Made stable `Map`'s and `Set`'s conform to the `Stable1` interface.
- Reworked `Hashtbl.find_exn` to not allocate.

    Previously, `Hashtbl.find_exn` allocated because it called
    `Hashtbl.find`, which allocates an option (partially because
    `Avltree` allocates options in its `find` function).

## 112.24.00

- Added `Time_ns` module.

  A fragment of `Core.Std.Time_ns` is now in `Core_kernel.Std.Time_ns` such that
  `Async_kernel` can use `Time_ns` and only depend on `Core_kernel`.

- Renamed `Dequeue` as `Deque`.
  `Dequeue` remains for backward compatibility, but should not be used anymore.
  Use `Deque` instead.

- Added `Fdeque` module, a functional version `Deque`.
  Deprecate deque-like functions in `Fqueue`.

## 112.17.00

- Added `List.is_prefix`.

  ```ocaml
  val List.is_prefix : 'a t -> prefix:'a t -> equal:('a -> 'a -> bool) -> bool
  ```
- Made `String_id.Make` functor generative, which exposes that the
  result has `type t = private string`.

  Previously the result of `String_id.Make` didn't expose `type t =
  private string` due to a type-checker bug:

  * http://caml.inria.fr/mantis/view.php?id=6485
  * http://caml.inria.fr/mantis/view.php?id=6011

- Used generative functors, e.g. for `Unique_id`.

  Used generative functors (new feature in 4.02) where previously we
  used dummy `M : sig end` arguments in the signature and `(struct
  end)` when applying the functor.

  Just to note the difference between applicative and generative
  functors.  Suppose we have:

  ```ocaml
  module F (M : sig end) : sig type t end
  ```

  and we apply it several times

  ```ocaml
  module A = F (struct end)
  module B = F (struct end)
  module C = F (String)
  module D = F (String)
  ```

  Then we have that `A.t <> B.t` but `C.t = D.t`.  This can lead to
  subtle bugs, e.g. `Unique_id.Int (Unit)`.  Note that it is perfectly
  valid to apply any module to `F`, even though that is certainly not
  what we want.

  In 4.02, we can explicitly say that functor generates new types,
  i.e. it is generative. For this we use argument `()`.  So `F`
  becomes

  ```ocaml
  module F () : sig type t end
  ```

  You can only apply `F` to `()` or `(struct end)` but each
  application yields a new type `t`.

  ```ocaml
  module A = F ()
  module B = F ()
  module C = F (struct end)
  module D = F (String) (* illegal *)
  ```

  and now `A.t`, `B.t` and `C.t` are all different.

  Note that `F (struct end)` is still allowed but was converted to to
  `F ()` for consistency with signatures.

  Propagated generativity where necessary.  If inside a functor we use
  generative functor that creates new types, then we also need to make
  the enclosing functor generative.

  For functors that don't create types (like `Async.Log.Make_global`),
  generative or applicative functors are the same, but the syntax of
  generative functors is lighter.
- Exported `Core_kernel.Std.With_return`.
- Exposed the record type of `Source_code_position.t`.
- In `Weak_hashtbl.create`, exposed the `?growth_allowed` and `?size`
  arguments of the underlying `Hashtbl.create`.
- Added `with compare` to `Array`.
- Sped up `Int.pow`.

  Benchmarks before:

  | Name                                          |     Time/Run | mWd/Run | Percentage |
  |-----------------------------------------------|--------------|---------|------------|
  | [int_math.ml:int_math_pow] random[ 5] x 10000 | 140_546.89ns |         |     53.98% |
  | [int_math.ml:int_math_pow] random[10] x 10000 | 173_853.08ns |         |     66.77% |
  | [int_math.ml:int_math_pow] random[30] x 10000 | 219_948.85ns |         |     84.47% |
  | [int_math.ml:int_math_pow] random[60] x 10000 | 260_387.26ns |         |    100.00% |
  | [int_math.ml:int_math_pow] 2 ^ 30             |      11.34ns |         |            |
  | [int_math.ml:int_math_pow] 2L ^ 30L           |      21.69ns |   3.00w |            |
  | [int_math.ml:int_math_pow] 2L ^ 60L           |      22.95ns |   3.00w |            |

  and after:

  | Name                                          |     Time/Run | mWd/Run | Percentage |
  |-----------------------------------------------|--------------|---------|------------|
  | [int_math.ml:int_math_pow] random[ 5] x 10000 | 105_200.94ns |         |     80.78% |
  | [int_math.ml:int_math_pow] random[10] x 10000 | 117_365.82ns |         |     90.12% |
  | [int_math.ml:int_math_pow] random[30] x 10000 | 130_234.51ns |         |    100.00% |
  | [int_math.ml:int_math_pow] random[60] x 10000 | 123_621.45ns |         |     94.92% |
  | [int_math.ml:int_math_pow] 2 ^ 30             |       8.55ns |         |            |
  | [int_math.ml:int_math_pow] 2L ^ 30L           |      22.17ns |   3.00w |      0.02% |
  | [int_math.ml:int_math_pow] 2L ^ 60L           |      22.49ns |   3.00w |      0.02% |
- Removed the old, deprecated permission phantom types (`read_only`,
  etc.) and replaced them with the new =Perms= types.

  The old types had subtyping based on covariance and `private` types.
  The new types have subtyping based on contravariance and dropping
  capabilities.

  Renamed `read_only` as `read`, since `Perms` doesn't distinguish
  between them.

  The idiom for the type of a function that only needs read access
  changed from:

  ```ocaml
  val f : _ t -> ...
  ```

  to

  ```ocaml
  val f : [> read ] t -> ...
  ```

  This mostly hit `Iobuf` and its users.
- Added `String.is_substring`.
- Added `With_return.prepend`, and exposed `With_return.t` as
  contravariant.

  ```ocaml
  (** [prepend a ~f] returns a value [x] such that each call to [x.return] first applies [f]
      before applying [a.return].  The call to [f] is "prepended" to the call to the
      original [a.return].  A possible use case is to hand [x] over to an other function
      which returns ['b] a subtype of ['a], or to capture a common transformation [f]
      applied to returned values at several call sites. *)
  val prepend : 'a return -> f:('b -> 'a) -> 'b return
  ```
- Moved the `Gc` module's alarm functionality into a new
  `Gc.Expert.Alarm` module.

  The was done because the Gc alarms introduce threading semantics.
- Exposed modules in `Core_kernel.Std`: `Int_conversions`,
  `Ordered_collection_common`
- Removed `Pooled_hashtbl` from `Hashable.S`, to eliminate a
  dependency cycle between `Int63` and `Pool`.

  This was needed to use `Int63` in `Pool`.  Previously, `Int63 <- Int
  <- Hashable <- Pool`, which made it impossible to use `Int63` in
  `Pool`.

  So, we are removing the dependency `Hashable <- Pool`, simplifying
  `Hashable` to not include `Pooled_hashtbl`, and letting users call
  the `Pooled_hashtbl` functor directly when necessary.
- Added to `Pool.Pointer.Id` conversions to and from `Int63`.
- Made `Pooled_hashtbl.resize` allocate less.
- Removed `Pool.pointer_of_id_exn_is_supported`, which was always
  `true`.
- Added `with compare` to `Info`, `Error`, `Or_error`.
- Moved `Backtrace` from `Core`
- In C stubs, replaced `intxx` types by `intxx_t`.

  Following this: http://caml.inria.fr/mantis/view.php?id=6517

  Fixes #23
- Removed `Backtrace.get_opt`, which is no longer necessary now that
  `Backtrace.get` is available on all platforms.
- Added module types: `Stable`, `Stable1`, `Stable2`.
- Exposed `Core_kernel.Std.Avltree`.
- Removed from `Binary_packing` a duplicated exception,
  `Pack_signed_32_argument_out_of_range`.

  Closes #26
- Made `Info`, `Error`, and `Or_error` stable.

  The new stable serialization format is distinct from the existing
  unstable serialization format in the respective modules, which wasn't
  changed.
- Add `Sequence.Step.sexp_of_t`.

## 112.06.00

- Made `String_id` have `Stable_containers.Comparable`.
- Changed `Gc.disable_compaction` to require an `allocation_policy`.
- Made `Option` match `Invariant.S1`.
- Added `Sequence.filter`, `compare`, and `sexp_of_t`.
- Added `With_return.with_return_option`, abstracting a common pattern
  of `with_return`.

        val with_return        : ('a return -> 'a  ) -> 'a
        val with_return_option : ('a return -> unit) -> 'a option

- Install a handler for uncaught exceptions, using
  `Printexc.set_uncaught_exception_handler`, new in OCaml 4.02.
- Changed `Day_of_week` representation to a normal variant.
- Changed `Exn.handle_uncaught` so that if it is unable to print, it
  still does `exit 1`.
- Added `Sexp.of_sexp_allow_extra_fields`, previously in
  `Core_extended.Sexp`.
- Changed the implementation of `Exn.raise_without_backtrace` to use
  `raise_notrace`, new in OCaml 4.02.
- Added `Float` functions for converting to and from IEEE
  sign/exponent/mantissa.
- Added `String.Caseless` module, which compares and hashes strings
  ignoring case.
- Reimplemented `Type_equal.Id` using extensible types (new in OCaml
  4.02), removing a use of `Obj.magic`.

    Changed `Type_equal.Id.same_witness` to return `option` rather than
    `Or_error`, which allows it to be implemented without allocation.

- Removed a reference to the `Unix` module. Applications using
  `core_kernel` should be able to link without `unix.cma` again.
- Made `Char.is_whitespace` accept `\f` and `\v` as whitespace,
  matching C.

## 112.01.00

- Removed vestigial code supporting OCaml 4.00.
- Used `{Hashable,Comparable}.S_binable` in `Day_of_week` and `Month`.
- Improved the performance of `Set_once.set`.
- Added `Type_equal.Lift3` functor.
- Replaced occurrences of `Obj.magic 0` with `Obj.magic None`.

  With the former the compiler might think the destination type is
  always an integer and instruct the GC to ignore references to such
  values.  The latter doesn't have this problem as options are not
  always integers.
- Made `String_id.of_string` faster.
- Added `Bigstring` functions for reading and writing the
  size-prefixed bin-io format.

  - `bin_prot_size_header_length`
  - `write_bin_prot`
  - `read_bin_prot`
  - `read_bin_prot_verbose_errors`
- Added `{Info,Error}.to_string_mach` which produces a single-line
  sexp from an `Error.t`.
- Added `{Info,Error}.createf`, for creation from a format string.
- Added new `Perms` module with phantom types for managing access
  control.

  This module supersedes the `read_only`, `read_write`, and
  `immutable` phantom types, which are now deprecated, and will be
  removed in the future.  This module uses a different approach using
  sets of polymorphic variants as capabilities, and contravariant
  subtyping to express dropping capabilities.

  This approach fixes a bug with the current phantom types used for
  `Ref.Permissioned` in which `immutable` types aren't guaranteed to
  be immutable:

  ```ocaml
  let r = Ref.Permissioned.create 0
  let r_immutable = (r :  (int, immutable) Ref.Permissioned.t)
  let () = assert (Ref.Permissioned.get r_immutable = 0)
  let () = Ref.Permissioned.set r 1
  let () = assert (Ref.Permissioned.get r_immutable = 1)
  ```

  The bug stems from the fact that the phantom-type parameter is
  covariant, which allows OCaml's relaxed value restriction to kick
  in, which allows one to create a polymorphic value, which can then
  be viewed as both immutable and read write.  Here's a small
  standalone example to demonstrate:

  ```ocaml
  module F (M : sig
              type +'z t
              val create : int -> _ t
              val get : _ t -> int
              val set : read_write t -> int -> unit
            end) : sig
    val t : _ M.t
  end = struct
    let t = M.create 0
    let t_immutable = (t :  immutable M.t)
    let () =
      assert (M.get t_immutable = 0);
      M.set t 1;
      assert (M.get t_immutable = 1);
    ;;
  end
  ```

  The new approach fixes the problem by making the phantom-type
  parameter contravariant, and using polymorphic variants as
  capabilities to represent what operations are allowed.
  Contravariance allows one to drop capabilities, but not add them.
- Added `Int.Hex` module, which has hexadecimal sexp/string
  conversions.
- Added `Gc.major_plus_minor_words`, for performance reasons.

## 111.28.00

- Added `Pooled_hashtbl.resize` function, to allow preallocating a table
  of the desired size, to avoid growth at an undesirable time.
- Added `Pooled_hashtbl.on_grow` callback, to get information about
  hashtbl growth.
- Changed `Hashable.Make` to not export a `Hashable` module.

    The `Hashable` module previously exported was useless, and shadowed
    `Core.Std.Hashable`.

- Moved `Common.does_raise` to `Exn.does_raise`, to make it easier to
  find.
- Added `Float.one`, `minus_one`, and `~-`.  (fixes #12).
- Removed `Core.Std.unimplemented` and renamed it as
  `Or_error.unimplemented`.

    It is not used enough to live in the global namespace.

## 111.25.00

- Fix build on FreeBSD

  Closes #10
- Added functions to `Container` interface: `sum`, `min_elt`,
  `max_elt`.

  ```ocaml
  (** Returns the sum of [f i] for i in the container *)
  val sum
    : (module Commutative_group.S with type t = 'sum)
    -> t -> f:(elt -> 'sum) -> 'sum

  (** Returns a min (resp max) element from the collection using the provided [cmp]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same
      complexity as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : t -> cmp:(elt -> elt -> int) -> elt option
  val max_elt : t -> cmp:(elt -> elt -> int) -> elt option
  ```
- Made `Core_hashtbl_intf` more flexible. For instance supports
  modules that require typereps to be passed when creating a table.

  Address the following issues:

  The type `('a, 'b, 'z) create_options` needs to be consistently used
  so that `b` corresponds with the type of data values in the returned
  hash table.  The type argument was wrong in several cases.

  Added the type `('a, 'z) map_options` to `Accessors` so that
  map-like functions -- those that output hash tables of a different
  type than they input -- can allow additional arguments.
- Fixed a bug in `Dequeue`'s `bin_prot` implementation that caused it
  to raise when deserializing an empty dequeue.
- Made `Container.Make`'s interface match `Monad.Make`.
- Deprecated infix `or` in favor of `||`.
- Simplified the interface of `Arg` (which was already deprecated in
  favor of `Command`).
- Replaced `Bag.fold_elt` with `Bag.filter`.
- `Memo.general` now raises on non-positive `cache_size_bound`.
- Removed `Option.apply`.
- Removed `Result.call`, `Result.apply`.
- Moved `Quichcheck` to `core_extended`.

  It should not be used in new code.

## 111.21.00

- Removed our custom C stub for closing channels, reverting to the one
  in the OCaml runtime.

    A long time ago we found that the OCaml runtime did not release the
    lock before calling `close` on the fd underlying a channel.  On some
    filesystems (e.g. smb, nfs) this could cause a runtime hang.  We
    filed a bug with INRIA and wrote our own `close` function which
    `In_channel` calls to this day.  The bug has long been fixed, and
    our function is probably buggy, so this reverts us to the runtime's
    `close`.

- Added `Float.{of,to}_int64_preserve_order`, which implement the
  order-preserving zero-preserving bijection between non-NaN floats and
  99.95% of `Int64`'s.

    Used the new function to improve `one_ulp`, which is now exposed:

        (** The next or previous representable float.  ULP stands for "unit of least precision",
            and is the spacing between floating point numbers.  Both [one_ulp `Up infinity] and
            [one_ulp `Down neg_infinity] return a nan. *)
        val one_ulp : [`Up | `Down] -> t -> t

- Changed `Map.symmetric_diff` to return a `Sequence.t`
  instead of a `list`.
- Added `Sequence.filter_map`.
- Improved `Stable_unit_test.Make_sexp_deserialization_test`'s error
  message so that it includes the expected sexp.

## 111.17.00

- In `Bigstring`, made many operations use compiler primitives new in
  OCaml 4.01.

  Exposed `Bigstring.get` and `set` as compiler primitives in the
  interface.

  Added `Bigstring.unsafe_get_int64_{le,be}_trunc`.
- Made `Error` round trip `exn`, i.e. `Error.to_exn (Error.of_exn exn)
  = exn`.
- Added to `failwiths` an optional `?here:Lexing.position` argument.
- Added `with typerep` to `Flags.S`.
- Optimized `List.dedup []` to return immediately.
- Added `data` argument to polymorphic type `Hashtbl_intf.Creators.create_options`.

  This allows implementations of `Hashtbl_intf.Creators` to have
  constructor arguments that depend on the type of both key and data
  values.  For example:

  ```ocaml
  module type Hashtbl_creators_with_typerep =
    Hashtbl_intf.Creators
    with type ('key, 'data, 'z) create_options
      =  typerep_of_key:'key Typerep.t
      -> typerep_of_data:'data Typerep.t
      -> 'z
  ```
- Improved the interface for getting `Monad.Make` to define `map` in
  terms of `bind`.

  Instead of passing a `map` function and requiring everyone who wants
  to define `map` using `bind` to call a special function, we use a
  variant type to allow the user to say what they want:

  ```ocaml
  val map : [ `Define_using_bind
            | `Custom of ('a t -> f:('a -> 'b) -> 'b t)
            ]
  ```
- Improved the performance of many `Dequeue` functions.

  Previously, many `Dequeue.dequeue`-type functions worked by raising
  and then catching an exception when the dequeue is empty.  This is
  much slower than just testing for emptiness, which is what the code
  now does.

  This improves the performance of `Async.Writer`, which uses
  `Dequeue.dequeue_front`.

## 111.13.00

- Added a `Sequence` module that implements polymorphic, on-demand
  sequences.

    Also implemented conversion to `Sequence.t` from various containers.

- Improved the explicitness and expressiveness of
  `Binary_searchable.binary_search`.

    `binary_search` now takes an additional (polymorphic variant)
    argument describing the relationship of the returned position to the
    element being searched for.

        val binary_search
          :  ?pos:int
          -> ?len:int
          -> t
          -> compare:(elt -> elt -> int)
          -> [ `Last_strictly_less_than         (** {v | < elt X |                       v} *)
             | `Last_less_than_or_equal_to      (** {v |      <= elt       X |           v} *)
             | `Last_equal_to                   (** {v           |   = elt X |           v} *)
             | `First_equal_to                  (** {v           | X = elt   |           v} *)
             | `First_greater_than_or_equal_to  (** {v           | X       >= elt      | v} *)
             | `First_strictly_greater_than     (** {v                       | X > elt | v} *)
             ]
          -> elt
          -> int option

- Added a new function, `Binary_searchable.binary_search_segmented`,
that can search an array consisting of two segments, rather than ordered
by `compare`.

        (** [binary_search_segmented ?pos ?len t ~segment_of which] takes an [segment_of]
            function that divides [t] into two (possibly empty) segments:

            {v
              | segment_of elt = `Left | segment_of elt = `Right |
            v}

            [binary_search_segmented] returns the index of the element on the boundary of the
            segments as specified by [which]: [`Last_on_left] yields the index of the last
            element of the left segment, while [`First_on_right] yields the index of the first
            element of the right segment.  It returns [None] if the segment is empty.

            By default, [binary_search] searches the entire [t].  One can supply [?pos] or
            [?len] to search a slice of [t].

            [binary_search_segmented] does not check that [segment_of] segments [t] as in the
            diagram, and behavior is unspecified if [segment_of] doesn't segment [t].  Behavior
            is also unspecified if [segment_of] mutates [t]. *)
        val binary_search_segmented
          :  ?pos:int
          -> ?len:int
          -> t
          -> segment_of:(elt -> [ `Left | `Right ])
          -> [ `Last_on_left | `First_on_right ]
          -> int option

- Made `Queue` match `Binary_searchable.S1`.
- Made `Gc.Stat` and `Gc.Control` match `Comparable`.
- Fixed some unit tests in `Type_immediacy` that were fragile due to GC.

## 111.11.00

- Added to `String` functions for substring search and replace, based
  on the KMP algorithm.

  Here are some benchmarks, comparing `Re2` for a fixed pattern,
  Mark's kmp from extended_string, and this implementation ("needle").

  The pattern is the usual `abacabadabacabae...`.  The text looks
  similar, with the pattern occurring at the very end.

  For =Re2= and =Needle= search benchmarks, the pattern is
  preprocessed in advance, outside of the benchmark.

  FWIW: I've also tried searches with pattern size = 32767, but =Re2=
  blows up, saying:

  ```
  re2/dfa.cc:447: DFA out of memory: prog size 32771 mem 2664898
  ```

  | Name                          |        Time/Run |       mWd/Run |    mjWd/Run | Prom/Run | Percentage |
  |-------------------------------|-----------------|---------------|-------------|----------|------------|
  | create_needle_15              |        102.56ns |        21.00w |             |          |            |
  | re2_compile_15                |      6_261.48ns |               |       3.00w |          |      0.01% |
  | create_needle_1023            |     13_870.48ns |         5.00w |   1_024.01w |          |      0.03% |
  | re2_compile_1023              |    107_533.32ns |               |       3.03w |          |      0.24% |
  | create_needle_8191            |     90_107.02ns |         5.00w |   8_192.01w |          |      0.20% |
  | re2_compile_8191              |  1_059_873.47ns |               |       3.28w |    0.28w |      2.37% |
  | create_needle_524287          |  6_430_623.96ns |         5.00w | 524_288.09w |          |     14.35% |
  | re2_compile_524287            | 44_799_605.83ns |               |       3.77w |    0.77w |    100.00% |
  | needle_search_15_95           |        349.65ns |         4.00w |             |          |            |
  | re2_search_15_95              |        483.11ns |               |             |          |            |
  | mshinwell_search_15_95        |      1_151.38ns |       781.01w |             |          |            |
  | needle_search_15_815          |      2_838.85ns |         4.00w |             |          |            |
  | re2_search_15_815             |      3_293.06ns |               |             |          |            |
  | mshinwell_search_15_815       |      8_360.57ns |     5_821.07w |       0.55w |    0.55w |      0.02% |
  | needle_search_15_2415         |      8_395.84ns |         4.00w |             |          |      0.02% |
  | re2_search_15_2415            |      9_594.14ns |               |             |          |      0.02% |
  | mshinwell_search_15_2415      |     24_602.09ns |    17_021.16w |       1.62w |    1.62w |      0.05% |
  | needle_search_1023_6143       |     14_825.50ns |         4.00w |             |          |      0.03% |
  | re2_search_1023_6143          |     40_926.59ns |               |             |          |      0.09% |
  | mshinwell_search_1023_6143    |     81_930.46ns |    49_149.66w |   1_025.65w |    1.65w |      0.18% |
  | needle_search_1023_52223      |    126_465.96ns |         4.00w |             |          |      0.28% |
  | re2_search_1023_52223         |    365_359.98ns |               |             |          |      0.82% |
  | mshinwell_search_1023_52223   |    527_323.73ns |   371_715.39w |   1_033.17w |    9.17w |      1.18% |
  | needle_search_1023_154623     |    377_539.53ns |         4.00w |             |          |      0.84% |
  | re2_search_1023_154623        |  1_001_251.93ns |               |             |          |      2.23% |
  | mshinwell_search_1023_154623  |  1_499_835.01ns | 1_088_518.15w |   1_033.19w |    9.19w |      3.35% |
  | needle_search_8191_49151      |    115_223.31ns |         4.00w |             |          |      0.26% |
  | re2_search_8191_49151         |    559_487.38ns |               |             |          |      1.25% |
  | mshinwell_search_8191_49151   |    653_981.19ns |   393_219.50w |   8_201.01w |    9.01w |      1.46% |
  | needle_search_8191_417791     |    976_725.24ns |         4.00w |             |          |      2.18% |
  | re2_search_8191_417791        |  4_713_965.69ns |               |             |          |     10.52% |
  | mshinwell_search_8191_417791  |  4_224_417.93ns | 2_973_709.32w |   8_202.37w |   10.37w |      9.43% |
  | needle_search_8191_1236991    |  2_912_863.78ns |         4.00w |             |          |      6.50% |
  | re2_search_8191_1236991       | 14_039_230.59ns |               |             |          |     31.34% |
  | mshinwell_search_8191_1236991 | 11_997_713.73ns | 8_708_130.87w |   8_202.47w |   10.47w |     26.78% |
- Added to `Set` functions for converting to and from a `Map.t`.

  ```ocaml
  val to_map : ('key, 'cmp) t -> f:('key -> 'data) -> ('key, 'data, 'cmp) Map.t
  val of_map_keys : ('key, _, 'cmp) Map.t -> ('key, 'cmp) t
  ```

  This required adding some additional type trickery to
  `Core_set_intf` to indicate that the comparator for a given module
  may or may not be fixed.
- Added an optional `iter` parameter to `Container.Make`.

  A direct implementation of `iter` is often more efficient than
  defining `iter` in terms of `fold`, and in these cases, the results
  of `Container.Make` that are defined in terms of `iter` will be more
  efficient also.
- Added `Int.pow` (and for other integer types), for bounds-checked
  integer exponentiation.

## 111.08.00

- Added `Hashtbl.for_all` and `for_alli`.
- Added `Float.to_padded_compact_string` for converting a floating point
  number to a lossy, compact, human-readable representation.

    E.g., `1_000_001.00` becomes `"1m "`.

- Tweaked the form of the definition of `Blang.Stable.V1`.

    Removed a `type t_` that is not necessary now that we can use `nonrec`
    without triggering spurious warnings.

## 111.06.00

- Added inline benchmarks for `Array`

  Here are some of the results from the new benchmarks, with some
  indexed tests dropped.

  | Name                                                |    Time/Run | mWd/Run |  mjWd/Run |
  |-----------------------------------------------------|-------------|---------|-----------|
  | [core_array.ml:Alloc] create:0                      |     13.65ns |         |           |
  | [core_array.ml:Alloc] create:100                    |     99.83ns | 101.00w |           |
  | [core_array.ml:Alloc] create:255                    |    201.32ns | 256.00w |           |
  | [core_array.ml:Alloc] create:256                    |  1_432.43ns |         |   257.00w |
  | [core_array.ml:Alloc] create:1000                   |  5_605.58ns |         | 1_001.01w |
  | [core_array.ml:Blit.Poly] blit (tuple):10           |     87.10ns |         |           |
  | [core_array.ml:Blit.Poly] blito (tuple):10          |    112.14ns |   2.00w |           |
  | [core_array.ml:Blit.Poly] blit (int):10             |     85.25ns |         |           |
  | [core_array.ml:Blit.Poly] blito (int):10            |    107.23ns |   2.00w |           |
  | [core_array.ml:Blit.Poly] blit (float):10           |     84.71ns |         |           |
  | [core_array.ml:Blit.Poly] blito (float):10          |     86.71ns |   2.00w |           |
  | [core_array.ml:Blit.Int] blit:10                    |     19.77ns |         |           |
  | [core_array.ml:Blit.Int] blito:10                   |     23.54ns |   2.00w |           |
  | [core_array.ml:Blit.Float] blit:10                  |     19.87ns |         |           |
  | [core_array.ml:Blit.Float] blito:10                 |     24.12ns |   2.00w |           |
  | [core_array.ml:Is empty] Polymorphic '='            |     18.21ns |         |           |
  | [core_array.ml:Is empty] Array.equal                |      8.08ns |   6.00w |           |
  | [core_array.ml:Is empty] phys_equal                 |      2.98ns |         |           |
  | [core_array.ml:Is empty] Array.is_empty (empty)     |      2.98ns |         |           |
  | [core_array.ml:Is empty] Array.is_empty (non-empty) |      3.00ns |         |           |
- Moved `Thread_safe_queue` to core
- Generalized the type of `Exn.handle_uncaught_and_exit` to `(unit ->
  'a) -> 'a`.

  In the case where `handle_uncaught_and_exit` succeeds, it can return
  the value of the supplied function.

  It's type had been:

  ```ocaml
  val handle_uncaught_and_exit : (unit -> never_returns) -> never_returns
  ```
- Added `Int.round*` functions for rounding to a multiple of another
  int.

  ```ocaml
  val round : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> to_multiple_of:t -> t

  val round_towards_zero : t -> to_multiple_of:t -> t
  val round_down         : t -> to_multiple_of:t -> t
  val round_up           : t -> to_multiple_of:t -> t
  val round_nearest      : t -> to_multiple_of:t -> t
  ```

  These functions were added to `Int_intf.S`, implemented by `Int`,
  `Nativeint`, `Int32`, and `Int64`.

  Various int modules were also lightly refactored to make it easier
  in the future to implement common operators available for all
  modules implementing the int interface via a functor to share the
  code.

## 111.03.00

- Added `Error.to_string_hum_deprecated` that is the same as
  `Error.to_string_hum` pre 109.61.
- Changed `Error.to_string_hum` so that
  `Error.to_string_hum (Error.of_string s) = s`.

  This fixed undesirable sexp escaping introduced in 109.61 and
  restores the pre-109.61 behavior for the special case of
  `Error.of_string`.  A consequence of the removal of the custom
  `to_string_hum` converter in 109.61 was that:

  ```ocaml
  Error.to_string_hum (Error.of_string s) =
      Sexp.to_string_hum (Sexp.Atom s)
  ```

  That introduced sexp escaping of `s`.
- Added to `Doubly_linked` functions for moving an element
  within a list.

  ```ocaml
  val move_to_front : 'a t -> 'a Elt.t -> unit
  val move_to_back  : 'a t -> 'a Elt.t -> unit
  val move_after    : 'a t -> 'a Elt.t -> anchor:'a Elt.t -> unit
  val move_before   : 'a t -> 'a Elt.t -> anchor:'a Elt.t -> unit
  ```
- Improved `Core_map_unit_tests.Unit_tests` to allow arbitrary data
  in the map, not just `ints`.

  This was done by eta expansion.

## 110.01.00

- Changed `Queue` from a linked to an array-backed implementation.

  Renamed the previous implementation to `Linked_queue`.

  Renamed `transfer`, which was constant time, as `blit_transfer`,
  which is linear time.

  Removed `partial_iter`.  One can use `with_return`.

  Added `singleton`, `filter`, `get`, `set`.
- For `Error` and `Info`, changed `to_string_hum` to use `sexp_of_t`
  and `Sexp.to_string_hum`, rather than a custom string format.
- Changed the output format of `Validate.errors` to be a sexp.
- Added `Hashtbl.of_alist_or_error` and `Map.of_alist_or_error`.
- Added `String_id.Make` functor, which includes a module name for
  better error messages.
- Exposed `Bucket.size`.
- Changed the default for `Debug.should_print_backtrace` to be `false`
  rather than `true`.

  Usually the backtraces are noise.
- Removed the tuning of gc parameters built in to Core, so that the
  default is now the stock OCaml settings.

  Such tuning doesn't belong in Core, but rather done per application.
  Also, the Core settings had fallen way out of date, and not kept up
  with changes in the OCaml runtime settings.  We have one example
  (lwt on async) where the Core settings significantly slowed down a
  program.
- Added `Exn.raise_without_backtrace`, to raise without building a
  backtrace.

  `raise_without_backtrace` never builds a backtrace, even when
  `Backtrace.am_recording ()`.
- Made `with_return` faster by using `Exn.raise_without_backtrace`.
- Improved `with_return` to detect usage of a `return` after its
  creating `with_return` has returned.

## 109.60.00

- Added `Gc.keep_alive`, which ensures its argument is live at the point
  of the call.
- Added `Sexp.With_text` module, which keeps a value and the a sexp it
  was generated from, preserving the original formatting.

## 109.58.00

- Moved all of the `Gc` module into `Core_kernel`.

  Part of the `Gc` module used to be in `Core` because it used
  threads.  But it doesn't use threads anymore, so can be all in
  `Core_kernel`.
- Made `Stable.Map` and `Set` have `with compare`.
- Added `String.rev`.

  Closes janestreet/core#16

  We will not add `String.rev_inplace`, as we do not want to encourage
  mutation of strings.
- Made `Univ_map.Key` equivalent to `Type_equal.Id`.
- Added `Univ.view`, which exposes `Univ.t` as an existential, `type t
  = T : 'a Id.t * 'a -> t`.

  Exposing the existential makes it possible to, for example, use
  `Univ_map.set` to construct a `Univ_map.t`from a list of `Univ.t`s.

  This representation is currently the same as the underlying
  representation, but to make changes to the underlying representation
  easier, it has been put in a module `Univ.View`.

## 109.55.00

- Added `with typerep` to many `Core` types.
- Changed `Flat_queue` to raise if the queue is mutated during
  iteration.
- Improved `Map.merge` to run in linear time.

## 109.53.00

- Added `Float.to_string_round_trippable`, which produces a string
  that loses no precision but (usually) uses as few digits as
  possible.

  This can eliminate noise at the end (e.g. `3.14` not
  `3.1400000000000001243`).

  Benchmarks:

  New sexp:

  | Name                   | Time/Run | mWd/Run | Percentage |
  |------------------------|----------|---------|------------|
  | new Float.sexp_of 3.14 | 463.28ns |   6.00w |     48.88% |
  | new Float.sexp_of e    | 947.71ns |  12.00w |    100.00% |

  Old sexp:

  | Name                   | Time/Run | mWd/Run | Percentage |
  |------------------------|----------|---------|------------|
  | old Float.sexp_of 3.14 | 841.99ns | 178.00w |     98.03% |
  | old Float.sexp_of e    | 858.94ns | 178.00w |    100.00% |

  Much of the speedup in the 3.14 case comes from the fact that
  `format_float "%.15g"` is much faster than `sprintf "%.15g"`.  And
  of course the above does not capture any of the benefits of dealing
  with shorter strings down the road.

  Here are some detailed benchmarks of the various bits and pieces of
  what's going on here:

  | Name                                |   Time/Run | mWd/Run | Percentage |
  |-------------------------------------|------------|---------|------------|
  | format_float '%.15g' 3.14           |   335.96ns |   2.00w |     32.71% |
  | format_float '%.17g' 3.14           |   394.18ns |   4.00w |     38.38% |
  | format_float '%.20g' 3.14           |   459.79ns |   4.00w |     44.77% |
  | format_float '%.40g' 3.14           |   638.06ns |   7.00w |     62.13% |
  | sprintf '%.15g' 3.14                |   723.71ns | 165.00w |     70.47% |
  | sprintf '%.17g' 3.14                |   803.44ns | 173.00w |     78.23% |
  | sprintf '%.20g' 3.14                |   920.78ns | 176.00w |     89.66% |
  | sprintf '%.40g' 3.14                |   990.09ns | 187.00w |     96.41% |
  | format_float '%.15g' e              |   357.59ns |   4.00w |     34.82% |
  | format_float '%.17g' e              |   372.16ns |   4.00w |     36.24% |
  | format_float '%.20g' e              |   434.59ns |   4.00w |     42.32% |
  | format_float '%.40g' e              |   592.78ns |   7.00w |     57.72% |
  | sprintf '%.15g' e                   |   742.12ns | 173.00w |     72.26% |
  | sprintf '%.17g' e                   |   747.92ns | 173.00w |     72.83% |
  | sprintf '%.20g' e                   |   836.30ns | 176.00w |     81.43% |
  | sprintf '%.40g' e                   | 1_026.96ns | 187.00w |    100.00% |
  | valid_float_lexem 12345678901234567 |    76.29ns |   9.00w |      7.43% |
  | valid_float_lexem 3.14              |     9.28ns |   5.00w |      0.90% |
  | float_of_string 3.14                |   130.19ns |   2.00w |     12.68% |
  | float_of_string 1234567890123456.7  |   184.33ns |   2.00w |     17.95% |
  | to_string 3.14                      |   316.47ns |   7.00w |     30.82% |
  | to_string_round_trippable 3.14      |   466.02ns |   9.00w |     45.38% |
  | to_string e                         |   315.41ns |   7.00w |     30.71% |
  | to_string_round_trippable e         |   949.12ns |  15.00w |     92.42% |

- Replaced `Float.min_positive_value` with `min_positive_normal_value`
  and `min_positive_subnormal_value`.
- Added some functions to `Float.O`: `abs`, `of_float`, and
  `Robustly_comparable.S`.
- Small improvements to the `Heap` module.

  Implemented `Heap.iter` directly rather than in terms of `fold`.

  In `heap.ml`, fixed the idiom for using `Container.Make`.
- Added an `Int.O` and other `Int*.O` modules, with arithmetic
  operators, infix comparators, and a few useful arithmetic values.
- Added `Int.( ~- )`, for unary negation.
- Added `Pool.unsafe_free`.
- Added `Percent` module.

## 109.52.00

- Added to `Binary_packing` module functions for packing and unpacking
  signed 64-bit ints in little- and big-endian.
- Changed the `Comparator` interfaces to no longer have `with bin_io`
  or `with sexp`.

  The `Comparator` interfaces are now just about having a comparator.

  Also, renamed `type comparator` as `type comparator_witness`.  And,
  removed `Comparator.S_binable`, since one can use:

  ```ocaml
  type t with bin_io
  include Comparator.S with type t :` t
  ```
- Changed `Comparator.Make` to return a module without a type `t`,
  like other `*able` functors,

   This made it possible to remove the signature constraint when
  `Comparator.Make` is applied.
- Made `Comparable.S_binable` be like `Comparable.S` and not have
  `type t with sexp`.

  The following two functors now fail to type check:

  ```ocaml
  module F1 (M : Comparable.S        ) : sig type t with sexp end ` M
  module F2 (M : Comparable.S_binable) : sig type t with sexp end ` M
  ```

  whereas previously `F1` was rejected and `F2` was accepted.
- Changed the `Monad.Make` functor to require a `val map` argument.

  This was done since we almost always want a specialized `map`, and
  we kept making the mistake of not overriding the generic one in the
  three places needed.

  Added `Monad.map_via_bind`, which one can use to create a standard
  `map` function using `bind` and `return`.
- Removed unnecessary signature constraints on the result of applying
  `Monad.Make`.

  Some time ago, `Monad.Make` changed from returning:

  ```ocaml
  S with type 'a t ` 'a M.t
  ```

  to returning:

  ```ocaml
  S with type 'a t :` 'a M.t
  ```

  so we no longer need to constrain the result of `Monad.Make` at its
  uses to remove `t`.
- Changed `String.exists` and `String.for_all` to iterate by
  increasing index rather than decreasing.
- Added `with compare` to module `Ref`.
- Made `Flags` be `Comparable`, with the order consistent with bitwise
  subset.
- Cleaned up the implementation of `Union_find`.

  Improvemed the code in `union_find.ml`:

  * Removed an assert false.
  * do not reallocate a parent node during compress. This should
    result in more stability for sets memory wise.
  * Added implementation notes.
  * Renamed internal variant constructors.
  * Added unit tests.
- Added `Float.O`, a sub-module intended to be used with local opens.

  The idea is to be able to write expressions like:

  ```ocaml
  Float.O.((3. + 4.) > 6. / 2.)
  ```

  This idiom is expected to be extended to other modules as well.
- Added a `sexp_of_t` converter to `Type_equal.Id`.
- Replaced `Univ.Constr` with `Type_equal.Id`.
- Added `Debug.eprintf`, analogous to `eprint` and `eprints`.

## 109.47.00

- Added `Error.to_info` and `of_info`.
- Significantly sped up `Float.iround_*` functions.

  For `iround_down_exn`, the new version appears to use about 25% of the
  CPU time of the old version on non-negative floats.  For negative
  floats it uses around 60% of the CPU time.

  | Name                    | Time (ns) | % of max |
  |-------------------------|-----------|----------|
  | old iround_down_exn pos |     15.02 |    95.23 |
  | new iround_down_exn pos |      3.75 |    23.75 |
  | old iround_down_exn neg |     15.78 |   100.00 |
  | new iround_down_exn neg |      9.80 |    62.10 |
- Added `Binary_searchable.Make` functor to core, and used it in `Array` and `Dequeue`.
- Fixed `Bounded_int_table` to match `Invariant.S2`.
- Added to `Pool` support for `10-`, `11-`, and `12-` tuples.
- Added functions to the `Gc` module to get usage information without allocating.

  Added these functions, all of type `unit -> int`:

  ```
  minor_collections
  major_collections
  heap_words
  heap_chunks
  compactions
  top_heap_words
  ```

  They all satisfy:

  ```ocaml
  Gc.f () = (Gc.quick_stat ()).Gc.Stat.f
  ```

  They all avoid the allocation of the stat record, so one can monitor
  the garbage collector without perturbing it.

## 109.45.00

- Changed `Blang.bind` to short-circuit `And`, `Or`, and `If`
  expressions.

  For example if `bind t1 f ` false`, then `bind (and_ t1 t2) `
  false`, and will not evaluate `bind t2 f`.

- Renamed `Dequeue.get` as `get_opt`, and `get_exn` as `get`, to be
  consistent with other containers which don't use the `_exn` suffix
  for subscripting exceptions.
- Removed `Source_code_position.to_sexp_hum`, in favor of
  `sexp_of_t_hum`, which works smoothly with `with sexp`.
- Changed `Flat_queue_unit_tests` to run `Flat_queue.invariant`, which
  was mistakenly not being used.

## 109.44.00

- Implemented `Dequeue.iter` directly, instead of as a specialization
  of `fold`.

  Extended random tests to cover `iter`.

## 109.42.00

- Added `Array.is_sorted_strictly` and `List.is_sorted_strictly`.

  ```ocaml
  val is_sorted_strictly : 'a t -> cmp:('a -> 'a -> int) -> bool
  ```

- Added `Array.find_consecutive_duplicate` and `List.find_consecutive_duplicate`.

  ```ocaml
  val find_consecutive_duplicate : 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option
  ```

- Added `Array.truncate`, which changes (shortens) the length of an array.

  ```ocaml
  val truncate : _ t -> len:int -> unit
  ```

- Improved the debugging message in `Bounded_int_table.remove` to show the data structure's details.

- Added `Float.iround_lbound` and `iround_ubound`, the bounds for rounding to `int`.

- Added `Hashtbl.similar`, which is like `equal`, but allows the types of the values in the two tables to differ.

- Added `Pool.Pointer.phys_compare`, which is analagous to `phys_equal`, and does not require an argument comparison function.

  ```ocaml
  val phys_compare : 'a t -> 'a t -> int
  ```
- Exposed that `Pool.Debug`'s output types are the same as its input types.

## 109.41.00

- Added `Map.of_alist_reduce`.

  This function is a natural addition alongside `of_alist_fold`.  Its
  advantage is that it does not require an `init` argument like
  `of_alist_fold`.  Moreover, it does not involve `option` types, like
  `List.reduce` does in order to handle the empty list case.

## 109.39.00

- Implemented `Heap.iter` directly instead of in terms of `fold`.

## 109.37.00

- Added Core.Std.Poly as a short name for
  Core.Std.Polymorphic_compare.
- Exposed module Core.Std.Decimal.

## 109.36.00

- Made `Hashtbl.Poly.hash` equal `Caml.Hashtbl.hash`, and changed changed `String.hash` and `Float.hash` to match OCaml's hash function.

  Previously, `Core.Poly.hash` had been defined as:

  ```ocaml
  let hash x = hash_param 10 100 x
  ```

  This fell out of sync with OCaml's hash function, and was providing worse hash values.

- Fixed `Obj_array.singleton` to never create a float array.

  Also made it clearer that `Obj_array.copy` could never create a float
  array.

- Changed `Pool.create` to allow zero-length pools.

  Previously, `Pool.create ~capacity:0` had raised, which made it easy
  to write code that blows up on edge cases for no apparent reason.  For
  example, `Heap.copy` was written in a way that copying an empty heap
  would blow up (regardless of its capacity), and `Heap.of_array` would
  also blow up on an empty array.

- Added `String.split_lines`.

  ```ocaml
  (** [split_lines t] returns the list of lines that comprise [t].  The lines do
      not include the trailing ["\n"] or ["\r\n"]. *)
  val split_lines : t -> t list
  ```

## 109.35.00

- Added `with compare` to `List.Assoc.t`.
- Made `Pooled_hashtbl.create` handle non-positive and very large
  `size`s in the same way as `Core.Hashtbl`.
- Added `is_error`, `is_ok`, and `does_raise` to `Core.Std`.

  ```ocaml
  let is_error ` Result.is_error
  let is_ok    ` Result.is_ok
  val does_raise : (unit -> _) -> bool
  ```
- Reimplemented `Heap` and reworked the interface to be more standard.

  The new implementation uses pairing heaps and `Pool`.
- Added a module `Pool.Unsafe`, which is like `Pool`, except that
  `create` doesn't require an initial value.

  This makes it unsafe to access pool pointers after they have been
  freed.  But it is useful for situations when one isn't able to
  create an initial value, e.g. `Core.Heap`.
- Removed `Time.to_localized_string` and `Time.to_string_deprecated`.

  These did not include the time-zone offset.  Instead, use
  `Time.to_string` and `Time.to_string_abs`, which do include the
  time-zone offset.
- Exposed that `Int63.t = private int` on 64-bit machines.

  This lets the OCaml compiler avoid `caml_modify` when dealing with
  it.
- Added `Gc` stat functions that don't allocate: `Gc.minor_words`,
  `Gc.major_words`, `Gc.promoted_words`.

  Added the following `Gc` functions:

  ```ocaml
  Gc.minor_words : unit -> int
  Gc.major_words : unit -> int
  Gc.promoted_words : unit -> int
  ```

  such that these functions cause no allocations by themselves. The
  assumption being that 63-bit ints should be large enough to express
  total allocations for most programs.  On 32-bit machines the numbers
  may overflow and these functions are not as generally useful.

  These functions were added because doing memory allocation debugging
  with `Gc.quick_stat` as the primary means of understanding
  allocations is difficult: tracking down allocations of the order of
  a few hundred words in a hot loop by putting in lots of `quick_stat`
  statements becomes too intrusive because of the memory allocations
  they cause.

  Here are some benchmarks of existing `Gc` functions and the newly
  added functions:

  ```
  $ ./test_bench.exe -q 2 -clear name time +alloc +time-err
  Estimated testing time 12s (change using -quota SECS).
  ```

  | Name            | Time (ns) |      95% ci | Time R^2 | Minor |
  |-----------------|-----------|-------------|----------|-------|
  | quick_stat      |     92.16 | +0.72 -0.64 |     1.00 | 23.00 |
  | counters        |     33.63 | +0.26 -0.23 |     1.00 | 10.00 |
  | allocated_bytes |     37.89 | +0.34 -0.32 |     1.00 | 12.00 |
  | minor_words     |      4.63 | +0.03 -0.02 |     1.00 |       |
  | major_words     |      4.36 | +0.02 -0.02 |     1.00 |       |
  | promoted_words  |      4.10 | +0.03 -0.02 |     1.00 |       |

## 109.34.00

- Added a new module, `Flat_queue`, which is a queue of flat tuples.

  This is essentially:

  ```ocaml
  ('a1 * .. * 'aN) Queue.t
  ```

  However the queue is implemented as a `Flat_array`, so the tuples are layed out
  flat in the array and not allocated.

- Improved `Bounded_int_table.remove`'s error message when it detects an internal inconsistency.

- Added new `Debug` module.

- Changed `Invariant.invariant` to take `_here_` rather than a string.

- Made `Float` satisfy the `Identifiable` interface.

## 109.32.00

- Added `val Option.merge: 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t`.

- Added `val Validate.failf : ('a, unit, string, t) format4 -> 'a`.

- In `Validated.Make_binable`, made it possible to apply the validation function when un-bin-io-ing a value.

- Added `module Pooled_hashtbl` to `module type Hashable`.

  This is an alternative implementation to `Core.Hashtbl`.  It uses a
  standard linked list to resolve hash collisions, and `Pool` to manage
  the linked-list nodes.

## 109.31.00

- Renamed some functions in module `Lazy`: dropped the `lazy_` prefix from `is_val`, `from_val`, and `from_fun`.

## 109.30.00

  - Added module, `Core.Blit`, which codifies the type, implementation, and unit-testing of blit functions.

  - Added `remove_zero_flags` option to `Flags.Make`, to support flags that are zero.

    This fixes a problem with `Flags.Make` on CentOS 5 because `O_CLOEXEC` is `0` there.

  - Removed `Pool.None`, and folded `Pool.Obj_array` into `Pool` proper.

    `Pool.None` had its day, but `Pool.Obj_array` dominates it, so we don't need it any more.

## 109.28.00

- Moved all the contents of the `Zero` library into `Core`, mostly
  into `Core_kernel`.

  We want to start using `Zero` stuff more in `Core`, which couldn't
  be done with `Zero` as a separate library.

  Everything moved into `Core_kernel`, except for `Timing_wheel`,
  which moved into `Core` proper, due to its dependence on `Time`.
- Renamed `Flat_tuple_array` as `Flat_array`.
- Added `Dequeue.{front,back}_index_exn`

  These are more efficient than using `{front,back}_index` and then
  `Option.value_exn`.
- Exposed `Core.String.unsafe_{get,set}`.
