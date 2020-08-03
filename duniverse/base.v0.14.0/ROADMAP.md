# Stable Interface (v1.0)

  - [X] Make the entire library `-safe-string` compliant. This will involve
    introducing a `Bytes` module, removing all direct mutation on strings from
    the `String` module, and "re-typing" string values that require mutation to
    `bytes`.

  - [X] Do not export the `\*\_intf` modules from Base. Instead, any signatures
    should be exported by the `.ml` and `.mli`s.

  - [X] Only expose the first-class module interface of `Hashtbl`. Accompanying
    this should be cleanup of `Hashtbl_intf`, moving anything that's still
    required in core_kernel to the appropriate files in that project.

  - [X] Replace `Hashtbl.create (module String) ()` by just
    `Hashtbl.create (module String)`

  - [X] Remove `replace` from `Hashtbl_intf.Accessors`.

  - [X] Label one of the arguments of `Hashtbl_intf.merge_into` to indicate the
    flow of data.

  - [X] Merge `Hashtbl_intf.Key_common` and `Hashtbl_intf.Key_plain`.

  - [X] Use `Either.t` as the return value for `Map.partition`.

  - [X] Rename `Monad_intf.all_ignore` to `Monad_intf.all_unit`.

  - [ ] Eliminate all uses of `Not_found`, replacing them with descriptive error messages.

  - [X] Move the various private modules to `Base.Base_private`
    instead of `Base.Exported_for_specific_uses` and `Base.Not_exposed_properly`

  - [X] Use `compare` rather than `cmp` as the label for comparison functions
    throughout.

# Implementation Cleanup

  - [ ] Remove `ignore` and `(=)` from `Sexp_conv`'s public interface. These
    values are hidden from the documentation so their removal won't be
    considered a breaking API change.

  - [ ] Do not expose the type equality `Int63_emul.W.t = int64`.

  - [ ] Replace the exception thrown by `Float.of_string` with a named
    exception that's more descriptive.

  - [X] Delete the `Hashable` toplevel module. This is a vestige of the previous
    `Map` and `Set` implementations and is no longer needed.

  - [ ] Ensure that `Map` operations that are effective NO-OPs return the same
    `Map.t` they were provided. Candidate operations include e.g `add`, `remove`,
    `filter`.

  - [ ] Simplify the implementation of `Option.value_exn`, if possible.

  - [ ] Eliminate all instances of `open! Polymorphic_compare`

  - [ ] Refactor common blit code in `String.replace_all` and `String.replace_first`.

  - [ ] Delete unused function aliases in `Import0`

  - [ ] Put `Sexp_conv.Exn_converter` into its own file, with only an
        alias in Sexp_conv, so that it doesn't get pulled unless used

  - [ ] Create a file with all the basic types and their associated
        combinators (`sexp_of_t`, `compare`, `hash`), and expose the
        declaration

  - [ ] Put all the exported private modules from
    `Base.Exported_for_specific_uses` and `Base.Not_exposed_properly`
    in `Base.Base_private`

  - [ ] Decide on a better name for `Polymorphic_compare`.
        `Polymorphic_compare_intf` contains interface for comparison
        of non-polymorphic types, which is weird. Get rid of it and
        inline things in `Comparable_intf`

  - [X] `hashtbl_of_sexp` shouldn't live in Base.Sexp_conv since we
    have our own hash tables. Move it to sexplib

# Performance Improvements

  - [ ] In `Hash_set.diff`, use the size of each set to determine which to iterate
    over.

  - [ ] Ensure that the correct `compare` function and other related functions are
    exported by all modules. These functions should not be derived from
    a functor application, in order to ensure proper inlining. Implementing
    this change should also include benchmarks to verify the initial result,
    and to maintain it on an ongoing basis. See `bench/bench_int.ml` for
    examples.

  - [X] Optimize `Lazy.compare` by performing a `phys_equal` check before
    forcing the lazy value. Note that this will also change the semantics of
    `compare` and should be documented and rolled out with care.

  - [ ] Conduct a thorough performance review of the `Sequence` module.

# Documentation

  - [ ] Consolidate documentation the interface and implementation files
    related to the `Hash` module.

  - [ ] Add documentation to the `Ref` toplevel module.

  - [ ] Document properly how `String.unescape_gen` handles malformed strings

# Changes For The Distant Future

  - [ ] Make the various comparison functions return an `Ordering.t`
    instead of an `int`.
