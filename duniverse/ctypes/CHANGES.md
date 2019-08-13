## ctypes 0.14.0

* Add `CArray.of_string`  
  https://github.com/ocamllabs/ocaml-ctypes/pull/562  
  https://github.com/ocamllabs/ocaml-ctypes/pull/563

* Attach ocaml_integers.h to the cstubs subpackage  
  https://github.com/ocamllabs/ocaml-ctypes/pull/557

* fix custom operations of ComplexL  
  https://github.com/ocamllabs/ocaml-ctypes/pull/549

Thanks to Andreas Hauptmann (@fdopen), Rudi Grinberg (@rgrinberg) and
Marcello Seri (@mseri) for contributions to this release.

## ctypes 0.13.1

* Add -no-keep-locs to the foreign-(un)?threaded build rules.  
  https://github.com/ocamllabs/ocaml-ctypes/pull/553

## ctypes 0.13.0

* Print typedefed enums correctly  
  https://github.com/ocamllabs/ocaml-ctypes/pull/547

* Move the TYPE and FOREIGN signatures from cstubs to ctypes  
  https://github.com/ocamllabs/ocaml-ctypes/pull/544  
  https://github.com/ocamllabs/ocaml-ctypes/pull/537

Thanks to Leo White (@lpw25) for contributing to this release.

## ctypes 0.12.1

* Preserve intermediate values from views across foreign calls.  
  https://github.com/ocamllabs/ocaml-ctypes/pull/531

## ctypes 0.12.0

* Support for Fortran-layout bigarrays  
  https://github.com/ocamllabs/ocaml-ctypes/pull/523

* Use the integers package for signed and unsigned integer support  
  https://github.com/ocamllabs/ocaml-ctypes/pull/515

* Add support for binding enums defined without tags  
  https://github.com/ocamllabs/ocaml-ctypes/pull/510

Thanks to Bertrand Bonnefoy-Claudet (@bbc2) and Leo White (@lpw25) for
contributions to this release.

## ctypes 0.11.4

* Fix function pointer support on no-exec platforms  
  https://github.com/ocamllabs/ocaml-ctypes/pull/511

Thanks to Matthias Horn (@tiash) for contributing to this release.

## ctypes 0.11.3

* Stub out some `long double` functions that are not supported on NetBSD and OpenBSD  
  https://github.com/ocamllabs/ocaml-ctypes/pull/503

* Fix the build when shared libraries are not available  
  https://github.com/ocamllabs/ocaml-ctypes/pull/495

Thanks to Andreas Hauptmann (@fdopen) and Peter Zotov (@whitequark)
for contributions to this release.

## ctypes 0.11.2

### Bug fixes

* Fix truncation in UInt64.of_int; remove other uses of `Val_int`/`Int_val`  
  https://github.com/ocamllabs/ocaml-ctypes/pull/492

## ctypes 0.11.1

### Bug fixes

* Stub out some `long double complex` functions that are not supported on Android  
  https://github.com/ocamllabs/ocaml-ctypes/pull/486

Thanks to Peter Zotov (@whitequark) and Jeremie Dimino (@diml) for
contributions to this release.

## ctypes 0.11.0

### Features

* Support for the `long double` and `long double complex` types  
  https://github.com/ocamllabs/ocaml-ctypes/pull/475

* Support for binding foreign arrays  
  https://github.com/ocamllabs/ocaml-ctypes/pull/470

* Improved printing for structs and unions without tags  
  https://github.com/ocamllabs/ocaml-ctypes/pull/467

* Added a function `is_null`  
  https://github.com/ocamllabs/ocaml-ctypes/pull/463

### Bug fixes

* Custom operations (namely deserializing) for unsigned integers registered  
  https://github.com/ocamllabs/ocaml-ctypes/pull/480

* All objects kept live when the runtime lock is released  
  https://github.com/ocamllabs/ocaml-ctypes/pull/473

Thanks to Andrew Ray (@andrewray) and Yunxing Dai (@yunxing) for contributions
to this release.

## ctypes 0.10.2

### Bug fixes 

* Fix generated code for Lwt jobs where the return type is `void`.  
  https://github.com/ocamllabs/ocaml-ctypes/pull/460

Thanks to David Sheets (@dsheets) for contributions to this release.

## ctypes 0.10.1

### Bug fixes 

* Always extend integers returned from closures to full word size  
  (Fixes incorrectly-promoted integers returned from callbacks on ARM and MIPS)  
  https://github.com/ocamllabs/ocaml-ctypes/pull/456

Thanks to Andreas Hauptmann (@fdopen) for contributions to this release.

## ctypes 0.10.0

### Features

* Windows support for registration of C threads from callbacks  
  https://github.com/ocamllabs/ocaml-ctypes/issues/450

* Auto-load custom printers in the OCaml toplevel  
  https://github.com/ocamllabs/ocaml-ctypes/issues/448

* Add some extra `CArray` functions: `sub`, `copy`, `fold_right`,
  `fold_left`, `mapi`, `map`, `iter`  
  https://github.com/ocamllabs/ocaml-ctypes/pull/323

### Bug fixes

* Build fix for OpenBSD  
  https://github.com/ocamllabs/ocaml-ctypes/issues/454

* Fix for platforms (e.g. NetBSD) where standard integer types are
  defined as macros  
  https://github.com/ocamllabs/ocaml-ctypes/issues/453

* Add missing bounds check in `CArray`  
  https://github.com/ocamllabs/ocaml-ctypes/issues/447

### Compatibility

* Dl.dlsym now returns `nativeint`, not `Ctypes_ptr.voidp`  
  https://github.com/ocamllabs/ocaml-ctypes/issues/445
    
Thanks to Andreas Hauptmann (@fdopen) for contributions to this release.

## ctypes 0.9.2

### Bug fixes

* Revert a ctypes-foreign build regression in 0.9.1  
  https://github.com/ocamllabs/ocaml-ctypes/pull/443

## ctypes 0.9.1

### Bug fixes

* Fix build rules on OpenBSD  
  https://github.com/ocamllabs/ocaml-ctypes/issues/428

* Fix a memory leak that occurs when passing closures to C  
  https://github.com/ocamllabs/ocaml-ctypes/issues/436

Thanks to Enguerrand Decorne (@engil) for contributing to this release.

## ctypes 0.9.0

### Features

* NetBSD support

* Support for releasing the runtime lock in generated stubs  
  https://github.com/ocamllabs/ocaml-ctypes/issues/429

* Support for Lwt_preemptive  
  https://github.com/ocamllabs/ocaml-ctypes/issues/430

Thanks to Martin Lucina (@mato) for contributing to this release.

## ctypes 0.8.0

### Features

* Use unboxed types for `uint8_t` and `uint16_t`  
  https://github.com/ocamllabs/ocaml-ctypes/issues/413

* Reset `errno` in Lwt jobs before invoking job function  
  https://github.com/ocamllabs/ocaml-ctypes/issues/426

* Add a `~thread_registration` argument to `funptr` to support registering C threads with the OCaml runtime.  
  https://github.com/ocamllabs/ocaml-ctypes/issues/420

### Bug fixes

* Use `-opaque` for module interfaces whose `cmx` files are not installed.  
  https://github.com/ocamllabs/ocaml-ctypes/issues/423

* Install `cstubs` `cmx` files.  
  https://github.com/ocamllabs/ocaml-ctypes/issues/424

Thanks to David Sheets (@dsheets) and Demi Obenour (@DemiMarie) for contributions to this release.

## ctypes 0.7.0

### Features

* Add support for bytecode-only architectures  
    https://github.com/ocamllabs/ocaml-ctypes/issues/410

* Add a new `sint` type corresponding to a full-range C integer and update `errno` support to use `sint`  
    https://github.com/ocamllabs/ocaml-ctypes/issues/411

### Bug fixes

* Handle small integer return types correctly on big-endian platforms  
    https://github.com/ocamllabs/ocaml-ctypes/issues/404
    https://github.com/ocamllabs/ocaml-ctypes/issues/405

* Fix a bug with callbacks that return small types (less than a word)  
    https://github.com/ocamllabs/ocaml-ctypes/issues/405

Thanks to Stephane Glondu (@glondu) for contributions to this release.

## ctypes 0.6.2

### Bug fixes

* Fix for argument quoting in the Windows build after new cross compilation support  
    https://github.com/ocamllabs/ocaml-ctypes/pull/399

* Improve Lwt jobs support for functions with many or no arguments  
    https://github.com/ocamllabs/ocaml-ctypes/pull/400

Thanks to Andreas Hauptmann (@fdopen) for contributing to this release.

## ctypes 0.6.1

### Bug fixes

* Fix constructor qualification in code generated for inverted stubs:  
    https://github.com/ocamllabs/ocaml-ctypes/pull/397

## ctypes 0.6.0

### Features

* The `Cstubs.FOREIGN` interface has been extended with `returning` and `@->`, and some new types.  
  See the pull request for details:  
    https://github.com/ocamllabs/ocaml-ctypes/pull/389

  NB: code that generates bindings using `Cstubs` may need to be updated to select `return` and `@->` from the bindings functor argument rather than from the `Ctypes` module.  Code that needs to be updated will fail to compile with the new interface.  The pull request shows how to update your code, if necessary.

* The `Cstubs` module can now generate asynchronous bindings to C functions using the Lwt jobs framework.  
  See the pull request for details:  
    https://github.com/ocamllabs/ocaml-ctypes/pull/391

* The `Cstubs` module now supports optionally returning `errno` alongside the return value of bound C functions.  
  See the pull request for details:  
    https://github.com/ocamllabs/ocaml-ctypes/pull/392

* Cross-compilation support is improved: the configuration step no longer runs binaries on the host.  
  See the pull request for details:  
    https://github.com/ocamllabs/ocaml-ctypes/pull/383

* The `Unsigned.S` interface has new `of_int64` and `to_int64` functions.

### Compatibility

* The deprecated `*:*` and `+:+` functions have been removed.  Use `Ctypes.field` instead.

* OCaml 4.00.* is no longer supported.  The earliest supported OCaml release is 4.01.0

Thanks to Spiros Eliopoulos (@seliopou), @orbitz, Leonid Rozenberg (@rleonid) and Peter Zotov (@whitequark) for contributions to this release.

## ctypes 0.5.1

### Bug fixes

* Use a C function, not `Pervasives.ignore`, to keep values alive.

## ctypes 0.5.0

Thanks to Andreas Hauptmann (@fdopen), David Sheets (@dsheets), Etienne Millon (@emillon), Goswin von Brederlow (@mrvn), Leonid Rozenberg (@rleonid), @orbitz, Max Mouratov (@cakeplus), and Peter Zotov (@whitequark) for contributions to this release.

### Features

* Build and install `*.cmt` and `*.cmti` files.

* Expose `time_t` as an unsigned value

* Expose larger interfaces for POSIX types known to be integer types.

* Add support for 1- and 2-byte unsigned integer typedefs.

* Add support for 1-byte and 2-byte integer typedefs.

* Add a `Signed.Int` module.

* Expose more information in the `Uncoercible` exception.

* `allocate_n` now defaults to zeroing its memory.

* Add public root management interface.

  NB: the interface is experimental and subject to change.

* Look through views to add fields to structs and unions.

* Support signed arithmetic operations for `ssize_t`.

* Add support for `ptrdiff_t` as a typedef for a signed integer type.

* Support `intptr_t` and `uintptr_t` as typedefs

* Support coercions between object and function pointers.

* Add public `funptr_of_raw_address` function.

* Support `static_funptr` coercions

* Add function pointers to the core type language

  (See the `Ctypes_static.static_funptr` type, on which
  `Foreign.funptr` and `Foreign.foreign` are now based.)

* Better support for functions returning void with inverted stubs.

* Add support for releasing runtime lock to Cstubs_inverted.

### Bug fixes

* Fix: inconsistent use of `caml_stat_*` functions

* Fix: a memory leak in `ctypes_caml_roots_release`

## ctypes 0.4.2

* Fix a bug involving access to local roots while the runtime lock was not held.

## ctypes 0.4.1

Thanks to Etienne Millon (@emillon) for contributing to this release.

* Fix placement of docstring titles
* Add funptr's optional arguments to funptr_opt
* Fix a typo in libffi detection code
* Synchronize foreign.mli files (documentation)

## ctypes 0.4

Thanks to A. Hauptmann (@fdopen), David Sheets (@dsheets), Maverick Woo (@maverickwoo), Peter Zotov (@whitequark), David Kaloper (@pqwy), Ramkumar Ramachandra (@artagnon), Thomas Braibant (@braibant), Hugo Heuzard (@hhugo) and Edwin Török (@edwintorok) for contributions to this release.

### Major features

* Support for the C99 bool type

* Typedef support

* Enum support

* Support for accessing C globals with foreign_value in generated stubs

* Support for retrieving #define and enum constants from C

* Windows support

  There is now support for Windows (32-bit and 64-bit, using MinGW) and automatic building and testing on Windows using [Appveyor][appveyor-builds]. 

* Support for releasing the runtime lock in C calls

  The new `release_runtime_lock` argument to `Foreign.foreign` indicates whether the OCaml runtime lock should be released during the call to the bound C function, allowing other threads to run.

* Support for acquiring the runtime lock in callbacks

  There is a  new `runtime_lock` argument to `Foreign.funptr`.  Setting `runtime_lock` to `true` indicates that the OCaml runtime lock should be acquired during calls from C to OCaml and released during calls through function pointers from OCaml to C.

* Support for passing 'bytes' values directly to C

  See the [relevant section of the FAQ][strings_faq].

* Add support for custom printers in views.

* Optionally obtain struct and union layout from C

#### Other changes
* string_opt wraps char *, not void *.
* Remove some poorly-supported POSIX types
* Use nativeint to represent pointers
* Support zero-argument callbacks
* findlib package naming: ctypes.foreign-base ~> ctypes.foreign.base &c.
* Make it possible to print a field name
* Better exception handling when using RTLD_NOLOAD
* RTLD_LOCAL support
* Changed the #include path to $(ocamlfind query ctypes)
* Renamed some internal modules to avoid name clashes

[appveyor-builds]: https://ci.appveyor.com/project/yallop/ocaml-ctypes/branch/master

## ctypes 0.3.4

#### Bug fixes

Thanks to Yakov Zaytsev (@ysz) for contributing to this release.

* fix printing for nullary function stubs

## ctypes 0.3.3

#### Bug fixes

* respect `pbyte_offset` with cstubs

## ctypes 0.3.2

* Add bytes to the META "requires" field

## ctypes 0.3.1

#### New features

* Support for 'bytes'

#### Bug fixes

* Avoid invalidated pointer access

## ctypes 0.3

Thanks to Peter Zotov (@whitequark), David Sheets (@dsheets), Mike McClurg (@mcclurmc) and Anil Madhavapeddy (@avsm) for contributions to this release.

#### Major features

##### Support for passing OCaml strings directly to C
(Patch by Peter Zotov.)

The implications are discussed [in the FAQ][strings_faq].

[strings_faq]: https://github.com/ocamllabs/ocaml-ctypes/wiki/FAQ#strings

##### Support for generating C stubs from names and type declarations.
There are various examples available of packages which use stub support: see the [fts example][fts-example] in the distribution (which uses a custom Makefile), [this fork of async_ssl][async_ssl] (which uses OCamlMakefile), and [the cstubs branch of ocaml-lz4][ocaml-lz4] (which uses oasis and ocamlbuild).

[fts-example]: https://github.com/ocamllabs/ocaml-ctypes/tree/master/examples/fts/stub-generation
[async_ssl]: https://github.com/yallop/async_ssl/tree/stub-generation
[ocaml-lz4]: https://github.com/whitequark/ocaml-lz4/tree/cstubs

##### Support for turning OCaml modules into C libraries.
See the [ocaml-ctypes-inverted-stubs-example][inverted-stubs-example] repository for a sample project which exposes a part of [Xmlm][xmlm]'s API to C.

[inverted-stubs-example]: https://github.com/yallop/ocaml-ctypes-inverted-stubs-example/ 
[xmlm]: http://erratique.ch/software/xmlm

#### Other changes

* Add a function [`string_from_ptr`][string_from_ptr] for creating a string from an address and length.
* Generate [codes for libffi ABI specifications][libffi_abi].
* Support for passing complex numbers to C using the stub generation backend.
* Add [`raw_address_of_ptr`][raw_address_of_ptr], an inverse of [`ptr_of_raw_address`][ptr_of_raw_address].
* Add a function [`typ_of_bigarray_kind`][typ_of_bigarray_kind] for converting `Bigarray.kind` values to `Ctypes.typ` values.
* Improved [coercion][coercion] support

[typ_of_bigarray_kind]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALtyp_of_bigarray_kind
[string_from_ptr]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALstring_from_ptr
[raw_address_of_ptr]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALraw_address_of_ptr
[ptr_of_raw_address]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALptr_of_raw_address
[CArray]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.Array.html
[libffi_abi]: http://ocamllabs.github.io/ocaml-ctypes/Libffi_abi.html
[coercion]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALcoerce

#### Backwards incompatibilities

* `Array` has been renamed to [`CArray`][CArray].

## ctypes 0.2.3

#### Bug fixes

* Fix GC-related bug that shows up on OS X.

## ctypes 0.2.2

* Don't install ctypes-foreign cmx files.

## ctypes 0.2.1

* Bump META version

## ctypes 0.2

Thanks to Ivan Gotovchits, Greg Perkins, Daniel Bünzli, Rob Hoes and Anil Madhavapeddy for contributions to this release.

#### Major features

##### Bigarray support.
See [Bigarray types][bigarray-types] and [Bigarray values][bigarray-values] for details.

[bigarray-types]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#4_Bigarraytypes
[bigarray-values]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#4_Bigarrayvalues

##### Give the user control over the lifetime of closures passed to C.
See [the FAQ][faq-lifetime] for details.

[faq-lifetime]: https://github.com/ocamllabs/ocaml-ctypes/wiki/FAQ#function-lifetime

##### Top level printing for C values and types
Loading the new findlib package `ctypes.top` in the toplevel will install custom printers for C types and values.

#### Other changes

* Basic [coercion][coercion] support
* Remove `returning_checking_errno`; pass a flag to [`foreign`][foreign] instead.
* Add an optional argument to [`Foreign.foreign`][foreign] that ignores absent symbols.
  (Patch by Daniel Bünzli.)
* More precise tests for whether types are 'passable'
* Compulsory names for structure and union fields (`*:*` and `+:+` are deprecated, but still supported for now.)
* [`UInt32.of_int32`][of_int32], [`UInt32.to_int32`][to_int32], [`UInt64.of_int64`][of_int64], and [`UInt64.to_int64`][to_int64] functions.
* Finalisers for ctypes-allocated memory.
* Add a [`string_opt`][string_opt] view
  (Patch by Rob Hoes.)
* Add the ['camlint'][camlint] basic type.
* Complex number support
* Abstract types [now have names][abstract].

[foreign]: http://ocamllabs.github.io/ocaml-ctypes/Foreign.html#VALforeign
[of_int32]: http://ocamllabs.github.io/ocaml-ctypes/Unsigned.Uint32.html#VALof_int32
[to_int32]: http://ocamllabs.github.io/ocaml-ctypes/Unsigned.Uint32.html#VALto_int32
[of_int64]: http://ocamllabs.github.io/ocaml-ctypes/Unsigned.Uint64.html#VALof_int64
[to_int64]: http://ocamllabs.github.io/ocaml-ctypes/Unsigned.Uint64.html#VALto_int64
[string_opt]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALstring_opt
[camlint]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALcamlint
[abstract]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALabstract
[coercion]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALcoerce

## ctypes 0.1.1

#### Bug fixes

* Remove hard-coded alloc size

## ctypes 0.1

initial release
