v6.1.1 2022-07-24
-----------------

- ppx_cstruct: remove superfluous stdlib-shims dependency (@hannesm #300)

v6.1.0 2022-03-29
-----------------

**breaking changes**
- The deprecated functions `Cstruct.len`, `Cstruct.add_len`, `Cstruct.set_len`,
  and `Cstruct.blit_to_string` have been removed (@hannesm #291)

- Implement host_endian (@haesbaert #292, fixes #72)
- Compatibility with OCaml 5.0.0 (@dinosaure #294)
- Drop support of OCaml < 4.08.0, remove bigarray-compat dependency
  (@hannesm #298)
- Fix year in changes of 6.0.1 (@reynir #297)

v6.0.1 2021-06-25
-----------------

**breaking changes**
- `Cstruct.len` is deprecated, it will be deleted at the next
  release. You should use `Cstruct.length` instead.
  (@dinosaure, @hannesm, #284)

- Remove color from ppx's binary to replicate diff of errors in
  any contexts (@dinosaure, @sternenseemann, #285)
- Add `shiftv` (@talex5, @avsm, @dinosaure, #287)
- Use `Bytes_val` if available (@hannesm, @avsm, @dinosaure, #286)

v6.0.0 2020-09-10
-----------------

**breaking changes**
- Add the function `Cstruct.length`, `Cstruct.len` will
  be deprecated to next release (@dinosaure, @hannesm, @avsm, #279)
- `Cstruct.to_string` requires 2 optional arguments `?off` and `?len`
  (@hannesm, @cfcs, @talex5, @dinosaure, #262)

- Use `ppxlib` instead `ppx_tools_versioned`
  (@pveber, @avsm, @dinosaure, #280)
- Delete the support of old version of OCaml (< 4.07.0) for `ppx_cstruct`
  (@avsm, @dinosaure, #280)
- Add useful functions to be able to parse some contents
  with `Cstruct.t` like the `astring` library (#227,
  @dinosaure, @avsm, @samoht, @hannesm).
- Improve the interface of `Cstruct.t` with capabilities
  (@hannesm, @cfcs, @talex5, @dinosaure, #262)

v5.2.0 2020-06-10
-----------------

Upgrade the `ppx_cstruct` library to use the OCaml 4.11
AST rather than than OCaml 4.04, which in turn should
make it easier to port to ppxlib in the future and
improve interoperability with other PPXs (@bikallem #275).

Also upgrades build files to use dune 2.0 (@bikallem #275)
and fixes the GitHub Actions versions (@smorimoto #273)
and also test OCaml 4.10.0 (@avsm).

v5.1.1 2019-11-23
-----------------

This is a point release to fix a regression in 5.1.0
on compilers earlier than 4.07.

The release also fixes the test suite and CI on compilers
greater than 4.07.

v5.1.0 2019-11-23
-----------------

- Do not issue deprecation warnings when using OCaml 4.08.0
  and cstruct-ppx with enums due to `Pervasives` (#269 @cypheon @hannesm)

- Tighten parsing of the `[@len]` attribute to ensure it is a
  valid, positive integer (#265 @emillon)

- Update JavaScript bindings to latest `Js_of_ocaml` 3.5.0
  interfaces (@hhugo #268)

v5.0.0 2019-04-19
-----------------

**Security**: This release tightens bounds checks to ensure
that data outside a given view (but still inside the underlying
buffer) cannot be accessed.

- `sub` does more checks (#244 #245 @hannesm @talex5 review by @dinosaure)
- `add_len` and `set_len` are now deprecated and will be removed
  in a future release. (#251 @hannesm)
- do not add user-provided data for bounds checks 
  (#253 @hannesm, report and review by @talex5)
- improve CI to add fuzzing (#255 #252 @avsm @yomimono @talex5)

**Remove Unix dependency**: cstruct now uses the new `bigarray-compat`
library instead of Bigarray directly, to avoid a dependency on Unix
when using OCaml compilers less than 4.06.0.  This will break downstream
libraries that do not have a direct dependency on `Bigarray`.  Simply
fix it in your library by adding a `bigarray` dependency in your dune
file. (#247 @TheLortex)

**Capability module**: To improve the safety of future code with stronger type
checking, this release introduces a new `Cstruct_cap` module which makes the
underlying Cstruct an abstract type instead of a record. In return for this
extra abstraction, the module can enforce read-only, write only, and read/write
buffers by tracking them as phantom type variables.  Although this library
shares an implementation internally with classic `Cstruct`, it is a significant
revision and so we will be gradually migrating to it.  Feedback on it is
welcome! (#237 @dinosaure and many excited reviewers)

**Ppx compare functions**: A new `compare_X` function is generated for
`cenum` declarations. This respects custom ids supplied in the cenum
declaration and so is more robust than polymorphic compare (#248 @emillon)

The CI has also been switched over to both Azure Pipelines and Drone in
addition to Travis, and as a result the tests all run on Windows, macOS,
various Linux distributions, on x86 and arm64 machines, and runs AFL
fuzz tests on the Drone cloud (#255 @avsm).

v4.0.0 2019-03-25
-----------------

- Sexplib is now an optional library for the base `Cstruct` module.
  A new `Cstruct_sexp` module has been introduced with the serialiser
  functions, contained within the `cstruct-sexp` opam package.

  To convert old code, simply use `Cstruct_sexp.t` instead of
  `Cstruct.t` in a record type for which you are using `[@@deriving sexp]`.
  This is a type alias to `Cstruct.t` but also has the right
  sexp-conversion functions in scope.  There is an example of this
  in the `ppx_test/with-sexp` directory in the source repo.

  When you have converted and released your library, add an
  opam constraint of `cstruct {>="4.0.0"}` to your own opam
  packages to ensure that they pick up this version of the library.
  (fixes #222, @avsm)

- JavaScript stubs are now installed using the jsoo mechanism
  rather than a manual specification (#241 @jonludlam)

- Use computed versions in opam files to ensure that dependent
  opam packages such as cstruct-async get the same base version
  of cstruct to avoid mismatches. (@avsm)

- Add a ppx test suite to checks that all error paths in ppx
  handling are reachable and have meaningful location info (#238 @emillon)

v3.7.0 2019-03-10
-----------------

- Improve performance by not doing redundant bounds checks
  in both the Bigarray and Cstruct level (#236 @Reperator @chambart)
- Ignore fields starting wih `_` by skipping code generation
  but still respecting the space usage of that field. This was
  a convention before but is now enforced by the code generator
  to save space in the output (#233 @emillon)
- More warnings suppression for sizeof and enums (#231 @emillon)

v3.6.0 2019-03-01
-----------------

- A `[%%cstruct type ...]` declaration generates many values that
  are potentially unused. The code generator in `ppx_cstruct` now
  guarantees that there will be no more "unused value" (warning 32)
  statements from use of the ppx form. (#228 @emillon)
- Actually run the ppx tests instead of just building them.
  (#227 @emillon to fix #226 from @XVilka)


v3.5.0 2019-02-26
-----------------

- Remove trailing spaces in hexdump output (#219 @emillon)
- Add `Cstruct.rev` to allocate a reversed cstruct (#221 @emillon)
- `Cstruct_unix` now uses the post-OCaml 4.06 `Unix.map_file`
  instead of the deprecated Bigarray `map_file` that was removed
  in OCaml 4.08 (@avsm, see ocaml/ocaml#2263)
- Remove unnecessary `(wrapped false)` in the build system (@avsm)
- Correct ocamldoc to the right `cstruct-ppx` package pointer (@avsm)

v3.4.0 2019-02-02
-----------------

- Remove old compatibility packages for `cstruct.lwt`, `cstruct.async`,
  `cstruct.ppx` and `cstruct.unix`.  These were deprecated in
  cstruct.3.0.0 in favour of counter part libraries with a dash
  in the name (`cstruct-lwt`, `cstruct-async`, `cstruct.unix`)
  or `ppx_cstruct` for the PPX extension. (@avsm)

v3.3.0 2019-01-22
-----------------

- Support for bi-endian cstructs that generate both sets of accessor
  functions (#212 by @XVilka). This generates both `BE` and `LE`
  modules to use as needed.

- Modify `of_string` and `of_bytes` to have an option `?off`
  offset argument into the source bytes. (#208 by @XVilka)

- Improve tests to work on 32-bit architectures and handle
  Gc better (@samoht)

- Do not depend explicitly on deprecated `ppx_driver`. This was
  primarily there for older compilers, and new uses should be
  based around `ppxlib`. (#201 by @edwintorok).

- Upgrade opam metadata to 2.0 format. (#217 by @XVilka @avsm)

- Upgrade to dune from jbuilder and support dune-release instead
  of topkg (@avsm)

v3.2.1 2017-12-13
-----------------

- improve performance by using primitives instead of C stubs.  the performance
  regression was introduced in #177 in 3.2.0 (#195 by @pqwy)

v3.2.0 2017-11-17
-----------------

- wrap `hexdump_pp` output in a box (#175 by @cfcs)
- remove dependency on `ocplib-endian` (#177 by @hannesm)
- add `of_hex: string -> t` (#179 by @hannesm and @pqwy)
- add `to_bytes: t -> Bytes.t` (#183 by @hannesm)
- add `empty: t` (#184 by @hannesm)
- sub: check the bounds more carefully for overflow (#185 by @hannesm)
- cstruct-unix: fix the build on OCaml 4.06.0 (#187 by @djs55)
- travis: test OCaml 4.04.2 and 4.06.0 (#186 by @hannesm)

v3.1.1 2017-07-13
-----------------

- `check_alignment` now treats a large alignment as an unsigned value
  and so doesnt raise a signal (#171 by @yallop)
- Improve Windows support by avoiding `void *` pointer arithmetic and
  have more portable headers in the C stubs (#170 by @fdopen)

v3.1.0 2017-07-12
-----------------

- Fix arithmetic overflow in `Cstruct.lenv` and `copyv` (#159 by @yallop)
- Reject negative destination offsets in `blit` (#160 by @yallop)
- Add AFL fuzz tests using Crowbar, which independently discovered
  #160 and also an overflow in `of_bigarray` and `sub`, now bith
  fixed (#164 by @talex5)
- Improve performance of several allocation functions by eliminating an
  unnecessary buffer zero step (#158 by @hannesm)
- Compile the source tree with stricter flags, including dead variable
  detection and deprecation warnings (#157 by @samoht)
- Bump the required minimum OCaml version up to 4.03.0 (due to #157).

v3.0.2 2017-06-14
-----------------

- fix the `cstruct-async` package build, and depend on the latest
  Async packages (>="v0.9.0") as part of this. (#152 @jnfoster)

v3.0.1 2017-06-09
-----------------

- ppx: remove an errant standalone initialiser that was messing up
  the composition of cstruct with other `ppx_driver` based ppx converters
  (most notably `ppx_sexp_conv`.  If you are having trouble with using
  `ppx_cstruct` with other drivers, put a constraint on `ppx_cstruct>=3.0.1`.
  (#151 #150 #149 #148 via @djs55 @g2p @avsm @diml).
- ppx: also add a test case for `cstruct` and `lwt` working together, but
  this will not work until a `lwt>3.0` release happens.
- Update opam rules to use `jbuilder subst` for version information
  in the distribution.
- Fix tests so that `check_alignment` expects a negative result.
- Add opam test target for core library.

v3.0.0 2017-05-11
-----------------

- Split up OPAM packages into multiple independent ones. We now
  have a standalone `cstruct`, and then separate `cstruct-lwt`,
  `cstruct-async`, `cstruct-unix` packages, and a `ppx_cstruct`
  package for the syntax extension. Transitional findlib packages
  with the old scheme are available, but now packages should migrate
  to using `cstruct-async` instead of `cstruct.async` for example.
  This has the added benefit of the OPAM package names now matching
  the findlib names. (#138 by @avsm @rgrinberg).

- Port build to [jbuilder](https://github.com/janestreet/jbuilder).
  See the README for local development instructions.

- Ensure that `check_alignment` only takes a non-zero argument
  for alignment (#143 #145 by @cfcs @avsm).

v2.4.1 2017-05-03
-----------------

- fix missing `ppx_tools_versioned` dependency (#136, @let-def)

v2.4.0 2017-03-30
-----------------

Distribute the PPX extension so that it is compatible with Jbuilder.
`ppx_cstruct` is now distributed as both a library and a binary.
Findlib predicates are used to distinguish usage:
 - the binary is used for toplevel and simple -ppx building
 - the library is used for linking custom rewriters
 - `-package cstruct.ppx` alone uses the binary for rewriting.
 - `-package cstruct.ppx` -predicates custom_ppx,ppx_driver" is used to link the rewriter.

To use the PPX extension in jbuilder, just add:

```
  (libraries (cstruct))
  (preprocess (pps (cstruct.ppx)))
```

to your `jbuild` file.  This may be renamed to `ppx_cstruct` in a
future release so that the PPX dependency is decoupled from the main
library, so this `cstruct.ppx` is intended to be transitional as it
is what was originally used.

v2.3.3 2017-03-28
-----------------

* Port ppx extension to use `ocaml-migrate-parsetree` so it should
  also compile on future revisions of OCaml (#127 via @let-def).

v2.3.2 2017-03-03
-----------------

* Add support for OCaml 4.05 for the PPX extension.
* Docs: correct to description of shift function (#121 via @orbifx).

v2.3.1 2016-12-10
-----------------

* Fix a memory leak in the exception printing code (#130 via @djs55)
* Appveyor CI fixes (#130 via @avsm)
* Fix typo in docstring (#117 via @yomimono)
* Fix opam base-unix dependency (#115 via @avsm)

v2.3.0 2016-08-16
-----------------

* Add `Cstruct.of_bytes/to_bytes`. In common with the existing
  implementation, this relies on the representation of bytes and string
  being the same, which is true as of OCaml 4.04 and lower (#105 via @yallop).
* Support OCaml 4.04 (#111 via @gasche).

v2.2.0 2016-06-30
-----------------

* Make `create` zero out the new buffer. The new `create_unsafe`
  function can be used if you want to trade safety for speed.

v2.1.0 2016-05-04
-----------------

* Add `hexdump_pp` that uses the Format module. This works better with the
  Logs library than using `hexdump_to_buffer`, and also makes it easy to
  indent the hexdump (#100 via @talex5).

v2.0.0 2016-04-26
-----------------

* Remove camlp4 extension as it is no longer maintained (#95).
* Add support for OCaml 4.03 in the PPX extension (#96).
* Minimum supported OCaml version for the library is now 4.02.3.
* Fix parsing of high int32 `@@enum` values.
* Move `Cstruct.check_alignment` into the stubs. Before this patch
  we returned the buffer address from C and then calculated using
  OCaml's boxed `Int64.t`. This patch reduces minor allocations by
  performing the calculation in the C stubs. This makes the function
  suitable for use in an assert in a performance sensitive path.

v1.9.0 2016-02-19
-----------------

* Add support for a ppx-based extension that uses the extension point
  support in OCaml 4.02 and higher to generate Cstruct and Cenum
  function definitions.  The new syntax is documented in the README file.

v1.8.0 2016-01-05
-----------------

* Add support for `-safe-string` in OCaml 4.02 upwards.
  The main change is to rename `blit_to_string` to `blit_to_bytes` and
  change its type so that it writes to bytes rather than string
  (#74 by @yallop).
* Remove strong build-time dependency on `camlp4` in the base library.
  The `sexplib` functions were only used in the interface, so replace them
  with manually written ones.  This also enables compatibility with latest
  Core that has switched to ppx.
* Add multi-distro testing via Travis/Docker containers.

v1.7.1 2015-12-15
-----------------

* Correct error output for `LE.get_uint16` on invalid bounds (#75)
* Fix `fillv`. If the source didn't fit in the buffer then we
  skipped the amount we wanted to copy, not the amount actually copied (#77).

v1.7.0 2015-07-11
-----------------

* Add `Cstruct.concat` and `Cstruct.append` (#57, @pqwy)
* Add `js_of_ocaml` stubs (#63, #64, @djs55)

v1.6.0 2015-03-28
-----------------

* Add `memset` to set all the bytes of a cstruct value efficiently (#49)
* More useful `Invalid_argument` parameters (#48).
* Fix `to_sexp` to expose only the current view (#44 from David Kaloper).
* Add `compare` and `equal` (#23, #24 and #45 from David Kaloper).
* Add `fillv` to copy over a list of buffers (from Thomas Leonard).
* Shift to centralised Travis scripts.

v1.5.0 2014-11-24
-----------------

* Make `camlp4` an optional build-time dependency (#35).
* Remove `ounit` as a dependency in the `opam` file.
* Improve `opam` description file for OPAM 1.2 workflow (#36).
* Refresh Merlin IDE description (#37).

v1.4.0 2014-08-10
-----------------

Comprehensive addition of bounds checking to all cstruct operations
(from @pqwy in #33).  The major changes are:
* Disallow negative indexing with all cstruct accessors.
* Disallow negative `sub` and `shift` operations.
* Make sure `of_bigarray` cannot create invalid `cstruct` values.

v1.3.1 2014-07-10
-----------------

* Also bounds test single-byte operations on views (#31 via @pqwy).

v1.3.0 2014-07-04
-----------------

* Add bounds checks for `Cstruct.BE/LE` functions that violate a view.
  Previously, only bounds errors on the underlying buffers would raise.
  Bug #25, reported by Mindy Preston in mirage/mirage-tcpip#56.
* Add 'Lwt_cstruct.complete' to ensure that `read`/`write` operatiosn
  run to completion.
* Add `Sexplib` conversion functions to `Cstruct.t` values (#27 #22).

v1.2.0 2014-06-06
-----------------

Add a `sexp` optional decorator to `cenum` to output the values as s-expressions.
This is compatible with the `sexplib` convention.  The syntax is;

```
cenum foo64 {
  ONE64;
  TWO64;
  THREE64
} as uint64_t(sexp)
```

And `sexp_of_foo64` and `foo64_of_sexp` functions will also be available.
The representation of the Sexp is the string representation of the enum.

v1.1.0 2014-02-19
-----------------

* Improve bounds checks on sub, shift, set_len, add_len.
* Add `to_bigarray` to convert back into a Bigarray slice.

v1.0.1 2013-12-09
-----------------

* Fix Cstruct.shift function

v1.0.0 2013-12-05
-----------------

* Remove IPv4/IPv6 types (now moved to `ocaml-ipaddr`).
* Improved ocamldoc for the interface.
* More conservative bounds checking in the length manipulation functions.
* Build C stubs with `-Wall`.

v0.8.1 2013-11-06
-----------------

* Trailing semicolons are allowed in cstruct field definitions.
* Buffer elements can be any primitive integer, not just `uint8`.

v0.8.0 2013-10-13
-----------------

* Improved ocamldoc for BE/LE modules.
* Add Travis-CI test scripts and fix `test.sh` script compilation.
* Support int32/int64 constant values in cenum like `VAL = 0xffffffffl`, useful for 32-bit hosts.
* Check and raise error in case of negative offsets for blits (#4).
* Correctly preserve the sequence after a constant constructor is set during a `cenum` definition.
* Do not repeat the `sizeof_<field>` binding for every get/set field (should be no externally observable change).
* Add `Cstruct.hexdump_to_buffer` to make spooling hexdump output easier.
* Generate `hexdump_foo` and `hexdump_foo_to_buffer` prettyprinting functions for a `cstruct foo`.

v0.7.1 2013-03-06
-----------------

* Add `Async_cstruct.Pipe` to map pipes of `Cstruct` buffers to strings or `Bigsubstring`.

v0.7.0 2013-02-25
-----------------

* Add zero-copy conversion functions to/from the Core `Bigsubstring`.
* Add an `of_string` function to simplify the construction from OCaml values.
* Add Async interface to interoperate with Jane Street Core code.

v0.6.2 2013-02-08
-----------------

* Add experimental `cstruct.obuild` for the `obuild` build tool.
* Use bounds checked version of all functions in the external interface.
* Expose the `Cstruct.debug` to dump internal state of a buffer to a string.
* Add `set_len` and `add_len` to manipulate the total-length field directly.

v0.6.1 2012-12-20
-----------------

* Add `sendto`, `read` and `recvfrom` functions to the Lwt subpackage.

v0.6.0 2012-12-20
-----------------

* Add fast bigarray<->string functions to replace byte-by-byte copies.
* Add an Lwt sub-package to expose a write call.
* Depend on ocplib-endian for fast low-level parsing of integers.
* Make `Cstruct.t` a record type that doesn't use Bigarray slicing
  to provide views onto buffers. This lets views be allocated directly
  on the minor heap rather than forcing a major heap allocation. It
  does alter the external API, so previous users of cstruct wont work.

v0.5.3 2012-12-16
-----------------

* No functional changes, just OASIS packaging fix to right version.

v0.5.2 2012-12-11
-----------------

* Remove the separate `xen` and `unix` subdirectories, as the
  portable `Bigarray` is now provided by the `xenbigarray` package.

v0.5.1 2012-09-28
-----------------

* Add `string_to_<cenum>` function to match the `<cenum>_to_string`,
  primarily to help with command-line parsing of enum arguments.

v0.5.0 2012-09-20
-----------------

* Add a signature generator for cstruct and cenum to permit their use in `.mli` files.
* Use the more reliable revised syntax camlp4 quotation expander, to avoid
  broken AST output from antiquotations.
* Switch the `xen/` version over to using OASIS also.

v0.4.0 2012-09-02
-----------------

* Fix META file for use with Xen

v0.3 2012-08-25
--------------

* Initial public release
