## v3.5.0 (2022-07-08)

 * Add 4.14 entry (@dra27 #45)
 * Add 5.0 entry (@dra27 @kit-ty-kate #44)
 * Add oldest versions of OCaml that support arm64, ppc64le and s390x (@tmcgilchrist #47)
 * Add 5.1.0 trunk development version (@dra27 #48)
 * Add riscv64 for 4.11 onwards (@mtelvers @dra27 #49)

## v3.4.0 (2021-10-05)

* Add 4.13.1 entry (@Octachron #42)

## v3.3.0 (2021-09-24)

* Add 4.13.0 and 4.12.1 entries (@Octachron)

## v3.2.0 (2021-07-27)

* Add support for S390x big-endian architectures (@avsm)
* Add 4.14.0 entry (@avsm)
* Add support for naked pointers checker option and add it to
  the 4.12+ variants (@kit-ty-kate @dra27 @avsm)
* Add a domains and effects variants for the experimental
  forks in 4.10 and 4.12, to aid in CI. (@avsm @ewanmellor)
* Do not advertise a 4.10 multicore as 4.12 is the preferred
  compiler now for that (@avsm)

## v3.1.0 (2021-02-25)

* Add OCaml 4.11.2 and 4.12.0 (@smorimoto @kit-ty-kate)
* Add OCaml 4.10.2 and 4.11.1 release. (@avsm)
* Add `Since.options_packages` and update for new opam-repository
  layout for 4.12+. (@dra27 @avsm)

## v3.0.0 (2020-08-23)

* Update for release of 4.11.0 and 4.10.1.

Interface changes:
* Change signature of `Configure_options.to_configure_flag` to
  take an OCaml version, and add support for post-autoconf
  flags in OCaml 4.08+ (#13 @avsm).
* Add some extra configure options for modern OCaml (@avsm).
* Add a `trunk_variants` that has additional tests that a
  full OCaml test run can use (like disable-flat-float-array).
* Add comparison and equality functions to `Configure_options`.
* Remove dependency on Result compatibility module and use
  Stdlib, which bumps up the minimum OCaml version to 4.07.0 (@avsm)
* Add conversion functions to go from Docker and opam
  representations of architecture strings (@avsm)

Base images:
* Remove safe-string variants from the compiler build images,
  as the era of safe string migration is behind us.
* Add in no-naked-pointers, disable-float-array to compiler
  trunk variants, as they need to be actively tested.

## v2.6.1 (2020-07-15)

* Fix `arch_of_string` to work with i386 strings (@avsm).

## v2.6.0 (2020-07-14)

* Mark trunk as 4.12.0 and add the 4.11 as a development beta (@avsm).
* Add i386 architecture (@dinosaure @avsm)
* Add a `arch_is_32bit` to determine if wordsize is 32-bits. (@avsm)

## v2.5.0 (2020-03-22)

* Add OCaml 4.09.1 and 4.10.0 releases (@avsm @kit-ty-kate).
* Add `equal` function to test versions for equality (#7 @kit-ty-kate)
* Add a top-level printer hint for `Ocaml_version.t` (@avsm)

## v2.4.0 (2020-02-20)

* Add `unreleased_betas` list to enumerate the latest release-candidate
  version for each unreleased minor OCaml series (#4 @craigfe).
* Do not `open Result` to clean up internal code (#5 @avsm).

## v2.3.0 (2019-08-29)

* Support OCaml 4.02.3, which brings back a dependency on
  the `result` library.

## v2.2.0

* Add OCaml 4.08.1 release

## v2.1.0 (2019-06-26)
* Add OCaml 4.08.0 release
* Add support for 4.10 as the new trunk.

## v2.0.0 (2019-02-06)

* Reinstate OCaml 4.02 to the "recent" list after a request
  from @dra27.  This means that all the supported OCaml compilers
  for Dune will be present in a single container.
* Add support for 4.09 now that 4.08 has branched.
* Use `+trunk` in the suffix for opam dev version packages.
* Add `Releases.is_dev` to make it easier to spot a dev release.

## v1.0.0

* Add ARM32 (aarch32, arm32v7) architecture.
* Add more OCaml 4.07.[0,1] functions and mark it as latest stable.
* Port to Dune from Jbuilder.
* Add several modules related to compiler configuration, in order
  to faciliate mechanical generation of opam2 compiler packages.
* Drop support for opam 1.2.x in favour of opam 2.0.0.
* Update opam metadata to 2.0 format.

## v0.4.0

* Add 4.07.0 release information.

## v0.3.0 (07/06/2018)

* Add PowerPC (ppc64le) architecture.

## v0.2.0 (22/05/2018)

* Add OCaml 4.06.1 release.
* Add metadata for OCaml 4.08 development.
* Add an Opam.V2 module for opam 2.0 package names for compilers.
* Add a `without_patch` function to remove patch information.
* Add `with_patch` function to manipulate patch information.

## v0.1.0 (24/12/2017)

* Initial public release.
