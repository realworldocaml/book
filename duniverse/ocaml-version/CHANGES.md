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
