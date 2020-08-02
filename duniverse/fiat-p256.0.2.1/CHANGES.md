## v0.2.1 (2020-05-09)

- use constant-time selection in scalar mult (#51, @emillon)
- use eqaf for compare_be (#46, @cfcs & @emillon)
- add documentation (#48, @emillon)
- document how to generate a secret from known data (#56, @emillon)
- mirage cross-compilation (#57, #58, @hannesm)
- sync opam file with opam-repository (@emillon, #53 @hannesm)
- generate opam file from dune-project (#52, @emillon)
- add version in .ocamlformat (#49, #55, @emillon)
- rename internal modules (#50, @emillon)

## v0.2.0 (2019-07-23)

### Fixed

- Fix a bug in `generate_key` where it would never actually work when used with
  a proper `rng` function (#44, @NathanReb)
- Fix benchmark executable. It is now built (but not executed) as part of tests
  (#45, @emillon)

### Changed

- Use `alcotest` instead of `ppx_expect` for tests (#43, @emillon)

## v0.1.0 (2019-06-28)

- Initial release
