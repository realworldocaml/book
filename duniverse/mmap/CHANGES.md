## v1.2.0 (2022-02-24)

- Use `bigarray-compat` instead of `bigarray` library which does not exists on
  OCaml 5.0 (@dinosaure, @kit-ty-kate, @anmonteiro, #7, #8)

## v1.1.0 (2019-04-04)

- Add Windows support by making `mmap` just a shim over
  the underlying `map_file` implementation in OCaml.
  (#3 @aantron)
- Add gitattributes for Windows (#2 @dra27)

## v1.0.2 (2019-03-12)

- Fix compatibility with older version of OCaml (#1 @diml)

## v1.0.1 (2019-06-10)

- Fix opam doc field so dune-release works (@avsm)
- Improve ocamldoc (@avsm)

## v1.0 2019-03-06

- Initial release. (@diml)
