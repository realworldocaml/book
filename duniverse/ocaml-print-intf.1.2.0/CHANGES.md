# v1.2.0 (2020-03-31)

- Fix a bug with `.ml` input files where `ocaml-print-intf` was unable to figure out
  the dune project root if the path was too long. (#3, @NathanReb)

# v1.1.0 (2020-03-26)

- Add support for passing `.ml` input files as a shortcut for building the `.cmi`
  (using dune) and calling `ocaml-print-intf` on the resulting file. (#2, @NathanReb)

# v1.0.0 (2020-03-17)

Initial public release.
