v1.0.3 2022-02-03
-----------------

- Support for OCaml 5.00, thanks to Kate (@kit-ty-kate) for
  the patch.

v1.0.2 2019-02-05 La Forclaz (VS)
---------------------------------

- Fix the substring folding functionality introduced in v1.0.0.
  It never worked correctly.

v1.0.1 2017-03-07 La Forclaz (VS)
---------------------------------

- OCaml 4.05.0 compatibility (removal of `Uchar.dump`).

v1.0.0 2016-11-23 Zagreb
------------------------

- `Uutf.String.fold_utf_{8,16be,16le}`, allow substring folding via
  optional arguments. Thanks to Raphaël Proust for the idea and the
  patch.
- OCaml standard library `Uchar.t` support.
  - Removes and substitutes `type Uutf.uchar = int` by the (abstract)
    `Uchar.t` type. `Uchar.{of,to}_int` allows to recover the previous
    representation.
  - Removes `Uutf.{is_uchar,cp_to_string,pp_cp}`. `Uchar.{is_valid,dump}`
    can be used instead.
- Safe string support. Manual sources and destinations now work on bytes
  rather than strings.
- Build depend on topkg.
- Relicense from BSD3 to ISC.

v0.9.4 2015-01-23 La Forclaz (VS)
---------------------------------

- Add `Uutf.decoder_byte_count` returning the bytes decoded so far.
- The `utftrip` cli utility now uses `Cmdliner` which becomes an
  optional dependency of the package. The cli interface is not
  compatible with previous versions.

v0.9.3 2013-08-10 Cambridge (UK)
--------------------------------

- Fix wrong decoding sequence when an UTF-8 encoding guess is based on
  a two byte UTF-8 sequence. Thanks to Edwin Török for the report.
- OPAM friendly workflow and drop OASIS support.

v0.9.2 2013-01-04 La Forclaz (VS)
---------------------------------

- utftrip, better tool help.
- Fix `Uutf.is_uchar` always returning false. Thanks to Edwin Török 
  for reporting and providing the fix and test.

v0.9.1 2012-08-05 Lausanne
--------------------------

- OASIS 0.3.0 support.

v0.9.0 2012-05-05 La Forclaz (VS)
---------------------------------

First release.
