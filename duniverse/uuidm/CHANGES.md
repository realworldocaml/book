v0.9.7 2019-03-08 La Forclaz (VS)
---------------------------------

- Add `Uuidm.v4`, creates random based V4 UUID using client provided
  random bytes (#8). Thanks to François-René Rideau for suggesting and
  David Kaloper Meršinjak for additional comments.
- Add `Uuidm.{to,of}_mixed_endian_bytes`. Support for UEFI and
  Microsoft's binary serialization of UUIDs.

v0.9.6 2016-08-12 Zagreb
------------------------

- Safe-string support. Thanks to Josh Allmann for the help.
- Deprecate `Uuidm.create` in favor of `Uuidm.v`.
- Deprecate `Uuidm.print` in favor of `Uuidm.pp_string`
- Add `Uuidm.pp`.
- Relicensed from BSD3 to ISC.
- Build depend on topkg.
- `uuidtrip` uses `Cmdliner` which becomes an optional dependency of
  the package. The command line interface is unchanged except for long
  options which have to be written with a double dash. Binary output
  no longer adds an ending newline.

v0.9.5 2012-08-05 Lausanne
--------------------------

- OASIS 0.3.0 support.


v0.9.4 2012-03-15 La Forclaz (VS)
---------------------------------

- OASIS support.
- New functions `Uuidm.v3` and `Uuidm.v5` that generate directly these 
  kinds of UUIDs.
- New function `Uuidm.v4_gen` returns a function that generates
  version 4 UUIDs with a client provided random state. Thanks to Lauri
  Alanko for suggesting that `Random.make_self_init` may be too weak
  for certain usages.


v0.9.3 2008-08-01 Lausanne
--------------------------

- POSIX compliant build shell script.


v0.9.2 2008-07-30 Lausanne 
--------------------------

- Support for debian packaging. Thanks to Sylvain Le Gall.


v0.9.1 2008-06-18 Lausanne
--------------------------

- Minor internal cleanings.


v0.9.0 2008-06-11 Lausanne
--------------------------

- First release.
