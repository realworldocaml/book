
v1.0.1 2016-03-07 La Forclaz (VS)
---------------------------------

- OCaml 4.05.0 compatibility (removal of `Uchar.dump`).

v1.0.0 2016-11-23 Zagreb
------------------------

- Support for RFC 7195/ECMA-404. This means that any JSON value can
  now be codec as JSON text, in RFC 4627 (obsoleted by 7195) this
  could only be an array or an object. If your code was relying on the
  fact the first decoded lexeme was either a `Os` or `As`,
  you will need to review that.
- Fix `Jsonm.decode` not eventually returning `End` on toplevel
  decode error.
- OCaml standard library `Uchar.t` support. At the API level only
  some cases of `Jsonm.error` change.
- Uutf 1.0.0 support.
- Safe string support.
- Build depend on topkg.
- Relicensed from BSD3 to ISC.


v0.9.1 2012-08-05 Lausanne 
--------------------------

- OASIS 0.3.0 support.


v0.9.0 2012-05-05 La Forclaz (VS)
---------------------------------

First release.
