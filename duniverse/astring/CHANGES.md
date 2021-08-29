v0.8.5 2020-08-08 Zagreb
------------------------

- Support OCaml 4.12 injectiviy annotation of Map.S (#18).
  Thanks to Jeremy Yallop for the patch. 
  
v0.8.4 2020-06-18 Zagreb
------------------------

- Handle `Pervasives`'s deprecation.
- Require OCaml 4.05
- Add conversions to/from Stdlib sets and maps. Thanks 
  to Hezekiah M. Carty for the patch.

v0.8.3 2016-09-12 Zagreb
------------------------

- Fix potential segfault on 32-bit platforms due to overflow in
  `String[.Sub].concat`. Spotted by Jeremy Yallop in the standard
  library. The same bug was present in Astring.

v0.8.2 2016-08-26 Zagreb
------------------------

- Fix `String.Set.pp` not using the `sep` argument.
- Build depend on topkg.
- Relicense from BSD3 to ISC.

v0.8.1 2015-02-22 La Forclaz (VS)
---------------------------------

- Fix a bug in `String.Sub.span`.

v0.8.0 2015-12-14 Cambridge (UK)
--------------------------------

First release.
