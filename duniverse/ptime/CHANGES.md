v1.0.0 2022-02-16 La Forclaz
----------------------------

* Change the `js_of_ocaml` strategy for `Ptime_clock`'s JavaScript
  implementation. Primitives of `ptime.clock.os` are now implemented
  in pure JavaScript and linked by `js_of_ocaml`. This means that the
  `ptime.clock.jsoo` library no longer exists, simply link against
  `ptime.clock.os` instead. Thanks to Hugo Heuzard for suggesting and
  implementing this.

* Require OCaml >= 4.08
* Correct a potential overflow in Ptime.Span.of_float_s (#26). 

v0.8.6 2021-11-28 Zagreb
------------------------

* Require OCaml >= 4.03
* Drop dependency on `result` compatibility package.
* Alter install structure. `ptime/{os,jsoo}` are now installed in
  `ptime/clock/{os,jsoo}`. Also a `ptime_clock.cm[t]i` is now
  installed in `ptime/clock/`. The `ocamlfind` packages are unchanged
  except for `ptime.clock.os.top` which no longer exists.
* Handle `Pervasives` deprecation.
* Fix `Ptime.truncate` to always truncate down. Thanks to David
  Kaloper Meršinjak for the report & fix.
* Allow compiling with MSVC compiler. Thanks to Jonah Beckford for the
  patch.

v0.8.5 2019-05-02 La Forclaz (VS)
---------------------------------

* Make the package compatible with `js_of_ocaml` 3.3.0's
  namespacing

v0.8.4 2018-07-26 Zagreb
------------------------

* `Ptime_clock`: Windows support. Thanks to IndiscriminateCoding
  and David Allsopp for the contribution.
* Fix `Ptime.frac_s` on pre-epoch time stamps. The function computed a
  span of `1s - f` instead of `f` on these.  This function is not used
  internally so this only affects users of this function that apply it
  on pre-epoch time stamps (#12). Thanks to David Kaloper Meršinjak
  for the report.

v0.8.3 2017-02-05 La Forclaz (VS)
---------------------------------

* Fix package for -custom linking.

v0.8.2 2016-07-22 Zagreb
------------------------

* Add `?tz_offset_s` optional argument to `Ptime.weekday`. Thanks
  to Maxence Guesdon for suggesting.

v0.8.1 2015-07-14 Cambridge (UK)
--------------------------------

* Add `Ptime.v` and `Ptime.Span.v` to safely deal with trusted
  inputs. Thanks to Matt Gray for suggesting.
* Add `Ptime.weekday`, to help conversions to denormalized
  timestamp formats. Thanks to Romain Calascibetta for suggesting.
* Build depend on topkg.
* Relicense from BSD3 to ISC.

v0.8.0 2015-12-24 Cambridge (UK)
--------------------------------

First release. Thanks to Raphaël Proust for lodging support.
