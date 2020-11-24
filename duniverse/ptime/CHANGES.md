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
