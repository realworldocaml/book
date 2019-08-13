v1.1.0 2017-06-24 London
------------------------

* Add `Mtime.Span.{add,zero,one,min_span,max_span}`.

v1.0.0 2017-05-09 La Forclaz (VS)
---------------------------------

This is a major breaking release with a new API. Thanks to David
Sheets for contributions and discussions. The API was changed to
mirror and follow the conventions and design of `Ptime`. The `Mtime`
module now only provides platform independent datatypes for supporting
monotonic clock readings. Platform dependent access to monotonic
clocks is provided by the `Mtime_clock` modules. The `Mtime.t` type
was added for monotonic timestamps.

* Rename packages `mtime.{jsoo,os}` to `mtime.{clock.jsoo,clock.os}`
  which implement the new `Mtime_clock` interface. The `mtime` package
  has the platform independent support.
* Remove `Mtime.available`, `Mtime_clock` functions now raise `Sys_error`
  on unsupported platforms or errors.
* Add a raw interface to `Mtime_clock` which statisfies MirageOS's monotonic
  clock signature.
* Move `Mtime.{elapsed,counter,count}` to
  `Mtime_clock.{elapsed,counter,count}`.
* Add `Mtime.t` a type to represent system-relative monotonic
  timestamps and related functions. Thanks to David Sheets for the
  patch and his patience.
* Add the `Mtime.Span` module for functions on monotonic time
  spans. Most of the previous platform independent support is now
  provided by this module. See below.
* Move `Mtime.to_ns_uint64` to `Mtime.Span.to_uint64_ns`.
* Move other `Mtime.to_*` to `Mtime.Span.to_*`.
* Move `Mtime.pp_span[_s]` to `Mtime.Span.pp[_float__s]`.
* Add `Mtime.Span.{compare,equal}`. Thanks to David Sheets for the patch.
* Add `Mtime.Span.of_uint64_ns`. Thanks to David Sheets for the patch.

v0.8.4 2017-02-05 La Forclaz (VS)
---------------------------------

* Fix package for -custom linking. Thanks to @orbitz for the report.
* Build depend on topkg.
* Relicense from BSD3 to ISC.

v0.8.3 2015-12-22 Cambridge (UK)
--------------------------------

* Fix Linux bytecode builds. Thanks to Edwin Török for the report.
* Really make js_of_ocaml an optional dependency.


v0.8.2 2015-05-17 La Forclaz (VS)
---------------------------------

* Simpler toploop support (internal change).
* Improve Linux build support by recording link flags against librt in
  the cma and cmxa (this seems to be needed in certain distributions).
  Thanks to David Scott for the report and the fix.


v0.8.1 2015-03-23 La Forclaz (VS)
---------------------------------

* Fix broken arithmetic on 32-bit platform with POSIX clocks. Thanks to
  Stephen Dolan for the report and the fix.


v0.8.0 2015-03-19 La Forclaz (VS)
---------------------------------

First release.
