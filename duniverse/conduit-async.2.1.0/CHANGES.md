## v2.1.0 (2020-03-14)

* port to tls.0.11.0 interfaces which also uses mirage-crypto (#309 @hannesm)
* do not use deprecated ppx sexplib declarations (#309 @avsm)
* replace Appveyor CI with GitHub Actions (#309 @avsm)

## v2.0.2 (2019-11-02)

* mirage: do not raise exceptions in DNS lookup (#305 @hannesm)
* mirage: adapt to mirage-stack/mirage-time/mirage-flow/mirage-random 2.0.0 interfaces (#306 #hannesm)
* mirage: adapt to dns 4.1.0 changes (#306 @hannesm)
* mirage: adapt to vchan 5.0.0 changes (#306 @hannesm)

## v2.0.1 (2019-08-19)

* lwt-unix: fix compilation with `lwt_ssl` and fix tests to correctly exercise this
  part of the codepath (#304 @avsm).

## v2.0.0 (2019-08-17)

* lwt-unix: obtain client IP correctly when using TLS connections (#277 @victorgomes)
* lwt-unix: replace the dune/ocaml file with a `(select)` build form.
  This avoids invoking `ocamlfind` from the build, and fits in with the
  rest of dune builds much more naturally (@avsm).
* lwt-unix: force callers to give a custom callback `on_exn` in case of exceptions
  to avoid random crashes (#261 @kit-ty-kate)
* mirage: use `dns-client>=4.0.0` which is the `udns` implementation (#290 @hannesm)
* mirage: rename `mirage-conduit` to `conduit-mirage` to fit the naming structure
  of this library suite more.  All new users of Mirage should use `conduit-mirage`,
  and migrating should involve simply swapping the name in your `dune` and `opam`
  files (#302 @hannesm @avsm)
* async: expose `verify_mode` correctly in `Conduit_async` (#298 @brendanlong)

## v1.5.0

* lwt-unix: Do not close file descriptors more than once, which led to a lot of
  log spam due to EBADF (#294 @hcarty @avsm)
* lwt-unix: Always close channels after handling an event (#283 @hcarty)
* Allow TCP to be established from existing file descriptors
  (for example, an inherited systemd socket) (#144 @SGrondin #282 @timbertson)
* async: add `Conduit_async.V3` which provides convenience functions for
  resolving URIs to addresses (#287 @vbmithr)
* `Lwt_ssl`: Enable certification validation (#291 @vouillon)
* `Async_ssl`: fix exception raised when other side disconnects
  due to sharing underlying fd (#295 @bogdan2412)

## v1.4.0

* Use Ipaddr 3.0.0+ interfaces (#284 by @avsm).
* Update opam metadata files to the opam 2.0 format (#284 by @avsm)
* Hook in an introduction ocamldoc page to the `conduit` odoc (#284 by @avsm)

## v1.3.0 (2018-10-19)

* Mark `ppx_sexp_conv` as not just a build dependency.
* Switch build to dune from jbuilder.
* Depend on more precise versions of `mirage-types` instead
  of just the generic package.
* Fix ocamldoc headings to work with latest odoc (level 2 not 1).

## v1.2.0 (2018-08-06)

* Correct depopt for conduit-lwt-unix (#260, @dra27)
* async: provide all `Async_ssl` options at config (#263, @vbmithr)
* async: add a V2 module for a new versioned API (#265, @rgrinberg)
* lwt-unix: do not link with tls.lwt on windows (#267, @samoht)

## v1.1.0 (2018-03-22)

* Implement SNI (Server Name Indication) for SSL backend (#255 by @vouillon)
* Make hostname optional in `Conduit_lwt_unix_ssl.Client.connect` (#255 by @vouillon)
* Fix file descriptor leakage on `EADDRINUSE` for the Lwt backend (#257 by @rixed)

## v1.0.3 (2018-01-06)

* Favour resolving over IPv4 instead of IPv6, if both are available
  and one has to be chosen. (#245 via @rixed)
* Fix some warnings with the dummy `Lwt_unix_ssl` module.
* Add a direct dependency on xenstore for mirage-conduit.
* Support latest Async v0.10.0 interfaces (no more `Async.Std`).

## v1.0.2 (2017-09-13)

* Fix regression with TLS/SSL backend: there is no need to set `CONDUIT_TLS`
  manually when using tls (#234, @hcarty)
* Update to lwt.3.0.0 (#236, #241, @rgrinberg and @samoht)
* Fix regression in linking with the launchd backend (#240, @samoht)

## v1.0.1 (2017-07-25)

* Fix linkage of mirage-conduit with apps, as a `tls.mirage` dependency
  was missing in the mirage-conduit-3.0.0 release (#232 by @samoht)

## v1.0.0 (2017-07-22)

Details on changes: https://discuss.ocaml.org/t/ann-major-releases-of-cohttp-conduit-dns-tcpip/571

Port build to jbuilder, and break up OPAM packages into multiple
independent packages instead of being optional dependencies against
the main `conduit` package. This makes it significantly easier to
depend on precisely the libraries you need, but requires porting
applications to use the new `ocamlfind` and `opam` scheme.

The new package layout is:

- `conduit`: the main `Conduit` module
- `conduit-lwt`: the portable Lwt implementation
- `conduit-lwt-unix`: the Lwt/Unix implementation
- `conduit-async` the Jane Street Async implementation
- `mirage-conduit`: the MirageOS compatible implementation

In each of these packages, the `opam` and `ocamlfind` package
names are now _the same_, so you will need to rename the former
subpackages such as `conduit.async` to `conduit-async`.  The
implementation is otherwise the same, so no other code changes
should be required.

In return for these breaking changes to the packaging, it is
now significantly easier to depend on a particular backend,
also for us to rev the interfaces towards a stable 1.0 release.
Jbuilder also builds the source tree around 4x faster than it
did previously.

There are still some optional dependencies remaining, most
notably the `tls` and `ssl` packages.  If they are present,
then conduit will be compiled with TLS support.

## 0.15.4 (2017-05-31)
* Lwt: Fix meta file and building with lwt_ssl (#222, @dkim)

## 0.15.3 (2017-05-04)
* Lwt: lwt 3.0 support for the tls backend too (#219, @@rgrinberg)

## 0.15.2 (2017-05-02)
* Move cstruct dependency from conduit to mirage-conduit

## 0.15.1 (2017-04-25)
* Lwt: Lwt 3.0 support (#214)
* Async: with_connection (#211)

## 0.15.0 (2017-02-23)
* support MirageOS 3, and drop support for earlier versions (#203, #202)

## 0.14.5 (2017-01-24)
* Fix exception swallowing (#206)

## 0.14.4 (2017-01-03)
* Fix tests (#195)

## 0.14.3 (2017-01-03)
* Fix mirage-conduit (@samoht, #188)
* Use ppx_driver's ocamlbuild plugin (@rgrinberg, #193)

## 0.14.2 (2017-01-01)
* Fix discover.ml (#191)

## 0.14.1 (2016-12-29)
* Tests: do not link with lwt.ssl if it is not installed (#186)

## 0.14.0 (2016-12-25)
* Add listening backlog option and increase it to 128 by deafult (#151)
* Add IP based URI support
* Fix server stop issues for all Lwt servers
* Add Logs based logging to Lwt server errors (#172)
* Add on_exn hook to Lwt servers (#181)
* Limit maximum number of active connections (#116)

## 0.13.0 (2016-09-18):
* Fix build system to stop compiling things twice (#137)
* Lwt: pass uncaught exceptions in server callback to async_exception_hook (#143)
* Lwt: stop printing stuff to stdout (#143)
* Async: fix swallowing of exceptions
* Async: add backlog argument to serve

## 0.12.0 (2016-04-30):
* Convert build system to use PPX instead of Camlp4.
* Call `set_close_on_exec` on `Lwt_unix` listen sockets (#123)

## 0.11.0 (2016-03-25):
* Minimum OCaml version is now 4.02.0
* Add multi-distro Travis testing script.
* Switch to using `pa_sexp_conv` for latest sexp.
* Support Core/Async >=113.24
* Fix `vchan` example code, and use functoria-style mirage
  for it (#108 via @jonludlam)
* [async] Add an `Ssl_unsupported exception` for Async rather than
  just raising `Failure`
* Workaround for infinite loop when failing to accept new connections
  (Edwin Torok #115)
* Support TLSv1.1 and TLSv1.2 with openssl backend (Edwin Torok #115)
* Fix FD leak with the openssl backend (Edwin Torok #115)

## 0.10.0 (2015-12-25):
* Add support for CA certificates in [Conduit_async.serve] (#98).
* Fix file descriptor leak in Lwt backend (#101 from @hannesm).
* Server in `Conduit_lwt_tls` waits for a user callback to finish
  before accepting more connections. Instead, it should only wait
  until the connection is accepted and detach client callback (#97).
* Close socket when `ssl_accept` fails, e.g. when cipher negotiation
  mismatch (#104).

## 0.9.0 (2015-10-14):
* Add a `Launchd` argument for the Conduit_lwt_unix server listener
  to support the MacOSX service launcher (#96).

## 0.8.8 (2015-09-15):
* Expose a new functor `Conduit_mirage.With_tcp` (#92, by @Drup)
* Expose a new functor: `Resolver_mirage.Make_with_stack` to build a DNS
  resolver using an existing network stack (#92, by @Drup)
* Expose `Resolver_mirage.S`, the signature for Mirage's conduit resolvers than
  can perform DNS lookups. These resolvers now expose their `DNS` implmentation
  as a submodule (#92, by @Drup)
* Expose a ?version arg in Conduit_async_ssl.ssl_listen, default being TLS 1.2
  (#94, by @vbmithr)

## 0.8.7 (2015-08-18):
* Do not ignore custom context when calling `Conduit_lwt_unix_ssl.accept`
  (reported by @jrb467 in #88)
* `Conduit_lwt_unix.Serve` now passes the client `flow` to the server
  callback instead of the listening server one.  This lets servers
  retrieve the peer endpoint correctly (reported by @fxfactorial in #87)

## 0.8.6 (2015-07-14)
* Add a `Conduit_mirage.Context`, a functor for creating HTTP(s) conduit
  contexts (with a DNS resolver).

## 0.8.5 (2015-07-12)
* Fix client-side `https://` resolution for `Conduit_mirage`

## 0.8.4 (2015-05-29):
* Full support for `ocaml-tls.0.5.0`
* Breaking API change for mirage-conduit. Now all the flows are dynamic,
  the functors are becoming first-class values so no big functor to build
  first.

## 0.8.3 (2015-05-04):
* Partial support for `ocaml-tls.0.5.0`
* setsockopt TCP_NODELAY fails on a Unix domain socket (#63 by @djs55)

## 0.8.2 (2015-04-18):
* Make TLS optional in `Conduit_mirage`, and disable it by default
  so that it is a developer-only option until it is properly released.
  It can be enabled by setting the `HAVE_MIRAGE_LWT` env variable.

## 0.8.1 (2015-04-17):
* Support Async_SSL version 112.24.00 and higher.
* Add a TLS echo server in `tests/async/`
* [lwt] Do not leak socket fd when a connect or handshake
  operation fails (#56 via Edwin Torok).
* [async] Do not leak pipes in SSL handling (#54 from Trevor Smith).

## 0.8.0 (2015-03-27):
* Add TLS client support for Mirage (#50)
* Do not overwrite the default name resolver for Mirage (#49)
* Add TLS support using the pure OCaml TLS stack (#46).
* Replace the Mirage `Make_flow` functor with `Dynamic_flow` that is
  easier to extend with more flow types.

## 0.7.2 (2015-01-26):
* Add an `error_message` function to simplify error display (#38).
* Improvements to documentation (#37).

## 0.7.1 (2014-12-05):
* Do not emit debug output when the `CONDUIT_DEBUG` variable is not set.
* Do not create symlinks in a local build, which helps with OPAM pins.
* Improve ocamldoc for `Conduit_lwt_unix`.

## 0.7.0 (2014-12-04):
* Add Lwt-unix support for the native OCaml/TLS stack as an alternative
  to OpenSSL. This can be activated by setting the `CONDUIT_TLS` environment
  variable to `native`.  If this is not set and OpenSSL is available, then
  OpenSSL is used by in preference to the pure OCaml implementation.
* Add sexp convertors for `Conduit_lwt_unix.ctx` and `Conduit_mirage.ctx`
  and the `Resolver` service types.
* Fix the Mirage tests to the Mirage 2.0.1+ Conduit interfaces.
* Add more debugging output when the `CONDUIT_DEBUG` variable is set on Unix.
* *Interface breaking:* The `client` and `server` types in `Conduit_lwt_unix`
  now explicitly label the fields of the tuples with a polymorphic variant.
  This allows them to remain independent of this library but still be
  more self-descriptive (i.e. `Port of int` instead of just `int`).

## 0.6.1 (2014-11-07):
* When terminating conduits, always close the output channel first before
  the input channel, so that any pending data in the underlying fd is flushed.

## 0.6.0 (2014-11-04):
* Add an explicit `ctx` content to track every conduit's runtime state.
* Allow the source interface for a conduit to be set.
* Support a `password` callback for the SSL layer (#4).
* [lwt] Add stop parameters in main-loop of the server (#5).
* Add `Conduit_mirage` with Mirage functor suport.
* Add ocamldoc of most interfaces.
* Add a `CONDUIT_DEBUG` environment variable to the Unix backends for
  live debugging.
* Add a `conn` value to the callback to query more information about the
  current connection (#2).
* Expose the representation of `Conduit_lwt_unix.flow` in the external signature.
  This lets library users obtain the original `Lwt_unix.file_descr` when using
  Conduit libraries like Cohttp.

## 0.5.1 (2014-08-07):
* Reenable Async SSL by default.

## 0.5.0 (2014-04-13):
* First public release.
