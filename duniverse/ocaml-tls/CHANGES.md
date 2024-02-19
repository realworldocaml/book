## v0.15.4 (2022-09-27)

* New package tls-eio (#451 @talex5)
* Tls_async: expose tls_handler (#448 @mbacarella, reviewed by @torinnd)

## v0.15.3 (2022-03-29)

* Upgrade to v0.15 of Jane Street packages (#444 @bcc32)
* Use cmdliner 1.1.0 in lwt/examples (#445 @hannesm)

## v0.15.2 (2021-11-14)

* Tls_async: drop dependency on async_find, now trust anchors in a directory
  are not recursively read - aligns it with the lwt and mirage implementations
  (#442 @torinnd)

## v0.15.1 (2021-10-29)

* Tls_lwt: avoid exception if connect is executed with a non-host name string
  (e.g. an IP address) (#441 @hannesm)
* Bugfix: log a warning if certificate decoding fails (#441 @hannesm)
* Remove rresult dependency (#441 @hannesm)

## v0.15.0 (2021-10-07)

* Adapt to x509 0.15.0 changes (#440 @hannesm)

## v0.14.1 (2021-09-13)

* Bugfix: do not filter signature_algorithms based on server certificate. Since
  signature_algorithms is also used for client authentication (as
  SignatureAlgorithms extension in CertificateVerify), previously the client
  needed the same key type as the server.
  Discovered in https://github.com/roburio/albatross/commit/df434da0e531e1b0c8091a0fc82e8b37ed319e7a

## v0.14.0 (2021-08-02)

* Breaking: peer_name (in config and epoch data, also own_name) is now a
  [`host] Domain_name.t instead of a string. (#434 #438 @torinnd @hannesm)
* Add a X509_async module (#435 @torinnd)
* Client and server constructor log messages are on the debug level (#436
  reported by @talex5, fix by @hannesm)
* Adapt to cstruct 6.0.0 API (Cstruct.len is deprecated) #439 @hannesm

## v0.13.2 (2021-06-04)

* New package tls-async that provides an effectful layer of TLS using async.
  (#432, @torinnd, @dinosaure, @kit-ty-kate, reviews by @hannesm @avsm @seliopou)

## v0.13.1 (2021-04-22)

* Breaking: use deriving sexp_of instead of sexp. Constructing a state from
  a sexp has not been supported (lead to exception), and is now removed
  (#430 by @torinnd, continued in #431 by @hannesm)
* Bugfix: TLS 1.3 client authentication with certificate, client side. This
  used to work accidentally before 0.13.0 changed the signature algorithms
  handling, now the right signature algorithm (as requested by server) is used.
  (#431 @hannesm, @talex5 reported https://github.com/mirage/capnp-rpc/pull/228)
* adapt to x509 0.13.0 and mirage-crypto-ec 0.10.0 changes (#431 @hannesm)

## v0.13.0 (2021-04-14)

* Remove static RSA and CBC ciphersuites from default configuration. The
  default configuration now includes FFDHE and ECDHE key exchanges with RSA or
  ECDSA/EdDSA certificates, and AEAD ciphers
  (AES-GCM, AES-CCM, ChaCha20-Poly1305) (#429 by @hannesm)
* Remove SHA1 from signature_algorithms in the default configuration
  (#429 by @hannesm)
* Support ECDSA and EdDSA certificates and private keys via x509 0.12.0 and
  mirage-crypto-ec (#428 by @hannesm)
  Breaking changes:
  - the second part of type Tls.Config.certchain is now a X509.Private_key.t
    (previously Mirage_crypto_pk.Rsa.priv)
  - the type aliases X509_lwt.priv and X509_lwt.authenticator have been removed
* Use mirage-crypto-ec instead of fiat-p256 and hacl_x25519 for elliptic curve
  support - this adds P384 and P521 ECDH support (#428 by @hannesm)
* Remove custom Monad implementation, use Result and Rresult instead
  (#429 by @hannesm)
* Remove Utils.Cs submodule, use Cstruct API instead (#429 by @hannesm)
* Breaking: Tls.Engine.ret type is now a result instead of a custom variant type
  (#429 by @hannesm)
* Breaking: Tls_lwt.Unix.epoch results in (Tls.Core.epoch_data, unit) result -
  it was a custom error type previously (#429 by @hannesm)

## v0.12.8 (2020-12-08)

* Re-add ECPointFormats hello extension (both client and server) to avoid
  handshake failures with Go's TLS stack (RFC 8422 makes it optional, but go
  (1.15.5) requires it) - reported by @jeffa5 at
  https://discuss.ocaml.org/t/strange-prohibited-tls-1-2-cipher-suite-9d-issue/
  fix by @hannesm #424

## v0.12.7 (2020-12-04)

* Tls.lwt: make the receive buffer connection-local to avoid potential data
  races (#422 by @dinosaure)
* Tls_mirage: remove unneeded type alias (@hannesm)
* Add Tls.Config.Ciphers.http2 - a list of ciphersuites allowed to be negotiated
  for HTTP2 sessions (#423 by @jeffa5)

## v0.12.6 (2020-11-06)

* OCaml 4.12 support (#421 @kit-ty-kate)

## v0.12.5 (2020-09-22)

* Rename length to v_length to be compatible with cstruct 6.0.0 (#419 @dinosaure)

## v0.12.4 (2020-08-08)

* handshake_server13: demote group and cipher log level (#417 by @xguerin)
* tls_lwt: register printers for Tls_alert and Tls_failure (#418 by @hannesm)

## v0.12.3 (2020-07-04)

* Adapt to new GCM and CCM API of mirage-crypto (#416 by @hannesm)
* Add support for ChaCha20/Poly1305 ciphersuite (#416 by @hannesm)

## v0.12.2 (2020-06-20)

* tls_lwt again calls Mirage_crypto_rng_lwt.initialize () -- which is since
  mirage-crypto-rng 0.8 no longer inside the lwt monad, and safe to be called
  multiple times and on top level (#415 by @hannesm)

## v0.12.1 (2020-06-12)

in #414 by @hannesm
* Drop support for RC4 ciphersuite
* Raise lower TLS version in default configuration to 1.2
* tls_lwt no longer calls Mirage_crypto_rng_unix.initialize -- this needs to be
  done in the application, inside Lwt_main.run:
  `Mirage_crypto_rng_lwt.initialize () >>= fun () ->`
* Support ECDHE ciphersuites in TLS 1.2 and below as specified in RFC 8422
  (requested in #413 by @ryanakca, also in #362 by @orbitz @annubiz)
* drop "TLS_" prefix from ciphersuite constructors
* BUGFIX: TLS client (<= 1.2) assembling an empty Certificate message
  (noticed in #413, present since 0.12.0 release)
* Cleanup Packet.any_ciphersuite list (remove ARIA, CAMELLIA, KRB5, EXPORT)
* Adapt interoperability test scripts with TLS 1.3 support

## v0.12.0 (2020-05-12)

in #405 by @hannesm
* TLS 1.3 support
* Tracing now uses the logs library (log source tls.tracing on debug level)
* bugfix for padding in ClientHello, which computed wrong length
* bugfix hs_fragments to be set before executing the protocol handling logic
* bugfix guard RSA signature with an Insufficient_key handler, which may occur
  when using an RSA key which size is too small for the used digest algorithm

## v0.11.1 (2020-04-09)

* Adapt to X509.0.11.0 API changes (#412)

## v0.11.0 (2020-03-12)

* use dune as build system (#407)
* BREAKING split into tls and tls-mirage opam packages (#407)
* BREAKING use mirage-crypto instead of nocrypto (#407)

## v0.10.6 (2020-01-23)

* adapt to x509 0.9.0 interface: certificate revocation lists can now be passed
  to the authenticator in Tls_mirage and X509_lwt; also a list of hash
  algorithms to be used for certificate signature verification can be passed to
  the authenticator
* adapt to lwt 5.0.0

## v0.10.5 (2019-11-01)

* adapt to mirage-flow 2.0.0, mirage-clock 3.0.0, mirage-kv 3.0.0 interfaces (#401 @hannesm)

## 0.10.4 (2019-08-15)

* tls_lwt: avoid double close by checking in the default `close` callback of
  `Lwt_io.make` whether the underlying file descriptor has been closed already.
  (reported and discussed by @hcarty in #395, merged #397)

## 0.10.3 (2019-07-26)

* support x509 0.7.0+
* remove dependency on Astring (was only used in the lwt-starttls example)

## 0.10.2 (2019-04-02)

* support for cstruct 4.0.0+
* remove support for < 4.04.2 (same as x509 in master)
* remove result (part of 4.03.0)
* enhance mirage/example2 to work on more platforms than unix

## 0.10.1 (2019-02-28)

* tls-mirage: fix compilation

## 0.10.0 (2019-02-28)

* tls: fix extensions length (used to include the 2 byte extension length field)
  if padding is inserted (introduced on May 5, 2014 in #73)
* tls-mirage: adapt to mirage-kv 2.0.0 API (#384, @samoht)

## 0.9.3 (2019-01-07)

* tls: do not require client sent ciphersuites to be a proper set
  (interoperability with some android devices)
* tls_lwt: delay error from writing to peer while reading, record errors only
  if state is active (fixes #347)
* migrate opam file to opam 2.0 format

## 0.9.2 (2018-08-24)

* compatibility with ppx_sexp_conv >v0.11.0 (#381), required for 4.07.0
* support ALPN (#378, @bobbypriambodo)

## 0.9.1 (2018-02-26)

* Tls_lwt: use Tls.Config instead of Config directly to avoid polluting imported
  names (#376, @rgrinberg)

## 0.9.0 (2017-12-23)

* renegotiation semantics (#375)
   allow acceptable_ca, authenticator, and own_cert to be updated (Config.with_x)
   semantics of reneg is blocking
   `{Tls_lwt.Unix|Tls_mirage}.reneg ~drop:bool` drops data of earlier epoch
* implement acceptable_ca (#332, @reynir)
* fix client renegotiation with ExtendedMasterSecret (#373, broken since 0.7.0)
* Config.client can get ~peer_name (#373)
* Asn.Time.t is Ptime.t now (asn1-combinators.0.2.0, x509.0.6.0, #372)
* cleanups (#360, #363, #369, @rgrinberg)
* remove 3DES CBC SHA from default ciphers (#359)

## 0.8.0 (2017-02-01)

* lwt: in Unix.client_of_fd the named argument host is now optional (#336)
* mirage: in client_of_flow the (positional) hostname argument is now optional (#336)
* mirage: adapt to PCLOCK interface (@mattgray #329 #331)
* build system migrated from oasis to topkg (#342)
* mirage: adapt to MirageOS3 (@yomimono @samoht #338 #349 #350 #351 #353)
* lwt: do not crash on double close (@vbmithr #345)
* fixed docstring typos (@mor1 #340)

## 0.7.1 (2016-03-21)

* remove camlp4 dependency (use cstruct ppx and sexplib ppx instead)
* sort client extensions, there are servers which dislike an extension without
  data at the end, thus try to send extensions with data at the end (#319)
* initial GCM support (#310)
* fix `hs_can_handle_appdata` (#315):
    Initially we allowed application data always after the first handshake.

    Turns out, between CCS and Finished there is new crypto_context in place
    which has not yet been authenticated -- bad idea to accept application data
    at that point (beginning of 2015 in OCaml TLS).

    The fix was to only allow application data in Established state (and block
    in Tls_lwt/Tls_mirage when the user requested renegotiation) (December 2015
    in OCaml-TLS).

    Renegotiation was also turned off by default when we introduced resumption
    (mid October 2015): both features together (without mitigating via session
    hash) allow the triple handshake.

    It turns out, the server side can happily accept application data from the
    other side when it just sent a HelloRequest (and waits for the ClientHello;
    same is true for the client side, waiting for the ServerHello in
    renegotiation case might be interleaved with application data) to let the
    client initiate a new handshake.  By this commit, OCaml-TLS allows
    application data then.

    In the end, it is a pretty academic thing anyways, since nobody uses
    renegotiation with OCaml-TLS in the field.
* during verification of a digitally signed: checked that the used hash
  algorithm is one of the configured ones (#313)
* unify return type of handshake and change cipher spec handler (#314)
* separate client and server extensions (#317)
* type equality (no longer generative error type), use result (#318)
* removed Printer (was barely useful)

## 0.7.0 (2015-12-04)

* session resumption (via session ID) support (#283)
  Config contains `session_cache : SessionID.t -> epoch_data option`
  and `cached_session : epoch_data option`
* session hash and extended master secret (RFC 7627) support (#287)

### semantic changes
* disable renegotiation by default (#300)
* blocking semantics (both Mirage and Lwt) while renegotiating (#304)
* `Engine.handshake_in_progress` no longer exist
* `Hex_fingerprint / `Fingerprint authenticators no longer exist
* Mirage X509 does no longer prefix keys and trust anchors with "tls/" in the path

### minor fixes
* fix concurrent read/write in tls_mirage (#303)
* expose own_random and peer_random in epoch_data (@cfcs, #297)
* public key pinning (X509_lwt) via `Hex_key_fingerprint / `Key_fingerprint (#301)
* certificate chain and peer certificate are exposed via epoch_data (new path-building X.509 interface)

## 0.6.0 (2015-07-02)

* API: dropped 'perfect' from forward secrecy in Config.Ciphers:
  fs instead of pfs, fs_of instead of pfs_of
* API: type epoch_data moved from Engine to Core
* removed Cstruct_s now that cstruct (since 1.6.0) provides
  s-expression marshalling
* require at least 1024 bit DH group, use FFDHE 2048 bit DH group
  by default instead of oakley2 (logjam)
* more specific alerts:
  - UNRECOGNIZED_NAME: if hostname in SNI does not match
  - UNSUPPORTED_EXTENSION: if server hello has an extension not present in
    client hello
  - ILLEGAL_PARAMETER: if a parse error occured
* encrypt outgoing alerts
* fix off-by-one in handling empty TLS records: if a record is less than 5
  bytes, treat as a fragment. exactly 5 bytes might already be a valid
  application data frame

## 0.5.0 (2015-05-02)

* updates to extension enum (contributed by Dave Garrett #264)
* removed entropy feeding (done by nocrypto) #265
* Tls_lwt file descriptor lifecycle: not eagerly close file descriptors #266

## 0.4.0 (2015-03-19)

* client authentication (both client and server side)
* server side SNI configuration (see sni.md)
* SCSV server-side downgrade prevention (by Gabriel de Perthuis @g2p #5)
* remove RC4 ciphers from default config #8
* support for AEAD ciphers, currently CCM #191
* proper bounds checking of handshake fragments #255
* disable application data between CCS and Finished #237
* remove secure renegotiation configuration option #256
* expose epoch in mirage interface, implement 2.3.0 API (error_message)
* error reporting (type failure in engine.mli) #246
* hook into Lwt event loop to feed RNG #254

## 0.3.0 (2014-12-21)

* X509_lwt provides `Fingerprints and `Hex_fingerprints constructor for
  checking fingerprints of certificates instead of trusting trust
  anchors #206 #207
* client configuration requires an authenticator #202
* server certificate must be at least Config.min_rsa_key_size bits
* expose epoch via lwt interface #208
* mirage-2.2.0 compatibility #212
* cleanups of mirage interface #213
* nocrypto-0.3.0 compatibility #194 #209 #210

## 0.2.0 (2014-10-30)

* distinguish between supported hash and mac algorithms (using Nocrypto.Hash)
  and those which may occur on the wire #189
* expose trust anchor when authenticating certificate (requires x509 >=0.2) #178
* information about the active session is exposed via epoch : state -> epoch
* distinguish between supported ciphersuites (type ciphersuite) and
  known ciphersuites (type any_ciphersuite) #173
* distinguish between supported versions by the stack (type tls_version)
  and readable versions (tls_any_version), which might occur in a tls
  record or client_hello read from the network #179 #172
* support > TLS-1.2 client hellos (as reported by ssllabs.com #161)
* support iOS 6 devices (who propose NULL ciphers - reported in #160)
* send minimal protocol version in record layer of client hello
  (maximum version is in the client hello itself) (RFC5246, E.1) #165

## 0.1.0 (2014-07-08)

* initial beta release