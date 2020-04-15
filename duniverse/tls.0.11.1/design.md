### The OCaml-TLS architecture

The OCaml ecosystem has several distinct ways of interacting with the outside world
(and the network in particular): straightforward [unix][ocaml-unix] interfaces
and the asynchronous programming libraries [lwt][] and [async][]. One of the
early considerations was not to restrict ourselves to any of those -- we wanted
to support them all.

There were also two distinct basic "platforms" we wanted to target from the
outset: the case of a simple executable, and the case of `Mirage` unikernels.

So one of the first questions we faced was deciding how to represent
interactions with the network in a portable way. This can be done by
systematically abstracting out the API boundary which gives access to network
operations, but we had a third thing in mind as well: we wanted to exploit the
functional nature of OCaml to its fullest extent!

Our various prior experiences with Haskell and Idris convinced us to adopt
what is called "purely functional" technique. We believe it to be an approach
which first forces the programmer to give principled answers to all the
difficult design questions (errors and global data-flow) *in advance*, and then
leads to far cleaner and composable code later on. A purely functional system
has all the data paths made completely explicit in the form of function
arguments and results. There are no unaccounted-for interactions between
components mediated by shared state, and all the activity of the parts of the
system is exposed through types since, after all, it's only about computing
values from values.

For these reasons, the library is split into two parts: the directory `/lib`
(and the corresponding findlib package `tls`) contains the core TLS logic, and
`/mirage` and `/lwt` (packaged as `tls.mirage` and `tls.lwt` respectively)
contain front-ends that tie the core to `Mirage` and `Lwt_unix`.

[ocaml-unix]: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html
[lwt]: http://ocsigen.org/lwt/
[async]: https://realworldocaml.org/v1/en/html/concurrent-programming-with-async.html

### Core

The [core][tls-engine-mli] library is purely functional. A TLS session is represented by the
abstract type `Tls.Engine.state`, and various functions consume this session
type together with raw bytes (`Cstruct.t` -- which is by itself mutable, but
`ocaml-tls` eschews this) and produce new session values and resulting buffers.

The central entry point is [handle_tls][], which transforms an input state and a
buffer to an output state, a (possibly empty) buffer to send to the
communication partner, and an optional buffer of data intended to be received by
the application:

```OCaml
type state

type ret = [
  | `Ok of [ `Ok of state | `Eof | `Alert of alert ] *
      [ `Response of Cstruct.t ] * [ `Data of Cstruct.t option ]
  | `Fail of alert * [ `Response of Cstruct.t ]
]

val handle_tls : state -> Cstruct.t -> ret
```

As the signature shows, errors are signalled through the `ret` type, which is a [polymorphic variant][poly]. This
reflects the actual internal structure: all the errors are represented as
values, and operations are composed using an error [monad][monad-ml].

Other entry points share the same basic behaviour: they transform the prior
state and input bytes into the later state and output bytes.

Here's a rough outline of what happens in `handle_tls`:

- TLS packets consist of a header, which contains the protocol
  version, length, and content type, and the payload of the given
  content type. Once inside our [main handler][handle_tls], we
  [separate][separate_records] the buffer into TLS records, and
  [process][handle_raw_record] each individually. We first check that
  the version number is correct, then [decrypt][decrypt], and [verify
  the mac][verify_mac].

- Decrypted data is then [dispatched][handle_packet] to one of four
  sub-protocol handlers (Handshake, Change Cipher Spec, Alert and
  Application Data). Each handler can [return][return_types] a new
  handshake state, outgoing data, application data, the new decryption
  state or an error (with the outgoing data being an interleaved list
  of buffers and new encryption states).

- The outgoing buffers and the encryption states are
  [traversed][encrypt] to produce the final output to be sent to the
  communication partner, and the final encryption, decryption and
  handshake states are combined into a new overall state which is
  returned to the caller.

Handshake is (by far) the most complex TLS sub-protocol, with an elaborate state
machine. Our [client][client_handshake] and [server][server_handshake] encode
this state as a "flat" [sum type][handshake_states], with exactly one incoming
message allowed per state. The handlers first [parse][parse_handshake] the
handshake packet (which fails in case of malformed or unknown data) and then
dispatch it to the handling function. The [handshake state][handshake_state] is
carried around and a fresh one is returned from the handler in case it needs
updates. It consists of a protocol version, the handshake state, configuration,
renegotiation data, and possibly a handshake fragment.

Logic of both handshake handlers is very localised, and does not mutate any
global data structures.

[poly]: https://realworldocaml.org/v1/en/html/variants.html#polymorphic-variants
[monad-ml]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/control.ml
[return_types]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/state.ml#L109
[encrypt]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.ml#L48
[handle_packet]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.ml#L240
[verify_mac]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.ml#L85
[decrypt]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.ml#L95
[handle_tls]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.ml#L321
[handle_raw_record]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.ml#L275
[separate_records]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.ml#L150

[handshake_state]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/state.ml#L92
[parse_handshake]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/reader.ml#L361
[separate_handshakes]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.ml#L217
[handshake_states]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/state.ml#L61
[server_handshake]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/handshake_server.ml#L247
[client_handshake]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/handshake_client.ml#L285

### Core API

OCaml permits the implementation a module to be exported via a more
abstract *signature* that hides the internal representation
details. Our public API for the core library consists of the
[Tls.Engine][tls-engine-mli] and [Tls.Config][tls-config-mli] modules.

`Tls.Engine` contains the basic reactive function `handle_tls`, mentioned above,
which processes incoming data and optionally produces a response, together with
several operations that allow one to initiate message transfer like
`send_application_data` (which processes application-level messages for
sending), `send_close_notify` (for sending the ending message) and `reneg`
(which initiates full TLS renegotiation).

The module also contains the only two ways to obtain the initial state:

```OCaml
val client : Config.client -> (state * Cstruct.t)
val server : Config.server -> state
```

That is, one needs a configuration value to create it. The `Cstruct.t`
that `client` emits is the initial Client Hello since in TLS,
the client starts the session.

`Tls.Config` synthesizes configurations, separately for client and server
endpoints, through the functions `client_exn` and `server_exn`. They take a
number of parameters that define a TLS session, check them for consistency, and
return the sanitized `config` value which can be used to create a `state` and,
thus, a session. If the check fails, they raise an exception.

The parameters include the pair of a certificate and its private key for the
server, and an `X509.Authenticator.t` for the client, both produced by our
[ocaml-x509][] library and described in a [previous article][x509-intro].

This design reflects our attempts to make the API as close to "fire and forget"
as we could, given the complexity of TLS: we wanted the library to be relatively
straightforward to use, have a minimal API footprint and, above all, fail very
early and very loudly when misconfigured.

[tls-engine-mli]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/engine.mli

[tls-config-mli]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lib/config.mli

[ocaml-x509]: https://github.com/mirleft/ocaml-x509


### Effectful front-ends

Clearly, reading and writing network data *does* change the state of the world.
Having a pure value describing the state of a TLS session is not really useful
once we write something onto the network; it is certainly not the case that we
can use more than one distinct `state` to process further data, as only one
value is in sync with the other endpoint at any given time.

Therefore we wrap the core types into stateful structures loosely inspired by
sockets and provide IO operations on those. The structures of `mirage` and `lwt`
front-ends mirror one another.

In both cases, the structure is pull-based in the sense that no processing is
done until the client requires a read, as opposed to a callback-driven design
where the client registers a callback and the library starts spinning in a
listening loop and invoking it as soon as there is data to be processed. We do
this because in an asynchronous context, it is easy to create a callback-driven
interface from a demand-driven one, but the opposite is possible only with
unbounded buffering of incoming data.

One exception to demand-driven design is the initial session creation: the
library will only yield the connection after the first handshake is over,
ensuring the invariant that it is impossible to interact with a connection if it
hasn't already been fully established.

**Mirage**

The `Mirage` [interface][tls_mirage_types_mli] matches the [FLOW][flow]
signature (with additional TLS-specific operations). We provide a functor that
needs to be applied to an underlying TCP module, to obtain a TLS transport on
top. For example:

```OCaml
module Server (Stack: STACKV4) (KV: KV_RO) =
struct

  module TLS  = Tls_mirage.Make (Stack.TCPV4)
  module X509 = Tls_mirage.X509 (KV) (Clock)

  let accept conf flow =
    TLS.server_of_tcp_flow conf flow >>= function
    | `Ok tls ->
      TLS.read tls >>= function
      | `Ok buf ->
        TLS.write tls buf >>= fun () -> TLS.close buf

  let start stack e kv =
    lwt authenticator = X509.authenticator kv `Default in
    let conf          = Tls.Config.server_exn ~authenticator () in
    Stack.listen_tcpv4 stack 4433 (accept conf) ;
    Stack.listen stack

end
```

**Lwt**

The `lwt` interface has [two layers][tls_lwt_mli]. `Tls_lwt.Unix` is loosely based
on read/write operations from `Lwt_unix` and provides in-place update of
buffers. `read`, for example, takes a `Cstruct.t` to write into and returns the
number of bytes read. The surrounding module, `Tls_lwt`, provides a simpler,
`Lwt_io`-compatible API built on top:

```OCaml
let main host port =
  lwt authenticator = X509_lwt.authenticator (`Ca_dir nss_trusted_ca_dir) in
  lwt (ic, oc)      = Tls_lwt.connect ~authenticator (host, port) in
  let req = String.concat "\r\n" [
    "GET / HTTP/1.1" ; "Host: " ^ host ; "Connection: close" ; "" ; ""
  ] in
  Lwt_io.(write oc req >>= fun () -> read ic >>= print)
```

We have further plans to provide wrappers for [`Async`][async] and plain [`Unix`][ocaml-unix] in a
similar vein.

[tls_mirage_types_mli]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/mirage/tls_mirage_types.mli
[flow]: https://github.com/mirage/mirage/blob/ae3c966f8d726dc97208595b8005e02e39478cb1/types/V1.mli#L136
[example_unikernel]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/mirage/example/unikernel.ml
[tls_lwt_mli]: https://github.com/mirleft/ocaml-tls/blob/6dc9258a38489665abf2bd6cdbed8a1ba544d522/lwt/tls_lwt.mli
