(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** Connection establishment using the {{:http://ocsigen.org/lwt/api/Lwt_unix}
    Lwt_unix} library *)

(** {2 Core types} *)

type client_tls_config =
  [ `Hostname of string ] * [ `IP of Ipaddr.t ] * [ `Port of int ]
[@@deriving sexp]
(** Configuration fragment for a TLS client connecting to a remote endpoint *)

type client =
  [ `TLS of client_tls_config
  | `TLS_native of client_tls_config
    (** Force use of native OCaml TLS stack to connect.*)
  | `OpenSSL of client_tls_config
    (** Force use of Lwt OpenSSL bindings to connect. *)
  | `TCP of [ `IP of Ipaddr.t ] * [ `Port of int ]
    (** Use TCP to connect to the given [ip], [port] tuple. *)
  | `Unix_domain_socket of [ `File of string ]
    (** Use UNIX domain sockets to connect to a socket on the [path]. *)
  | `Vchan_direct of [ `Domid of int ] * [ `Port of string ]
    (** Connect to the remote VM on the [domid], [port] tuple. *)
  | `Vchan_domain_socket of [ `Domain_name of string ] * [ `Port of string ]
    (** Use the Vchan name resolution to connect *) ]
[@@deriving sexp]
(** Set of supported client connections that are supported by this module:

    - [`TLS (`Hostname host, `IP ip, `Port port)]: Use OCaml-TLS or OpenSSL
      (depending on CONDUIT_TLS) to connect to the given [host], [ip], [port]
      tuple via TCP.
    - [`TLS_native _]: Force use of native OCaml TLS stack to connect.
    - [`OpenSSL _]: Force use of Lwt OpenSSL bindings to connect.
    - [`TCP (`IP ip, `Port port)]: Use TCP to connect to the given [ip], [port]
      tuple.
    - [`Unix_domain_socket (`File path)]: Use UNIX domain sockets to connect to
      a socket on the [path].
    - [`Vchan_direct (`Domid domid, `Port port)]: Connect to the remote VM on
      the [domid], [port] tuple.
    - [`Vchan_domain_socket (`Domain_name domain, `Port port_name)]: Use the
      Vchan name resolution to connect. *)

type server_tls_config =
  [ `Crt_file_path of string ]
  * [ `Key_file_path of string ]
  * [ `Password of bool -> string | `No_password ]
  * [ `Port of int ]
[@@deriving sexp]
(** Configuration fragment for a listening TLS server *)

type tcp_config =
  [ `Port of int | `Socket of Lwt_unix.file_descr [@sexp.opaque] ]
[@@deriving sexp]
(** Set of ways to create TCP servers

    - [`Port port]: Create a socket listening to provided port.
    - [`Socket file_descr]: Use the provided file descriptor to create a server. *)

type server =
  [ `TLS of server_tls_config
  | `OpenSSL of server_tls_config
  | `TLS_native of server_tls_config
  | `TCP of tcp_config
  | `Unix_domain_socket of [ `File of string ]
  | `Vchan_direct of int * string
  | `Vchan_domain_socket of string * string
  | `Launchd of string ]
[@@deriving sexp]
(** Set of supported listening mechanisms that are supported by this module.

    - [`TLS server_tls_config]: Use OCaml-TLS or OpenSSL (depending on
      CONDUIT_TLS) to connect to the given [host], [ip], [port] tuple via TCP.
    - [`TLS_native _]: Force use of native OCaml TLS stack to connect.
    - [`OpenSSL _]: Force use of Lwt OpenSSL bindings to connect.
    - [`TCP (`Port port)]: Listen on the specified TCPv4 port.
    - [`Unix_domain_socket (`File path)]: Use UNIX domain sockets to listen on
      the path.
    - [`Vchan_direct (domid, port)]: Listen for the remote VM on the [domid],
      [port] tuple.
    - [`Vchan_domain_socket (domain, port_name)]: Use the Vchan name resolution
      to listen
    - [`Listening_socket fd]: Use the socket given, useful for inherited systemd
      sockets.
    - [`Launchd name]: uses MacOS X launchd to start the service, via the name
      of the [Sockets] element within the service description plist file. See
      the {{:http://mirage.github.io/ocaml-launchd/launchd/} ocaml-launchd}
      documentation for more. *)

type 'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

type tcp_flow = private {
  fd : Lwt_unix.file_descr; [@sexp.opaque]
  ip : Ipaddr.t;
  port : int;
}
[@@deriving sexp_of]
(** [tcp_flow] contains the state of a single TCP connection. *)

type domain_flow = private {
  fd : Lwt_unix.file_descr; [@sexp.opaque]
  path : string;
}
[@@deriving sexp_of]
(** [domain_flow] contains the state of a single Unix domain socket connection. *)

type vchan_flow = private { domid : int; port : string } [@@deriving sexp_of]
(** [vchan_flow] contains the state of a single Vchan shared memory connection. *)

(** A [flow] contains the state of a single connection, over a specific
    transport method. *)
type flow = private
  | TCP of tcp_flow
  | Domain_socket of domain_flow
  | Vchan of vchan_flow
[@@deriving sexp_of]

type tls_own_key =
  [ `None
  | `TLS of
    [ `Crt_file_path of string ]
    * [ `Key_file_path of string ]
    * [ `Password of bool -> string | `No_password ] ]
[@@deriving sexp]
(** Type describing where to locate a PEM key in the filesystem *)

(**/**)

type tls_server_key = tls_own_key [@@deriving sexp]

(**/**)

type ctx [@@deriving sexp_of]
(** State handler for an active conduit *)

(** {2 Connection and listening} *)

val default_ctx : ctx Lazy.t
(** Default context that listens on all source addresses with no TLS certificate
    associated with the Conduit *)

val init :
  ?src:string ->
  ?tls_own_key:tls_own_key ->
  ?tls_authenticator:Conduit_lwt_tls.X509.authenticator ->
  ?ssl_ctx:Conduit_lwt_unix_ssl.Client.context ->
  ?ssl_client_verify:Conduit_lwt_unix_ssl.Client.verify ->
  unit ->
  ctx io
(** [init ?src ?tls_own_key ?tls_authenticator ?ssl_ctx ()] will initialize a
    Unix conduit that binds to the [src] interface if specified.

    If TLS server connections are used, then [tls_own_key] must contain a valid
    certificate to be used to advertise a TLS connection. In TLS mode the
    certificate is validated using [tls_authenticator]. By default, the
    validation is using the {{:https://github.com/mirage/ca-certs} OS trust
    anchors}.

    If SSL client connections are used, then [tls_own_key] may contain a valid
    certificate to be used to advertise a TLS connection. If it's not configured
    [ssl_ctx] will be used to configure OpenSSL. *)

val connect : ctx:ctx -> client -> (flow * ic * oc) io
(** [connect ~ctx client] establishes an outgoing connection via the [ctx]
    context to the endpoint described by [client] *)

val serve :
  ?backlog:int ->
  ?timeout:int ->
  ?stop:unit io ->
  on_exn:(exn -> unit) ->
  ctx:ctx ->
  mode:server ->
  (flow -> ic -> oc -> unit io) ->
  unit io
(** [serve ?backlog ?timeout ?stop ~on_exn ~ctx ~mode fn] establishes a
    listening connection of type [mode], using the [ctx] context. The [stop]
    thread will terminate the server if it ever becomes determined. Every
    connection will be served in a new lightweight thread that is invoked via
    the [fn] callback. The [fn] callback is passed the {!flow} representing the
    client connection and the associated input {!ic} and output {!oc} channels.
    If the callback raises an exception, it is passed to [on_exn]. *)

val set_max_active : int -> unit
(** [set_max_active nconn] sets the maximum number of active connections
    accepted. When the limit is hit accept blocks until another server
    connection is closed. *)

val endp_of_flow : flow -> Conduit.endp
(** [endp_of_flow flow] retrieves the original {!Conduit.endp} from the
    established [flow] *)

val endp_to_client : ctx:ctx -> Conduit.endp -> client io
(** [endp_to_client ~ctx endp] converts an [endp] into a a concrete connection
    mechanism of type [client] *)

val endp_to_server : ctx:ctx -> Conduit.endp -> server io
(** [endp_to_server ~ctx endp] converts an [endp] into a a concrete connection
    mechanism of type [server] *)

(** {2 TLS library selection} *)

(** Currently selected method of using TLS for client and servers *)
type tls_lib =
  | OpenSSL  (** The [Lwt_ssl] bindings to the C OpenSSL library *)
  | Native  (** A pure OCaml TLS implementation *)
  | No_tls  (** No TLS implementation available, so any connections will fail *)

val tls_library : tls_lib ref
(** The default selection is to select {!OpenSSL}, {!Native} and {!No_tls} in
    decreasing order of priority. The native OCaml stack can be forced by
    setting the [CONDUIT_TLS] Unix environment variable to [native]. *)
