open Async

module type V1 = sig
  type session [@@deriving sexp_of]
  type ssl_conn [@@deriving sexp_of]
  type ssl_version [@@deriving sexp]

  module Conduit_async : sig
    module Ssl : sig
      type config [@@deriving sexp]

      val verify_certificate : ssl_conn -> bool Deferred.t

      val configure
        : ?version:ssl_version
        -> ?name:string
        -> ?ca_file:string
        -> ?ca_path:string
        -> ?session:session
        -> ?verify:(ssl_conn -> bool Deferred.t)
        -> unit
        -> config
    end

    type +'a io = 'a Deferred.t
    type ic = Reader.t
    type oc = Writer.t

    type addr = [
      | `OpenSSL of string * Ipaddr.t * int
      | `OpenSSL_with_config of string * Ipaddr.t * int * Ssl.config
      | `TCP of Ipaddr.t * int
      | `Unix_domain_socket of string
    ] [@@deriving sexp]

    val connect : ?interrupt:unit io -> addr -> (ic * oc) io
    val with_connection : ?interrupt:unit io -> addr -> (ic -> oc -> unit io) -> unit io

    type trust_chain =
      [ `Ca_file of string
      | `Ca_path of string
      | `Search_file_first_then_path of
          [ `File of string ] *
          [ `Path of string ]
      ] [@@deriving sexp]

    type openssl =
      [ `OpenSSL of
          [ `Crt_file_path of string ] *
          [ `Key_file_path of string ]
      ] [@@deriving sexp]

    type server = [
      | openssl
      | `TCP
      | `OpenSSL_with_trust_chain of
          (openssl * trust_chain)
    ] [@@deriving sexp]

    val serve :
      ?max_connections:int ->
      ?backlog:int ->
      ?buffer_age_limit:Writer.buffer_age_limit ->
      on_handler_error:[ `Call of ([< Socket.Address.t ] as 'a) -> exn -> unit
                       | `Ignore
                       | `Raise ] ->
      server ->
      ('a, 'b) Tcp.Where_to_listen.t ->
      ('a -> ic -> oc -> unit io) ->
      ('a, 'b) Tcp.Server.t io
  end

  module Conduit_async_ssl : sig
    module Ssl_config = Conduit_async.Ssl

    val ssl_connect : Conduit_async.Ssl.config -> Reader.t -> Writer.t ->
      (Reader.t * Writer.t) Deferred.t

    val ssl_listen
      : ?version:ssl_version
      -> ?ca_file:string
      -> ?ca_path:string
      -> crt_file:string
      -> key_file:string
      -> Reader.t
      -> Writer.t
      -> (Reader.t * Writer.t) Deferred.t
  end
end

module type V2 = sig
  type allowed_ciphers =
    [ `Only of string list | `Openssl_default | `Secure ]
  [@@deriving sexp]
  type ssl_version [@@deriving sexp]
  type session [@@deriving sexp_of]
  type verify_mode [@@deriving sexp_of]
  type ssl_opt [@@deriving sexp]
  type ssl_conn [@@deriving sexp_of]


  module Ssl : sig
    module Config : sig
      type t [@@deriving sexp_of]

      val create
        : ?version:ssl_version
        -> ?options:ssl_opt list
        -> ?name:string
        -> ?hostname:string
        -> ?allowed_ciphers:allowed_ciphers
        -> ?ca_file:string
        -> ?ca_path:string
        -> ?crt_file:string
        -> ?key_file:string
        -> ?session:session
        -> ?verify_modes:verify_mode list
        -> ?verify:(ssl_conn -> bool Deferred.t)
        -> unit
        -> t
    end
  end

  type addr = [
    | `OpenSSL of Ipaddr.t * int * Ssl.Config.t
    | `TCP of Ipaddr.t * int
    | `Unix_domain_socket of string
  ] [@@deriving sexp_of]

  val connect
    : ?interrupt:unit Deferred.t
    -> addr
    -> (Reader.t * Writer.t) Deferred.t

  val with_connection
    : ?interrupt:unit Deferred.t
    -> addr
    -> (Reader.t -> Writer.t -> unit Deferred.t)
    -> unit Deferred.t

  type trust_chain =
    [ `Ca_file of string
    | `Ca_path of string
    | `Search_file_first_then_path of
        [ `File of string ] *
        [ `Path of string ]
    ] [@@deriving sexp]

  type openssl =
    [ `OpenSSL of
        [ `Crt_file_path of string ] *
        [ `Key_file_path of string ]
    ] [@@deriving sexp]

  type server = [
    | openssl
    | `TCP
    | `OpenSSL_with_trust_chain of
        (openssl * trust_chain)
  ] [@@deriving sexp]

  val serve :
    ?max_connections:int ->
    ?backlog:int ->
    ?buffer_age_limit:Writer.buffer_age_limit ->
    on_handler_error:[ `Call of ([< Socket.Address.t ] as 'a) -> exn -> unit
                     | `Ignore
                     | `Raise ] ->
    server ->
    ('a, 'b) Tcp.Where_to_listen.t ->
    ('a -> Reader.t -> Writer.t -> unit Deferred.t) ->
    ('a, 'b) Tcp.Server.t Deferred.t
end

module type V3 = sig
  type allowed_ciphers =
    [ `Only of string list | `Openssl_default | `Secure ]
  [@@deriving sexp]
  type ssl_version [@@deriving sexp]
  type session [@@deriving sexp_of]
  type verify_mode [@@deriving sexp_of]
  type ssl_opt [@@deriving sexp]
  type ssl_conn [@@deriving sexp_of]


  module Ssl : sig
    module Config : sig
      type t [@@deriving sexp_of]

      val create
        : ?version:ssl_version
        -> ?options:ssl_opt list
        -> ?name:string
        -> ?hostname:string
        -> ?allowed_ciphers:allowed_ciphers
        -> ?ca_file:string
        -> ?ca_path:string
        -> ?crt_file:string
        -> ?key_file:string
        -> ?session:session
        -> ?verify_modes:verify_mode list
        -> ?verify:(ssl_conn -> bool Deferred.t)
        -> unit
        -> t
    end
  end

  type _ addr =
    | OpenSSL : Socket.Address.Inet.t * Ssl.Config.t -> Socket.Address.Inet.t addr
    | Inet : Socket.Address.Inet.t -> Socket.Address.Inet.t addr
    | Unix : Socket.Address.Unix.t -> Socket.Address.Unix.t addr
  [@@deriving sexp_of]

  type _ tcp_sock =
    | Inet_sock :
        ([`Active], Socket.Address.Inet.t) Socket.t ->
        Socket.Address.Inet.t tcp_sock
    | Unix_sock :
        ([`Active], Socket.Address.Unix.t) Socket.t ->
        Socket.Address.Unix.t tcp_sock

  val resolve_uri :
    ?options:Unix.Addr_info.getaddrinfo_option list -> Uri.t ->
    Socket.Address.Inet.t addr Deferred.t

  val connect
    : ?interrupt:unit Deferred.t
    -> 'a addr
    -> ('a tcp_sock * Reader.t * Writer.t) Deferred.t

  val with_connection
    : ?interrupt:unit Deferred.t
    -> 'a addr
    -> ('a tcp_sock -> Reader.t -> Writer.t -> 'b Deferred.t)
    -> 'b Deferred.t

  val connect_uri :
    ?options:Unix.Addr_info.getaddrinfo_option list ->
    ?interrupt:unit Deferred.t
    -> Uri.t
    -> (Socket.Address.Inet.t tcp_sock * Reader.t * Writer.t) Deferred.t

  val with_connection_uri :
    ?options:Unix.Addr_info.getaddrinfo_option list ->
    ?interrupt:unit Deferred.t
    -> Uri.t
    -> (Socket.Address.Inet.t tcp_sock -> Reader.t -> Writer.t -> 'a Deferred.t)
    -> 'a Deferred.t

  type trust_chain =
    [ `Ca_file of string
    | `Ca_path of string
    | `Search_file_first_then_path of
        [ `File of string ] *
        [ `Path of string ]
    ] [@@deriving sexp]

  type openssl =
    [ `OpenSSL of
        [ `Crt_file_path of string ] *
        [ `Key_file_path of string ]
    ] [@@deriving sexp]

  type server = [
    | openssl
    | `TCP
    | `OpenSSL_with_trust_chain of
        (openssl * trust_chain)
  ] [@@deriving sexp]

  val serve :
    ?max_connections:int ->
    ?backlog:int ->
    ?buffer_age_limit:Writer.buffer_age_limit ->
    on_handler_error:[ `Call of ([< Socket.Address.t ] as 'a) -> exn -> unit
                     | `Ignore
                     | `Raise ] ->
    server ->
    ('a, 'b) Tcp.Where_to_listen.t ->
    ('a -> Reader.t -> Writer.t -> unit Deferred.t) ->
    ('a, 'b) Tcp.Server.t Deferred.t
end
