open Async_ssl
include S.V2
  with type session = Ssl.Session.t
   and type ssl_version = Ssl.Version.t
   and type ssl_conn = Ssl.Connection.t
   and type ssl_opt = Ssl.Opt.t
   and type verify_mode = Ssl.Verify_mode.t
   and type allowed_ciphers =
         [ `Only of string list | `Openssl_default | `Secure ]
