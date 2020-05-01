open Async_ssl

include S.V1
  with type session = Ssl.Session.t
   and type ssl_version = Ssl.Version.t
   and type ssl_conn = Ssl.Connection.t
