include S.V3
  with type session = [`Ssl_not_compiled_in]
   and type ssl_version = [`Ssl_not_compiled_in]
   and type ssl_conn = [`Ssl_not_compiled_in]
   and type ssl_opt = [`Ssl_not_compiled_in]
   and type allowed_ciphers =
         [ `Only of string list | `Openssl_default | `Secure ]

