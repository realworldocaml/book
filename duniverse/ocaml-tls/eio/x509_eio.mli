(** X.509 certificate handling using Eio. *)

(** [private_of_pems ~cert ~priv_key] is [priv], after reading the
    private key and certificate chain from the given PEM-encoded
    files. *)
val private_of_pems : cert:_ Eio.Path.t -> priv_key:_ Eio.Path.t -> Tls.Config.certchain

(** [certs_of_pem file] is [certificates], which are read from the
    PEM-encoded [file]. *)
val certs_of_pem     : _ Eio.Path.t -> X509.Certificate.t list

(** [certs_of_pem_dir dir] is [certificates], which are read from all
    PEM-encoded files in [dir]. *)
val certs_of_pem_dir : _ Eio.Path.t -> X509.Certificate.t list

(** [authenticator methods] constructs an [authenticator] using the
    specified method and data. *)
val authenticator : ?allowed_hashes:Mirage_crypto.Hash.hash list -> ?crls:_ Eio.Path.t ->
  [ `Ca_file of _ Eio.Path.t
  | `Ca_dir  of _ Eio.Path.t
  | `Key_fingerprint of Mirage_crypto.Hash.hash * Cstruct.t
  | `Hex_key_fingerprint of Mirage_crypto.Hash.hash * string
  | `Cert_fingerprint of Mirage_crypto.Hash.hash * Cstruct.t
  | `Hex_cert_fingerprint of Mirage_crypto.Hash.hash * string
  ]
  -> X509.Authenticator.t
