(** X.509 certificate handling using Lwt. *)

(** private material: a certificate chain and a RSA private key *)
type priv          = X509.Certificate.t list * Mirage_crypto_pk.Rsa.priv

(** authenticator *)
type authenticator = X509.Authenticator.t

(** [private_of_pems ~cert ~priv_key] is [priv], after reading the
    private key and certificate chain from the given PEM-encoded
    files. *)
val private_of_pems : cert:Lwt_io.file_name -> priv_key:Lwt_io.file_name -> priv Lwt.t

(** [certs_of_pem file] is [certificates], which are read from the
    PEM-encoded [file]. *)
val certs_of_pem     : Lwt_io.file_name -> X509.Certificate.t list Lwt.t

(** [certs_of_pem_dir dir] is [certificates], which are read from all
    PEM-encoded files in [dir]. *)
val certs_of_pem_dir : Lwt_io.file_name -> X509.Certificate.t list Lwt.t

(** [authenticator methods] constructs an [authenticator] using the
    specified method and data. *)
val authenticator : ?hash_whitelist:Mirage_crypto.Hash.hash list -> ?crls:Lwt_io.file_name ->
  [ `Ca_file of Lwt_io.file_name
  | `Ca_dir  of Lwt_io.file_name
  | `Key_fingerprints of Mirage_crypto.Hash.hash * ([`host] Domain_name.t * Cstruct.t) list
  | `Hex_key_fingerprints of Mirage_crypto.Hash.hash * ([`host] Domain_name.t * string) list
  | `Cert_fingerprints of Mirage_crypto.Hash.hash * ([`host] Domain_name.t * Cstruct.t) list
  | `Hex_cert_fingerprints of Mirage_crypto.Hash.hash * ([`host] Domain_name.t * string) list
  ]
  -> authenticator Lwt.t
