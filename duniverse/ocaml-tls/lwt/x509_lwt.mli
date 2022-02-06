(** X.509 certificate handling using Lwt. *)

(** [private_of_pems ~cert ~priv_key] is [priv], after reading the
    private key and certificate chain from the given PEM-encoded
    files. *)
val private_of_pems : cert:Lwt_io.file_name -> priv_key:Lwt_io.file_name -> Tls.Config.certchain Lwt.t

(** [certs_of_pem file] is [certificates], which are read from the
    PEM-encoded [file]. *)
val certs_of_pem     : Lwt_io.file_name -> X509.Certificate.t list Lwt.t

(** [certs_of_pem_dir dir] is [certificates], which are read from all
    PEM-encoded files in [dir]. *)
val certs_of_pem_dir : Lwt_io.file_name -> X509.Certificate.t list Lwt.t

(** [authenticator methods] constructs an [authenticator] using the
    specified method and data. *)
val authenticator : ?allowed_hashes:Mirage_crypto.Hash.hash list -> ?crls:Lwt_io.file_name ->
  [ `Ca_file of Lwt_io.file_name
  | `Ca_dir  of Lwt_io.file_name
  | `Key_fingerprint of Mirage_crypto.Hash.hash * Cstruct.t
  | `Hex_key_fingerprint of Mirage_crypto.Hash.hash * string
  | `Cert_fingerprint of Mirage_crypto.Hash.hash * Cstruct.t
  | `Hex_cert_fingerprint of Mirage_crypto.Hash.hash * string
  ]
  -> X509.Authenticator.t Lwt.t
