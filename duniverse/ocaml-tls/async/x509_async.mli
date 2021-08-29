open! Core
open! Async

include module type of struct
  include X509
end

module Authenticator : sig
  include module type of struct
    include Authenticator
  end

  module Param : sig
    type t

    val ca_file
      :  ?allowed_hashes:Mirage_crypto.Hash.hash list
      -> ?crls:Filename.t
      -> Filename.t
      -> unit
      -> t

    val ca_dir
      :  ?allowed_hashes:Mirage_crypto.Hash.hash list
      -> ?crls:Filename.t
      -> Filename.t
      -> unit
      -> t

    (** The fingerprint can be collected from a browser or by invoking an openssl command
        like 'openssl x509 -in <pem_file> -noout -fingerprint -sha256' *)
    val cert_fingerprints
      :  Mirage_crypto.Hash.hash
      -> ([ `host ] Domain_name.t * string) list
      -> t

    (** Async programs often don't use [Ptime_clock], so this is provided as a convenience
        function. Relies on [Unix.gettimeofday]. *)
    val time : unit -> Ptime.t option

    val to_authenticator
      :  time:(unit -> Ptime.t option)
      -> t
      -> Authenticator.t Deferred.Or_error.t
  end
end

module Private_key : sig
  include module type of struct
    include Private_key
  end

  val sign
    :  Mirage_crypto.Hash.hash
    -> ?scheme:Key_type.signature_scheme
    -> t
    -> [ `Digest of Cstruct.t | `Message of Cstruct.t ]
    -> string Or_error.t

  val decode_der : contents:string -> t Or_error.t
  val decode_pem : contents:string -> t Or_error.t
  val of_pem_file : Filename.t -> t Deferred.Or_error.t
end

module Public_key : sig
  include module type of struct
    include Public_key
  end

  val verify
    :  Mirage_crypto.Hash.hash
    -> ?scheme:Key_type.signature_scheme
    -> signature:string
    -> t
    -> [ `Digest of string | `Message of string ]
    -> unit Or_error.t

  val decode_der : contents:string -> t Or_error.t
  val decode_pem : contents:string -> t Or_error.t
end

module Certificate : sig
  include module type of struct
    include Certificate
  end

  val decode_pem_multiple : contents:string -> t list Or_error.t
  val decode_pem : contents:string -> t Or_error.t
  val decode_der : contents:string -> t Or_error.t
  val of_pem_file : Filename.t -> t list Deferred.Or_error.t
  val of_pem_directory : directory:Filename.t -> t list Deferred.Or_error.t
end

module Distinguished_name : sig
  include module type of struct
    include Distinguished_name
  end

  val decode_der : contents:string -> t Or_error.t
end

module CRL : sig
  include module type of struct
    include CRL
  end

  val decode_der : contents:string -> t Or_error.t

  val revoke
    :  ?digest:Mirage_crypto.Hash.hash
    -> issuer:Distinguished_name.t
    -> this_update:Ptime.t
    -> ?next_update:Ptime.t
    -> ?extensions:Extension.t
    -> revoked_cert list
    -> Private_key.t
    -> t Or_error.t

  val revoke_certificate
    :  revoked_cert
    -> this_update:Ptime.t
    -> ?next_update:Ptime.t
    -> t
    -> Private_key.t
    -> t Or_error.t

  val revoke_certificates
    :  revoked_cert list
    -> this_update:Ptime.t
    -> ?next_update:Ptime.t
    -> t
    -> Private_key.t
    -> t Or_error.t

  val of_pem_dir : directory:Filename.t -> t list Deferred.Or_error.t
end

module OCSP : sig
  include module type of struct
    include OCSP
  end

  module Request : sig
    include module type of struct
      include Request
    end

    val create
      :  ?certs:Certificate.t list
      -> ?digest:Mirage_crypto.Hash.hash
      -> ?requestor_name:General_name.b
      -> ?key:Private_key.t
      -> cert_id list
      -> t Or_error.t

    val decode_der : contents:string -> t Or_error.t
  end

  module Response : sig
    include module type of struct
      include Response
    end

    val create_success
      :  ?digest:Mirage_crypto.Hash.hash
      -> ?certs:Certificate.t list
      -> ?response_extensions:Extension.t
      -> Private_key.t
      -> responder_id
      -> Ptime.t
      -> single_response list
      -> t Or_error.t

    val responses : t -> single_response list Or_error.t
    val decode_der : contents:string -> t Or_error.t
  end
end

module PKCS12 : sig
  include module type of struct
    include PKCS12
  end

  val decode_der : contents:string -> t Or_error.t

  val verify
    :  string
    -> t
    -> [ `Certificate of Certificate.t
       | `Crl of CRL.t
       | `Decrypted_private_key of Private_key.t
       | `Private_key of Private_key.t
       ]
         list
         Or_error.t
end

module Signing_request : sig
  include module type of struct
    include Signing_request
  end

  val decode_der : ?allowed_hashes:Mirage_crypto.Hash.hash list -> string -> t Or_error.t
  val decode_pem : string -> t Or_error.t

  val create
    :  Distinguished_name.t
    -> ?digest:Mirage_crypto.Hash.hash
    -> ?extensions:Ext.t
    -> Private_key.t
    -> t Or_error.t

  val sign
    :  ?allowed_hashes:Mirage_crypto.Hash.hash list
    -> ?digest:Mirage_crypto.Hash.hash
    -> ?serial:Z.t
    -> ?extensions:Extension.t
    -> t
    -> Private_key.t
    -> Distinguished_name.t
    -> valid_from:Ptime.t
    -> valid_until:Ptime.t
    -> Certificate.t Or_error.t
end
