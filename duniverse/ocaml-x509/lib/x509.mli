(** X509 encoding, generation, and validation.

    [X509] is a module for handling X.509 certificates and supplementary
   material (such as public and private RSA or EC keys), as described in
   {{:https://tools.ietf.org/html/rfc5280}RFC 5280}.  X.509 describes a
   hierarchical public key infrastructure, where all trust is delegated to
   certificate authorities (CA).  The task of a CA is to sign certificate
   signing requests (CSR), which turns them into certificates, after
   verification that the requestor is eligible.

    An X.509 certificate is an authentication token: a public key, a subject
   (e.g. server name), a validity period, optionally a purpose (usage), and
   various other optional {{!Extension}Extensions}. The overall approach of
   this package is to support decoding what is present in the real world,
   including weak ciphers (various validation functions support an allow list
   to avoid using weak hashes in chains if needed).

    The public keys of trusted CAs are distributed with the software, or
   configured manually.  When an endpoint connects, it presents its
   certificate chain, which are pairwise signed certificates.  This chain is
   verified: the signatures have to be valid, the last certificate must be
   signed by a trusted CA, the name has to match the expected name, all
   certificates must be valid at the current time, and the purpose of each
   certificate must match its usage.  An alternative validator checks that the
   hash of the server certificate matches the given hash.

    This module uses the [result] type for errors. No provided binging raises
   an exception. Provided submodules include decoders and encoders (ASN.1 DER
   and PEM encoding) of X.509v3 {{!Certificate}certificates},
   {{!Distinguished_name}distinguished names}, {{!Public_key}public keys} and
   {{!Private_key}private keys}
   ({{:http://tools.ietf.org/html/rfc5208}PKCS 8, RFC 5208}), and
   {{!Signing_request}certificate signing requests}
   ({{:http://tools.ietf.org/html/rfc2986}PKCS 10, RFC 2986},
   both use parts of
   {{:https://tools.ietf.org/html/rfc2985}PKCS 9, RFC 2985}),
   {{!Validation} certificate validation} by construction of
   {{!Authenticator} authenticators}.  Name validation, as defined in
   {{:https://tools.ietf.org/html/rfc6125}RFC 6125}, is also implemented.

    The archive format for certificates and private keys,
   {{:https://tools.ietf.org/html/rfc7292}PKCS 12, RFC 7292}, is
   implemented in the {!PKCS12} submodule. While PKCS 12 decryption
   supports the weak algorithm used by default by widely used software (RC2!),
   the encryption path only supports AES.

    Missing is the handling of online certificate status protocol. Some X.509v3
   extensions are not handled, but only parsed, such as name constraints. If any
   extension is marked as critical in a certificate, but not handled, the
   validation will fail.

    {e v0.16.1 - {{:https://github.com/mirleft/ocaml-x509 }homepage}} *)

(** Hostnames (strict, wildcard), used for validation. *)
module Host : sig
  (** The polymorphic variant for hostname validation. *)
  type t = [ `Strict | `Wildcard ] * [ `host ] Domain_name.t

  (** [pp ppf host] pretty-prints [host] on [ppf]: if it is a wildcard, "*." is
      prefixed to the domain name. *)
  val pp : t Fmt.t

  (** The module for a set of hostnames. *)
  module Set : sig
    include Set.S with type elt = t

    (** [pp ppf host_set] pretty-prints the [host_set]. *)
    val pp : t Fmt.t
  end
end

(** Types of keys *)
module Key_type : sig
  (** The polymorphic variant of key types. *)
  type t = [ `RSA | `ED25519 | `P224 | `P256 | `P384 | `P521 ]

  val strings : (string * t) list
  (** [strings] is an associative list of string and key_type pairs. Useful for
      {{:https://erratique.ch/software/cmdliner}cmdliner} (Arg.enum). *)

  val to_string : t -> string
  (** [to_string kt] is a string representation of [kt]. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is [Ok key_type] if the string could be decoded as
      [key_type], or an [Error _]. *)

  val pp : t Fmt.t
  (** [pp ppf t] is a pretty printer of [t] on [ppf]. *)

  (** The type of signature schemes. *)
  type signature_scheme = [ `RSA_PSS | `RSA_PKCS1 | `ECDSA | `ED25519 ]

  val pp_signature_scheme : signature_scheme Fmt.t
  (** [pp_signature_scheme ppf s] is a pretty-printer of [s] on [ppf]. *)

  val supports_signature_scheme : t -> signature_scheme -> bool
  (** [supports_scheme key_type scheme] is [true] if the signature [scheme]
      is supported with [key type]. *)
end

(** Public keys *)
module Public_key : sig
  (** Public keys as specified in {{:http://tools.ietf.org/html/rfc5208}PKCS 8}
      are supported in this module. *)

  (** {1 The type for public keys} *)

  (** The polymorphic variant of public keys, with
      {{:http://tools.ietf.org/html/rfc5208}PKCS 8} encoding and decoding to
      PEM. *)
  type t = [
    | `RSA of Mirage_crypto_pk.Rsa.pub
    | `ED25519 of Mirage_crypto_ec.Ed25519.pub
    | `P224 of Mirage_crypto_ec.P224.Dsa.pub
    | `P256 of Mirage_crypto_ec.P256.Dsa.pub
    | `P384 of Mirage_crypto_ec.P384.Dsa.pub
    | `P521 of Mirage_crypto_ec.P521.Dsa.pub
  ]

  (** {1 Operations on public keys} *)

  (** [pp ppf pub] pretty-prints the public key [pub] on [ppf]. *)
  val pp : t Fmt.t

  (** [id public_key] is [digest], the 160-bit [`SHA1] hash of the BIT
      STRING subjectPublicKey (excluding tag, length, and number of
      unused bits) for publicKeyInfo of [public_key].

      {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.2}RFC 5280, 4.2.1.2, variant (1)} *)
  val id : t -> Cstruct.t

  (** [fingerprint ?hash public_key] is [digest], the hash (by
      default SHA256) of the DER encoded public key (equivalent to
      [openssl x509 -noout -pubkey | openssl pkey -pubin -outform DER | openssl dgst -HASH]).  *)
  val fingerprint : ?hash:Mirage_crypto.Hash.hash -> t -> Cstruct.t

  (** [key_type public_key] is its [key_type]. *)
  val key_type : t -> Key_type.t

  (** {1 Cryptographic verify operation} *)

  (** [verify hash ~scheme ~signature key data] verifies whether the [signature]
      on [data] is valid using the [key], or not. The [signature] must be in
      ASN.1 DER encoding. The [scheme] defaults to [`RSA_PSS] for RSA,
      [`ED25519] for ED25519, and [`ECDSA] for other EC keys. *)
  val verify : Mirage_crypto.Hash.hash ->
    ?scheme:Key_type.signature_scheme ->
    signature:Cstruct.t -> t ->
    [ `Message of Cstruct.t | `Digest of Cstruct.t ] ->
    (unit, [> `Msg of string ]) result

  (** {1 Decoding and encoding in ASN.1 DER and PEM format} *)

  (** [encode_der pk] is [buffer], the ASN.1 encoding of the given public key. *)
  val encode_der : t -> Cstruct.t

  (** [decode_der buffer] is [pubkey], the public key of the ASN.1 encoded buffer. *)
  val decode_der : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [decode_pem pem] is [t], where the public key of [pem] is extracted *)
  val decode_pem : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [encode_pem public_key] is [pem], the pem encoded public key. *)
  val encode_pem : t -> Cstruct.t
end

(** Private keys *)
module Private_key : sig
  (** Private keys as defined in
      {{:http://tools.ietf.org/html/rfc5208}PKCS 8}: decoding and encoding
      in PEM format  *)

  (** {1 The type for private keys} *)

  (** The polymorphic variant of private keys. *)
  type t = [
    | `RSA of Mirage_crypto_pk.Rsa.priv
    | `ED25519 of Mirage_crypto_ec.Ed25519.priv
    | `P224 of Mirage_crypto_ec.P224.Dsa.priv
    | `P256 of Mirage_crypto_ec.P256.Dsa.priv
    | `P384 of Mirage_crypto_ec.P384.Dsa.priv
    | `P521 of Mirage_crypto_ec.P521.Dsa.priv
  ]

  (** {1 Constructing private keys} *)

  (** [generate ~seed ~bits type] generates a private key of the given
      key type. The argument [bits] is only used for the bit length of RSA keys.
      If [seed] is provided, this is used to seed the random number generator.
  *)
  val generate : ?seed:Cstruct.t -> ?bits:int -> Key_type.t -> t

  (** [of_cstruct data type] decodes the buffer as private key. Only supported
      for elliptic curve keys. *)
  val of_cstruct : Cstruct.t -> Key_type.t -> (t, [> `Msg of string ]) result

  (** [of_string ~seed_or_data ~bits type data] attempts to decode the data as a
      private key. If [seed_or_data] is provided and [`Seed], the [data] is
      taken as seed and {!generate} is used. If it is [`Data], {!of_cstruct} is
      used with the Base64 decoded [data]. By default, if [type] is RSA, the
      data is used as seed, otherwise directly as the private key data. *)
  val of_string : ?seed_or_data:[`Seed | `Data] -> ?bits:int -> Key_type.t ->
    string -> (t, [> `Msg of string ]) result

  (** {1 Operations on private keys} *)

  (** [key_type priv] is the key type of [priv]. *)
  val key_type : t -> Key_type.t

  (** [public priv] is the corresponding public key of [priv]. *)
  val public : t -> Public_key.t

  (** {1 Cryptographic sign operation} *)

  (** [sign hash ~scheme key data] signs [data] with [key] using [hash] and
      [scheme]. If [data] is [`Message _], the [hash] will be applied before
      the signature. The [scheme] defaults to [`RSA_PSS] for RSA keys,
      [`ED25519] for ED25519, and [`ECDSA] for other EC keys. *)
  val sign : Mirage_crypto.Hash.hash ->
    ?scheme:Key_type.signature_scheme ->
    t -> [ `Digest of Cstruct.t | `Message of Cstruct.t ] ->
    (Cstruct.t, [> `Msg of string ]) result

  (** {1 Decoding and encoding in ASN.1 DER and PEM format} *)

  (** [decode_der der] is [t], where the private key of [der] is
      extracted. It must be in PKCS8 (RFC 5208, Section 5) PrivateKeyInfo
      structure. *)
  val decode_der : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [encode_der key] is [der], the encoded private key as PKCS8 (RFC 5208,
      Section 5) PrivateKeyInfo structure. *)
  val encode_der : t -> Cstruct.t

  (** [decode_pem pem] is [t], where the private key of [pem] is
      extracted. Both RSA PRIVATE KEY and PRIVATE KEY stanzas are supported. *)
  val decode_pem : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [encode_pem key] is [pem], the encoded private key (using [PRIVATE KEY]). *)
  val encode_pem : t -> Cstruct.t
end

(** X.500 distinguished name *)
module Distinguished_name : sig

  (** The variant of a relative distinguished name component, as defined in
    X.500: an attribute type and value. *)
  type attribute =
    | CN of string
    | Serialnumber of string
    | C of string
    | L of string
    | ST of string
    | O of string
    | OU of string
    | T of string
    | DNQ of string
    | Mail of string
    | DC of string
    | Given_name of string
    | Surname of string
    | Initials of string
    | Pseudonym of string
    | Generation of string
    | Street of string
    | Userid of string
    | Other of Asn.oid * string

  (** Relative_distinguished_name is a set of attributes. *)
  module Relative_distinguished_name : Set.S with type elt = attribute

  (** A distinguished name is a list of relative distinguished names, starting
      with the most significant component. *)
  type t = Relative_distinguished_name.t list

  (** [equal a b] is [true] if the distinguished names [a] and [b] are equal. *)
  val equal : t -> t -> bool

  (** [make_pp ()] creates a customized pretty-printer for {!t}.

      @param format
        Determines RDN order, escaping rules, and the default spacing:
        - [`RFC4514] produces the
          {{:https://tools.ietf.org/html/rfc4514}RFC4514}.
          RDNs are written in reverse order of the ASN.1 representation and
          spacing defaults to tight.
        - [`OpenSSL] produces the a format similar to OpenSSL.  RDNs are written
          in the order of the ASN.1 representation, and spacing defaults to
          loose.
        - [`OSF] emits RDNs in the order they occur in the ASN.1 representation,
          each prefixed by a slashes, using tight spacing.  This format is
          designed by analogy to RFC4514, substituting slash for comma an
          semicolon, and may currently not be fully compliant with the OSF
          specifications.

      @param spacing
        Determines whether to add space around separators:

        - [`Tight] to not add any redundant space,
        - [`Medium] to add space after comma and around plus signs, and
        - [`Loose] to also add space around equality signs.

        This parameter is currently ignored for the OSF format.

      The pretty-printer can be wrapped in a box to control line breaking and
      set it apart, otherwise the RDN components will flow with the surrounding
      text. *)
  val make_pp :
    format: [`RFC4514 | `OpenSSL | `OSF] ->
    ?spacing: [`Tight | `Medium | `Loose] ->
    unit -> t Fmt.t

  (** [pp ppf dn] pretty-prints the distinguished name. This is currently
      [Fmt.hbox (make_pp ~format:`OSF ())]. If your application relies on the
      precise format, it is advicable to create a custom formatter with
      {!make_pp} to guard against future changes to the default format. *)
  val pp : t Fmt.t

  (** [common_name t] is [Some x] if the distinguished name [t] contains a
      [CN x], [None] otherwise. *)
  val common_name : t -> string option

  (** [decode_der cs] is [dn], the ASN.1 decoded distinguished name of [cs]. *)
  val decode_der : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [encode_der dn] is [cstruct], the ASN.1 encoded representation of the
      distinguished name [dn]. *)
  val encode_der : t -> Cstruct.t
end

(** A list of [general_name]s is the value of both
    {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.6}subjectAltName}
    and
    {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.7}IssuerAltName}
    extension. *)
module General_name : sig
  type _ k =
    | Other : Asn.oid -> string list k
    | Rfc_822 : string list k
    | DNS : string list k
    | X400_address : unit k
    | Directory : Distinguished_name.t list k
    | EDI_party : (string option * string) list k
    | URI : string list k
    | IP : Cstruct.t list k
    | Registered_id : Asn.oid list k

  include Gmap.S with type 'a key = 'a k

  val pp : t Fmt.t
end


(** X.509v3 extensions *)
module Extension : sig

  (** The polymorphic variant of
  {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.3}key usages}. *)
  type key_usage = [
    | `Digital_signature
    | `Content_commitment
    | `Key_encipherment
    | `Data_encipherment
    | `Key_agreement
    | `Key_cert_sign
    | `CRL_sign
    | `Encipher_only
    | `Decipher_only
  ]

  (** The polymorphic variant of
  {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.12}extended key usages}. *)
  type extended_key_usage = [
    | `Any
    | `Server_auth
    | `Client_auth
    | `Code_signing
    | `Email_protection
    | `Ipsec_end
    | `Ipsec_tunnel
    | `Ipsec_user
    | `Time_stamping
    | `Ocsp_signing
    | `Other of Asn.oid
  ]

  (** The authority key identifier, as present in the
      {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.1}Authority Key Identifier}
      extension. *)
  type authority_key_id = Cstruct.t option * General_name.t * Z.t option

  (** The private key usage period, as defined in
      {{:https://tools.ietf.org/html/rfc3280#section-4.2.1.4}RFC 3280}. *)
  type priv_key_usage_period = [
    | `Interval   of Ptime.t * Ptime.t
    | `Not_after  of Ptime.t
    | `Not_before of Ptime.t
  ]

  (** Name constraints, as defined in
      {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.10}RFC 5280}. *)
  type name_constraint = (General_name.b * int * int option) list

  (** Certificate policies, the
      {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.4}policy extension}. *)
  type policy = [ `Any | `Something of Asn.oid ]

  (** Type of
      {{:https://tools.ietf.org/html/rfc5280#section-5.3.1}revocation reasons}
      for a given distribution point. *)
  type reason = [
    | `Unspecified
    | `Key_compromise
    | `CA_compromise
    | `Affiliation_changed
    | `Superseded
    | `Cessation_of_operation
    | `Certificate_hold
    | `Remove_from_CRL
    | `Privilege_withdrawn
    | `AA_compromise
  ]

  (** Distribution point name, either a full one using general names, or a
      relative one using a distinguished name. *)
  type distribution_point_name =
    [ `Full of General_name.t
    | `Relative of Distinguished_name.t ]

  (** {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.13}Distribution point},
      consisting of an optional name, an optional list of allowed reasons, and
      an optional issuer. *)
  type distribution_point =
    distribution_point_name option *
    reason list option *
    General_name.t option

  (** The type of an extension: the critical flag and the value itself. *)
  type 'a extension = bool * 'a

  (** The type of supported
      {{:https://tools.ietf.org/html/rfc5280#section-4.2}X509v3} and
      {{:https://tools.ietf.org/html/rfc5280#section-5.2}CRL} extensions. *)
  type _ k =
    | Unsupported : Asn.oid -> Cstruct.t extension k
    | Subject_alt_name : General_name.t extension k
    | Authority_key_id : authority_key_id extension k
    | Subject_key_id : Cstruct.t extension k
    | Issuer_alt_name : General_name.t extension k
    | Key_usage : key_usage list extension k
    | Ext_key_usage : extended_key_usage list extension k
    | Basic_constraints : (bool * int option) extension k
    | CRL_number : int extension k
    | Delta_CRL_indicator : int extension k
    | Priv_key_period : priv_key_usage_period extension k
    | Name_constraints : (name_constraint * name_constraint) extension k
    | CRL_distribution_points : distribution_point list extension k
    | Issuing_distribution_point : (distribution_point_name option * bool * bool * reason list option * bool * bool) extension k
    | Freshest_CRL : distribution_point list extension k
    | Reason : reason extension k
    | Invalidity_date : Ptime.t extension k
    | Certificate_issuer : General_name.t extension k
    | Policies : policy list extension k

  include Gmap.S with type 'a key = 'a k

  (** [critical ext_key ext_value] is the critical bit in [ext_value]. *)
  val critical : 'a key -> 'a -> bool

  (** [pp ppf ext_map] pretty-prints the extension map. *)
  val pp : t Fmt.t
end

(** X509v3 certificate *)
module Certificate : sig

  (** [decode_pkcs1_digest_info buffer] is [hash, signature], the hash and raw
      signature of the given [buffer] in ASN.1 DER encoding, or an error. *)
  val decode_pkcs1_digest_info : Cstruct.t ->
    (Mirage_crypto.Hash.hash * Cstruct.t, [> `Msg of string ]) result

  (** [encode_pkcs1_digest_info (hash, signature)] is [data], the ASN.1 DER
      encoded hash and signature. *)
  val encode_pkcs1_digest_info : Mirage_crypto.Hash.hash * Cstruct.t -> Cstruct.t

  (** {1 Abstract certificate type} *)

  (** The abstract type of a certificate. *)
  type t

  (** [pp ppf cert] pretty-prints the certificate. *)
  val pp : t Fmt.t

  (** {1 Encoding and decoding in ASN.1 DER and PEM format} *)

  (** [decode_der cstruct] is [certificate], the ASN.1 decoded [certificate]
      or an error. *)
  val decode_der : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [encode_der certificate] is [cstruct], the ASN.1 encoded representation of
      the [certificate]. *)
  val encode_der  : t -> Cstruct.t

  (** [decode_pem_multiple pem] is [t list], where all certificates of the [pem]
       are extracted *)
  val decode_pem_multiple : Cstruct.t -> (t list, [> `Msg of string ]) result

  (** [decode_pem pem] is [t], where the single certificate of the
      [pem] is extracted *)
  val decode_pem : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [encode_pem_multiple certificates] is [pem], the pem encoded certificates. *)
  val encode_pem_multiple : t list -> Cstruct.t

  (** [encode_pem certificate] is [pem], the pem encoded certificate. *)
  val encode_pem : t -> Cstruct.t

  (** {1 Operations on certificates} *)

  (** [supports_keytype certificate key_type] is [result], whether public key of
      the [certificate] matches the given [key_type]. *)
  val supports_keytype : t -> Key_type.t -> bool

  (** [public_key certificate] is [pk], the public key of the [certificate]. *)
  val public_key : t -> Public_key.t

  (** [signature_algorithm certificate] is the algorithm used for the signature. *)
  val signature_algorithm : t ->
    (Key_type.signature_scheme * Mirage_crypto.Hash.hash) option

  (** [hostnames certficate] is the set of domain names this
      [certificate] is valid for.  Currently, these are the DNS names of the
      {{:https://tools.ietf.org/html/rfc5280#section-4.2.1.6}Subject Alternative Name}
      extension, if present, or otherwise the singleton set containing the common
      name of the certificate subject. *)
  val hostnames : t -> Host.Set.t

  (** [supports_hostname certificate hostname] is [result], whether the
      [certificate] contains the given [hostname], using {!hostnames}. *)
  val supports_hostname : t -> [`host] Domain_name.t -> bool

  (** [ips certificate] are the IP addresses the certificate is valid
      for (as specified in SubjectAlternativeName extensioni). *)
  val ips : t -> Ipaddr.Set.t

  (** [supports_ip cert ip] is [true] if the [ip] is mentioned in
      the SubjectAlternativeName extension, [false] otherwise. *)
  val supports_ip : t -> Ipaddr.t -> bool

  (** [fingerprint hash cert] is [digest], the digest of [cert] using the
      specified [hash] algorithm *)
  val fingerprint : Mirage_crypto.Hash.hash -> t -> Cstruct.t

  (** [subject certificate] is [dn], the subject as distinguished name of
      the [certificate]. *)
  val subject : t -> Distinguished_name.t

  (** [issuer certificate] is [dn], the issuer as distinguished name of
      the [certificate]. *)
  val issuer : t -> Distinguished_name.t

  (** [serial certificate] is [sn], the serial number of the [certificate]. *)
  val serial : t -> Z.t

  (** [validity certificate] is [from, until], the validity of the certificate. *)
  val validity : t -> Ptime.t * Ptime.t

  (** [extensions certificate] is the extension map of [certificate]. *)
  val extensions : t -> Extension.t
end

(** Chain Validation. *)
module Validation : sig
  (** A chain of pairwise signed X.509 certificates is sent to the endpoint,
      which use these to authenticate the other endpoint.  Usually a set of
      trust anchors is configured on the endpoint, and the chain needs to be
      rooted in one of the trust anchors.  In reality, chains may be incomplete
      or reversed, and there can be multiple paths from the leaf certificate to
      a trust anchor.

      RFC 5280 specifies a {{:https://tools.ietf.org/html/rfc5280#section-6}path
      validation} algorithm for authenticating chains, but this does not handle
      multiple possible paths.  {{:https://tools.ietf.org/html/rfc4158}RFC 4158}
      describes possible path building strategies.

      This module provides path building, chain of trust verification, trust
      anchor (certificate authority) validation, and validation via a
      fingerprint list (for a trust on first use implementation).
  *)

  (** The type of signature verification errors. *)
  type signature_error = [
    | `Bad_signature of Distinguished_name.t * string
    | `Bad_encoding of Distinguished_name.t * string * Cstruct.t
    | `Hash_not_allowed of Distinguished_name.t * Mirage_crypto.Hash.hash
    | `Unsupported_keytype of Distinguished_name.t * Public_key.t
    | `Unsupported_algorithm of Distinguished_name.t * string
    | `Msg of string
  ]

  (** [pp_signature_error ppf sige] pretty-prints the signature error [sige] on
      [ppf]. *)
  val pp_signature_error : signature_error Fmt.t

  (** {1 Certificate Authorities} *)

  (** The polymorphic variant of possible certificate authorities failures. *)
  type ca_error = [
    | signature_error
    | `CAIssuerSubjectMismatch of Certificate.t
    | `CAInvalidVersion of Certificate.t
    | `CACertificateExpired of Certificate.t * Ptime.t option
    | `CAInvalidExtensions of Certificate.t
  ]

  (** [pp_ca_error ppf ca_error] pretty-prints the CA error [ca_error]. *)
  val pp_ca_error : ca_error Fmt.t

  (** [valid_ca ~allowed_hashes ~time certificate] is [result], which is [Ok ()]
      if the given certificate is self-signed with any hash algorithm of
      [hash_allowlist] (defaults to any hash), it is valid at [time], its
      extensions are not present (if X.509 version 1 certificate), or are
      appropriate for a CA (BasicConstraints is present and true, KeyUsage
      extension contains keyCertSign). *)
  val valid_ca : ?allowed_hashes:Mirage_crypto.Hash.hash list -> ?time:Ptime.t ->
    Certificate.t -> (unit, [> ca_error ]) result

  (** [valid_cas ~allowed_hashes ~time certificates] is [valid_certificates],
      only those certificates which pass the {!valid_ca} check. *)
  val valid_cas : ?allowed_hashes:Mirage_crypto.Hash.hash list -> ?time:Ptime.t ->
    Certificate.t list -> Certificate.t list

  (** {1 Chain of trust verification} *)

  (** The polymorphic variant of a leaf certificate validation error. *)
  type leaf_validation_error = [
    | `LeafCertificateExpired of Certificate.t * Ptime.t option
    | `LeafInvalidIP of Certificate.t * Ipaddr.t option
    | `LeafInvalidName of Certificate.t * [`host] Domain_name.t option
    | `LeafInvalidVersion of Certificate.t
    | `LeafInvalidExtensions of Certificate.t
  ]

  (** The polymorphic variant of a chain validation error. *)
  type chain_validation_error = [
    | `IntermediateInvalidExtensions of Certificate.t
    | `IntermediateCertificateExpired of Certificate.t * Ptime.t option
    | `IntermediateInvalidVersion of Certificate.t
    | `ChainIssuerSubjectMismatch of Certificate.t * Certificate.t
    | `ChainAuthorityKeyIdSubjectKeyIdMismatch of Certificate.t * Certificate.t
    | `ChainInvalidPathlen of Certificate.t * int
    | `EmptyCertificateChain
    | `NoTrustAnchor of Certificate.t
    | `Revoked of Certificate.t
  ]

  (** [build_paths server rest] is [paths], which are all possible certificate
      paths starting with [server].  These chains (C1..Cn) fulfill the predicate
      that each certificate Cn is issued by the next one in the chain (C(n+1)):
      the issuer of Cn matches the subject of C(n+1).  This is as described in
      {{:https://tools.ietf.org/html/rfc4158}RFC 4158}. *)
  val build_paths : Certificate.t -> Certificate.t list -> Certificate.t list list

  (** The polymorphic variant of a chain validation error: either the leaf
      certificate is problematic, or the chain itself. *)
  type chain_error = [
    | signature_error
    | leaf_validation_error
    | chain_validation_error
  ]

  (** [pp_chain_error ppf chain_error] pretty-prints the [chain_error]. *)
  val pp_chain_error : chain_error Fmt.t

  (** [verify_chain ~ip ~host ~time ~revoked ~allowed_hashes ~anchors chain] is
      [result], either [Ok] and the trust anchor used to verify the chain, or
      [Error] and the chain error.  RFC 5280 describes the implemented
      {{:https://tools.ietf.org/html/rfc5280#section-6.1}path validation}
      algorithm: The validity period of the given certificates is checked
      against the [time].  The signature algorithm must be present in
      [allowed_hashes] (defaults to SHA-2).  The X509v3 extensions of the
      [chain] are checked, then a chain of trust from [anchors] to the server
      certificate is validated.  The path length constraints are checked.  The
      server certificate is checked to contain the given [host], using
      {!Certificate.hostnames}. If [ip] is specified, the certificate is checked
      to contain the given [ip], using {!Certificate.ips}. The returned
      certificate is the root of the chain, a member of the given list of
      [anchors]. *)
  val verify_chain : ?ip:Ipaddr.t -> host:[`host] Domain_name.t option ->
    time:(unit -> Ptime.t option) ->
    ?revoked:(issuer:Certificate.t -> cert:Certificate.t -> bool) ->
    ?allowed_hashes:Mirage_crypto.Hash.hash list ->
    anchors:(Certificate.t list) -> Certificate.t list ->
    (Certificate.t, [> chain_error ]) result

  (** The polymorphic variant of a fingerprint validation error. *)
  type fingerprint_validation_error = [
    | `InvalidFingerprint of Certificate.t * Cstruct.t * Cstruct.t
  ]

  (** The polymorphic variant of validation errors. *)
  type validation_error = [
    | signature_error
    | leaf_validation_error
    | fingerprint_validation_error
    | `EmptyCertificateChain
    | `InvalidChain
  ]

  (** [pp_validation_error ppf validation_error] pretty-prints the
      [validation_error]. *)
  val pp_validation_error : validation_error Fmt.t

  type r = ((Certificate.t list * Certificate.t) option, validation_error) result

  (** [verify_chain_of_trust ~ip ~host ~time ~revoked ~allowed_hashes ~anchors certificates]
      is [result].  First, all possible paths are constructed using the
      {!build_paths} function, the first certificate of the chain is verified to
      be a valid leaf certificate (no BasicConstraints extension) and contains
      the given [host] (using {!Certificate.hostnames}) or [ip] if specified
      (using {!Certificate.ips}; if some path is valid, using
      {!verify_chain}, the result will be [Ok] and contain the actual
      certificate chain and the trust anchor. *)
  val verify_chain_of_trust :
    ?ip:Ipaddr.t -> host:[`host] Domain_name.t option ->
    time:(unit -> Ptime.t option) ->
    ?revoked:(issuer:Certificate.t -> cert:Certificate.t -> bool) ->
    ?allowed_hashes:Mirage_crypto.Hash.hash list ->
    anchors:(Certificate.t list) -> Certificate.t list -> r

  (** {1 Fingerprint verification} *)

  (** [trust_key_fingerprint ~ip ~host ~time ~hash ~fingerprint certificates] is
      [result], the first element of [certificates] is verified against the
      given [fingerprint] using {!Public_key.fingerprint}.  If [time] is
      provided, the certificate has to be valid at the given timestamp.  If
      [host] is provided, the certificate is checked for the given [host]
      (using {!Certificate.hostnames}). If [ip] is provided, the certificate is
      checked to include this IP address (using {!Certificate.ips}). *)
  val trust_key_fingerprint :
    ?ip:Ipaddr.t -> host:[`host] Domain_name.t option ->
    time:(unit -> Ptime.t option) -> hash:Mirage_crypto.Hash.hash ->
    fingerprint:Cstruct.t -> Certificate.t list -> r

  (** [trust_cert_fingerprint host ~time ~hash ~fingerprint certificates] is
      [result], the first element of [certificates] is verified to match the
      given [fingerprint] using {!Certificate.fingerprint}.  If [time] is
      provided, the certificate is checked to be valid in at the given
      timestamp. If [host] is provided, the certificate is checked for the given
      [host] (using {!Certificate.hostnames}). If [ip] is provided, the
      certificate is checked to include this IP address (using
      {!Certificate.ips}). Note that
      {{!trust_key_fingerprint}public key pinning} has
      {{:https://www.imperialviolet.org/2011/05/04/pinning.html} advantages}
      over certificate pinning. *)
  val trust_cert_fingerprint :
    ?ip:Ipaddr.t -> host:[`host] Domain_name.t option ->
    time:(unit -> Ptime.t option) -> hash:Mirage_crypto.Hash.hash ->
    fingerprint:Cstruct.t -> Certificate.t list -> r
end

(** Certificate Signing request *)

(** A certificate authority (CA) deals with
    {{:https://tools.ietf.org/html/rfc2986}PKCS 10 certificate signing requests},
    their construction and encoding, and provisioning using a private key to
    generate a certificate with a signature thereof. *)
module Signing_request : sig
  (** The abstract type of a (self-signed) certification request. *)
  type t

  (** {1 Decoding and encoding in ASN.1 DER and PEM format} *)

  (** [decode_der ~allowed_hashes cstruct] is [signing_request], the ASN.1
      decoded [cstruct] or an error. The signature on the signing request
      is validated, and its hash algorithm must be in [allowed_hashes] (by
      default only SHA-2 is accepted). *)
  val decode_der : ?allowed_hashes:Mirage_crypto.Hash.hash list -> Cstruct.t ->
    (t, [> `Msg of string ]) result

  (** [encode_der sr] is [cstruct], the ASN.1 encoded representation of the [sr]. *)
  val encode_der : t -> Cstruct.t

  (** [decode_pem pem] is [t], where the single signing request of the [pem] is extracted *)
  val decode_pem : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [encode_pem signing_request] is [pem], the pem encoded signing request. *)
  val encode_pem : t -> Cstruct.t

  (** {1 Construction of a signing request} *)

  module Ext : sig
    (** The GADT of certificate request extensions, as defined in
        {{:http://tools.ietf.org/html/rfc2985}PKCS 9 (RFC 2985)}. *)
    type _ k =
      | Password : string k
      | Name : string k
      | Extensions : Extension.t k

    include Gmap.S with type 'a key = 'a k

    val pp : t Fmt.t
  end

  (** The raw request info of a
      {{:https://tools.ietf.org/html/rfc2986#section-4}PKCS 10 certification request info}. *)
  type request_info = {
    subject    : Distinguished_name.t ;
    public_key : Public_key.t ;
    extensions : Ext.t ;
  }

  (** [info signing_request] is {!request_info}, the information inside the
      signing_request. *)
  val info : t -> request_info

  (** [signature_algorithm signing_request] is the algorithm used for the signature. *)
  val signature_algorithm : t ->
    (Key_type.signature_scheme * Mirage_crypto.Hash.hash) option

  (** [hostnames signing_request] is the set of domain names this
      [signing_request] is requesting. This is either the content of the DNS
      entries of the SubjectAlternativeName extension, or the common name of the
      [signing_request]. *)
  val hostnames : t -> Host.Set.t

  (** [create subject ~digest ~extensions private] creates [signing_request],
      a certification request using the given [subject], [digest] (defaults to
      [`SHA256]) and list of [extensions]. *)
  val create : Distinguished_name.t -> ?digest:Mirage_crypto.Hash.hash ->
    ?extensions:Ext.t -> Private_key.t -> (t, [> `Msg of string ]) result

  (** {1 Provision a signing request to a certificate} *)

  (** [sign signing_request ~valid_from ~valid_until ~allowed_hashes ~digest ~serial ~extensions ~subject private issuer]
      creates [certificate], a signed certificate.  Signing can fail if the
      signature on the [signing_request] is invalid, or its hash algorithm does
      not occur in [allowed_hashes] (default all SHA-2 algorithms).  Public key
      and subject are taken from the [signing_request] unless [subject] is
      passed, the [extensions] are added to the X.509 certificate.  The
      [private] key is used to sign the certificate, the [issuer] is recorded
      in the certificate.  The digest defaults to [`SHA256].  The [serial]
      defaults to a random value between 1 and 2^64.  Certificate version is
      always 3.  Please note that the extensions in the [signing_request] are
      ignored, you can pass them using:

{[match Ext.find Extensions (info csr).extensions with
| Ok ext -> ext
| Error _ -> Extension.empty
]} *)
  val sign : t -> valid_from:Ptime.t -> valid_until:Ptime.t ->
    ?allowed_hashes:Mirage_crypto.Hash.hash list ->
    ?digest:Mirage_crypto.Hash.hash -> ?serial:Z.t -> ?extensions:Extension.t ->
    ?subject:Distinguished_name.t ->
    Private_key.t -> Distinguished_name.t ->
    (Certificate.t, Validation.signature_error) result
end

(** X.509 Certificate Revocation Lists. *)
module CRL : sig
  (** A certificate revocation list is a signed structure consisting of an
      issuer, a timestamp, possibly a timestamp when to expect the next update,
      and a list of revoked certificates (represented by a serial, a revocation
      date, and extensions (e.g. reason) - see
      {{:https://tools.ietf.org/html/rfc5280#section-5.2}RFC 5280 section 5.2}
      for a list of available extensions (not enforced)).  It also may contain
      any extensions, e.g. a CRL number and whether it is partial or complete. *)

  (** The type of a revocation list, kept abstract. *)
  type t

  (** {1 Encoding and decoding in ASN.1 DER format} *)

  (** [encode_der crl] is [buffer], the ASN.1 DER encoding of the given
      certificate revocation list. *)
  val encode_der : t -> Cstruct.t

  (** [decode_der buffer] is [crl], the certificate revocation list of the
      ASN.1 encoded buffer. *)
  val decode_der : Cstruct.t -> (t, [> `Msg of string ]) result

  (** {1 Operations on CRLs} *)

  (** [issuer c] is the issuer of the revocation list. *)
  val issuer : t -> Distinguished_name.t

  (** [this_update t] is the timestamp of the revocation list. *)
  val this_update : t -> Ptime.t

  (** [next_update t] is either [None] or [Some ts], the timestamp of the next
      update. *)
  val next_update : t -> Ptime.t option

  (** The type of a revoked certificate, which consists of a serial number, the
      revocation date, and possibly extensions.  See
      {{:https://tools.ietf.org/html/rfc5280#section-5.3}RFC 5280 section 5.3}
      for allowed extensions (not enforced). *)
  type revoked_cert = {
    serial : Z.t ;
    date : Ptime.t ;
    extensions : Extension.t
  }

  (** [reason revoked] extracts the [Reason] extension from [revoked] if
      present. *)
  val reason : revoked_cert -> Extension.reason option

  (** [revoked_certificates t] is the list of revoked certificates of the
      revocation list. *)
  val revoked_certificates : t -> revoked_cert list

  (** [extensions t] is the list of extensions, see RFC 5280 section 5.2 for
      possible values. *)
  val extensions : t -> Extension.t

  (** [crl_number t] is the number of the CRL. *)
  val crl_number : t -> int option

  (** [signature_algorithm t] is the algorithm used for the signature. *)
  val signature_algorithm : t ->
    (Key_type.signature_scheme * Mirage_crypto.Hash.hash) option

  (** {1 Validation and verification of CRLs} *)

  (** [validate t ~allowed_hashes pk] validates the digital signature of the
      revocation list. The [allowed_hashes] defaults to SHA-2. *)
  val validate : t -> ?allowed_hashes:Mirage_crypto.Hash.hash list ->
    Public_key.t -> (unit, [> Validation.signature_error ]) result

  (** The type of CRL verification errors. *)
  type verification_error = [
    | Validation.signature_error
    | `Issuer_subject_mismatch of Distinguished_name.t * Distinguished_name.t
    | `Not_yet_valid of Distinguished_name.t * Ptime.t * Ptime.t
    | `Next_update_scheduled of Distinguished_name.t * Ptime.t * Ptime.t
  ]

  (** [pp_verification_error ppf vere] pretty-prints the CRL verification error
      [vere] on [ppf]. *)
  val pp_verification_error : verification_error Fmt.t

  (** [verify t ~allowed_hashes ~time cert] verifies that the issuer of [t]
      matches the subject of [cert], and validates the digital signature of the
      revocation list.  The used hash algorithm must be in the [allowed_hashes]
      (defaults to SHA-2). If [time] is provided, it must be after [this_update]
      and before [next_update] of [t]. *)
  val verify : t -> ?allowed_hashes:Mirage_crypto.Hash.hash list ->
    ?time:Ptime.t -> Certificate.t -> (unit, [> verification_error ]) result

  (** [is_revoked ~allowed_hashes ~issuer ~cert crls] is [true] if there exists
      a revocation of [cert] in [crls] which is signed by the [issuer].  The
      subject of [issuer] must match the issuer of the crl.  The hash algorithm
      used for signing must be in the [allowed_hashes] (defaults to SHA-2).  *)
  val is_revoked : ?allowed_hashes:Mirage_crypto.Hash.hash list ->
    issuer:Certificate.t -> cert:Certificate.t -> t list -> bool

  (** {1 Construction and signing of CRLs} *)

  (** [revoked ~digest ~issuer ~this_update ~next_update ~extensions certs priv]
      constructs a revocation list with the given parameters. *)
  val revoke : ?digest:Mirage_crypto.Hash.hash ->
    issuer:Distinguished_name.t ->
    this_update:Ptime.t -> ?next_update:Ptime.t ->
    ?extensions:Extension.t ->
    revoked_cert list -> Private_key.t -> (t, [> `Msg of string ]) result

  (** [revoke_certificate cert ~this_update ~next_update t priv] adds [cert] to
      the revocation list, increments its counter, adjusts [this_update] and
      [next_update] timestamps, and digitally signs it using [priv]. *)
  val revoke_certificate : revoked_cert ->
    this_update:Ptime.t -> ?next_update:Ptime.t -> t -> Private_key.t ->
    (t, [> `Msg of string ]) result

  (** [revoke_certificates certs ~this_update ~next_update t priv] adds [certs]
      to the revocation list, increments its counter, adjusts [this_update] and
      [next_update] timestamps, and digitally signs it using [priv]. *)
  val revoke_certificates : revoked_cert list ->
    this_update:Ptime.t -> ?next_update:Ptime.t -> t -> Private_key.t ->
    (t, [> `Msg of string ]) result
end

(** Certificate chain authenticators *)
module Authenticator : sig

  (** An authenticator [t] is a function type which takes optionally an IP
      address, a hostname and a certificate stack to an authentication decision
      {!Validation.r}. If [ip] is specified, it needs to be present in the
      SubjectAlternativeName extension of the server certificate. *)
  type t = ?ip:Ipaddr.t -> host:[`host] Domain_name.t option ->
    Certificate.t list -> Validation.r

  (** [chain_of_trust ~time ~crls ~allowed_hashes trust_anchors] is
      [authenticator], which uses the given [time] and list of [trust_anchors]
      to verify the certificate chain. All signatures must use a hash algorithm
      specified in [allowed_hashes], defaults to SHA-2.  Signatures on revocation
      lists [crls] must also use a hash algorithm in [allowed_hashes]. This is
      an implementation of the algorithm described in
      {{:https://tools.ietf.org/html/rfc5280#section-6.1}RFC 5280}, using
      {!Validation.verify_chain_of_trust}.  The given trust anchors are not
      validated, you can filter them with {!Validation.valid_cas} if desired. *)
  val chain_of_trust : time:(unit -> Ptime.t option) -> ?crls:CRL.t list ->
    ?allowed_hashes:Mirage_crypto.Hash.hash list -> Certificate.t list -> t

  (** [server_key_fingerprint ~time hash fingerprint] is an [authenticator]
      that uses the given [time] and [fingerprint] to verify that the
      fingerprint of the first element of the certificate chain matches the
      given fingerprint, using {!Validation.trust_key_fingerprint}. *)
  val server_key_fingerprint : time:(unit -> Ptime.t option) ->
    hash:Mirage_crypto.Hash.hash -> fingerprint:Cstruct.t -> t

  (** [server_cert_fingerprint ~time hash fingerprint] is an [authenticator]
      that uses the given [time] and [fingerprint] to verify the first
      element of the certificate chain, using
      {!Validation.trust_cert_fingerprint}.  Note that
      {{!server_key_fingerprint}public key pinning} has
      {{:https://www.imperialviolet.org/2011/05/04/pinning.html} advantages}
      over certificate pinning. *)
  val server_cert_fingerprint : time:(unit -> Ptime.t option) ->
    hash:Mirage_crypto.Hash.hash -> fingerprint:Cstruct.t -> t

  (** [of_string str] tries to parse the given [str] to an
      {!type:Authenticator.t}. The format of it is:
      - [none] no authentication,
      - [key-fp(:<hash>?):<base64-encoded fingerprint>] to authenticate a peer via
        its key fingerprint (hash is optional and defaults to SHA256),
      - [cert-fp(:<hash>?):<base64-encoded fingerprint>] to authenticate a peer via
        its certificate fingerprint (hash is optional and defaults to SHA256),
      - [trust-anchor(:<base64-encoded DER certificate>)+] to authenticate a
        peer from a list of certificates (certificate must be in PEM format
        without header and footer (----BEGIN CERTIFICATE-----) and without
        newlines).

      If decoding is successful, the returned value expects a function which
      outputs the current timestamp ([unit -> Ptime.t option]) and is then
      an authenticator. If decoding fails, and error is returned. *)
  val of_string : string ->
    ((unit -> Ptime.t option) -> t, [> `Msg of string ]) result
end

(** PKCS12 archive files *)
module PKCS12 : sig

  (** A PKCS12 encoded archive file, *)
  type t

  (** [decode_der buffer] is [t], the PKCS12 archive of [buffer]. *)
  val decode_der : Cstruct.t -> (t, [> `Msg of string ]) result

  (** [encode_der t] is [buf], the PKCS12 encoded archive of [t]. *)
  val encode_der : t -> Cstruct.t

  (** [verify password t] verifies and decrypts the PKCS12 archive [t]. The
      result is the contents of the archive. *)
  val verify : string -> t ->
    ([ `Certificate of Certificate.t | `Crl of CRL.t
     | `Private_key of Private_key.t | `Decrypted_private_key of Private_key.t ]
       list, [> `Msg of string ]) result

  (** [create ~mac ~algorithm ~iterations password certificates private_key]
      constructs a PKCS12 archive with [certificates] and [private_key]. They
      are encrypted with [algorithm] (using PBES2, PKCS5v2) and integrity
      protected using [mac]. *)
  val create : ?mac:[`SHA1 | `SHA224 | `SHA256 | `SHA384 | `SHA512 ] ->
    ?algorithm:[ `AES128_CBC | `AES192_CBC | `AES256_CBC ] ->
    ?iterations:int ->
    string -> Certificate.t list -> Private_key.t ->
    t
end


(** OCSP (Online Certificate Status Protocol) as described in
    {{:https://tools.ietf.org/html/rfc6960}RFC 6960}. *)
module OCSP : sig

  (** type for CertID to distinguish requested certs *)
  type cert_id

  (** [create_cert_id issuer serial] creates cert_id for this serial *)
  val create_cert_id : ?hash:Mirage_crypto.Hash.hash -> Certificate.t -> Z.t ->
    cert_id

  (** [cert_id_serial certid] is serial number of this certid *)
  val cert_id_serial : cert_id -> Z.t

  (** [pp_cert_id ppf cert_id] pretty prints cert_id *)
  val pp_cert_id : cert_id Fmt.t

  (** Module for encoding and decoding OCSP requests. *)
  module Request : sig

    (** type for Request *)
    type t

    (** [pp ppf request] pretty prints request *)
    val pp : t Fmt.t

    (** [create ~certs ~digest ~requestor_name ~key certids] creates request
        for given [certids] and, if [key] is provided, signs it using [digest].
        [requestorName] may be used by responder to distinguish requesters.
        [certs] may be used by responder to check requestor authority. *)
    val create : ?certs:Certificate.t list -> ?digest:Mirage_crypto.Hash.hash ->
      ?requestor_name:General_name.b -> ?key:Private_key.t -> cert_id list ->
      (t, [> `Msg of string ]) result

    (** [validate request key] validates the signature of [request]
        with the pulic [key]. *)
    val validate : t -> ?allowed_hashes:Mirage_crypto.Hash.hash list ->
      Public_key.t ->
      (unit, [> Validation.signature_error | `No_signature ]) result

    (** [requestor_name request] is requestorName from this request *)
    val requestor_name : t -> General_name.b option

    (** [cert_ids request] is cert ids from this request *)
    val cert_ids : t -> cert_id list

    (** [decode_der buffer] decodes request in buffer *)
    val decode_der : Cstruct.t -> (t, Asn.error) result

    (** [encode_der request] encodes request into buffer *)
    val encode_der : t -> Cstruct.t
  end

  (** Module for encoding and decoding OCSP responses. *)
  module Response : sig

    (** type for OCSPResponseStatus *)
    type status = [
      | `InternalError
      | `MalformedRequest
      | `SigRequired
      | `Successful
      | `TryLater
      | `Unauthorized
    ]

    (** [pp_status ppf status] pretty prints status *)
    val pp_status : status Fmt.t

    (** type for CertStatus *)
    type cert_status = [
      | `Good
      | `Revoked of Ptime.t * Extension.reason option
      | `Unknown
    ]

    (** [pp_cert_status ppf status] pretty prints cert status *)
    val pp_cert_status : cert_status Fmt.t

    (** type for SingleResponse *)
    type single_response

    (** [create_single_response ~next_update ~single_extension cert_id
        cert_status this_update] creates response info for one cert,
        [this_update] should be current time. *)
    val create_single_response : ?next_update:Ptime.t ->
      ?single_extensions:Extension.t -> cert_id -> cert_status -> Ptime.t ->
      single_response

    (** [pp_single_response ppf response] pretty prints single [response] *)
    val pp_single_response : single_response Fmt.t

    (** [single_response_cert_id response] is cert_id in this single [response] *)
    val single_response_cert_id : single_response -> cert_id

    (** [single_response_cert_id response] is cert_status in this single
        [response] *)
    val single_response_status : single_response -> cert_status

    (** type for ResponderID *)
    type responder_id = [
      | `ByKey of Cstruct.t
      | `ByName of Distinguished_name.t
    ]

    (** [create_responder_id pubkey] creates responderID identified by this key.
        Note: Cstruct here contains SHA1 hash of public key, not itself. *)
    val create_responder_id : Public_key.t -> responder_id

    (** [pp_responder_id ppf responderID] pretty prints [responderID] *)
    val pp_responder_id : responder_id Fmt.t

    (** type for OCSPResponse *)
    type t

    (** [create_success ~digest ~certs ~response_extensions priv_key
        responderID producedAt responses] creates response and signs it with
        [priv_key]. [producedAt] should be current timestamp. *)
    val create_success :
      ?digest:Mirage_crypto.Hash.hash ->
      ?certs:Certificate.t list ->
      ?response_extensions:Extension.t ->
      Private_key.t ->
      responder_id ->
      Ptime.t ->
      single_response list -> (t, [> `Msg of string ]) result

    (** [create status] creates error response. Successful status is not
        allowed here because it requires responseBytes. *)
    val create : [
        | `MalformedRequest
        | `InternalError
        | `TryLater
        | `SigRequired
        | `Unauthorized
      ] -> t

    (** [pp ppf response] pretty prints response *)
    val pp : t Fmt.t

    (** [status response] is response status *)
    val status : t -> status

    (** [responder_id request] is responder id from response *)
    val responder_id : t -> (responder_id, [> `Msg of string ]) result

    (** [responses response] is a list of responses (status per certificate). *)
    val responses : t -> (single_response list, [> `Msg of string ]) result

    (** [decode_der buffer] decodes response in buffer *)
    val decode_der : Cstruct.t -> (t, Asn.error) result

    (** [encode_der request] encodes response into buffer *)
    val encode_der : t -> Cstruct.t

    (** [validate response key] validates the signature of [response]
        with the pulic [key]. *)
    val validate : t -> ?allowed_hashes:Mirage_crypto.Hash.hash list ->
      ?now:Ptime.t -> Public_key.t ->
      (unit, [> Validation.signature_error | `No_signature | `Time_invalid ]) result
  end
end
