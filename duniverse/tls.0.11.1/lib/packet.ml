(** Magic numbers of the TLS protocol. *)

(* HACK: 24 bits type not in cstruct *)
let get_uint24_len buf =
  (Cstruct.BE.get_uint16 buf 0) * 0x100 + (Cstruct.get_uint8 buf 2)

let set_uint24_len buf num =
  Cstruct.BE.set_uint16 buf 0 (num / 0x100);
  Cstruct.set_uint8 buf 2 (num mod 0x100)

(* TLS record content type *)
[%%cenum
type content_type =
  | CHANGE_CIPHER_SPEC [@id 20]
  | ALERT              [@id 21]
  | HANDSHAKE          [@id 22]
  | APPLICATION_DATA   [@id 23]
  | HEARTBEAT          [@id 24]
  [@@uint8_t] [@@sexp]
]

(* TLS alert level *)
[%%cenum
type alert_level =
  | WARNING [@id 1]
  | FATAL   [@id 2]
  [@@uint8_t] [@@sexp]
]

(* TLS alert types *)
[%%cenum
type alert_type =
  | CLOSE_NOTIFY                    [@id 0]   (*RFC5246*)
  | UNEXPECTED_MESSAGE              [@id 10]  (*RFC5246*)
  | BAD_RECORD_MAC                  [@id 20]  (*RFC5246*)
  | DECRYPTION_FAILED               [@id 21]  (*RFC5246*)
  | RECORD_OVERFLOW                 [@id 22]  (*RFC5246*)
  | DECOMPRESSION_FAILURE           [@id 30]  (*RFC5246*)
  | HANDSHAKE_FAILURE               [@id 40]  (*RFC5246*)
  | NO_CERTIFICATE_RESERVED         [@id 41]  (*RFC5246*)
  | BAD_CERTIFICATE                 [@id 42]  (*RFC5246*)
  | UNSUPPORTED_CERTIFICATE         [@id 43]  (*RFC5246*)
  | CERTIFICATE_REVOKED             [@id 44]  (*RFC5246*)
  | CERTIFICATE_EXPIRED             [@id 45]  (*RFC5246*)
  | CERTIFICATE_UNKNOWN             [@id 46]  (*RFC5246*)
  | ILLEGAL_PARAMETER               [@id 47]  (*RFC5246*)
  | UNKNOWN_CA                      [@id 48]  (*RFC5246*)
  | ACCESS_DENIED                   [@id 49]  (*RFC5246*)
  | DECODE_ERROR                    [@id 50]  (*RFC5246*)
  | DECRYPT_ERROR                   [@id 51]  (*RFC5246*)
  | EXPORT_RESTRICTION_RESERVED     [@id 60]  (*RFC5246*)
  | PROTOCOL_VERSION                [@id 70]  (*RFC5246*)
  | INSUFFICIENT_SECURITY           [@id 71]  (*RFC5246*)
  | INTERNAL_ERROR                  [@id 80]  (*RFC5246*)
  | INAPPROPRIATE_FALLBACK          [@id 86]  (*draft-ietf-tls-downgrade-scsv*)
  | USER_CANCELED                   [@id 90]  (*RFC5246*)
  | NO_RENEGOTIATION                [@id 100] (*RFC5246*)
  | UNSUPPORTED_EXTENSION           [@id 110] (*RFC5246*)
  | CERTIFICATE_UNOBTAINABLE        [@id 111] (*RFC6066*)
  | UNRECOGNIZED_NAME               [@id 112] (*RFC6066*)
  | BAD_CERTIFICATE_STATUS_RESPONSE [@id 113] (*RFC6066*)
  | BAD_CERTIFICATE_HASH_VALUE      [@id 114] (*RFC6066*)
  | UNKNOWN_PSK_IDENTITY            [@id 115] (*RFC4279*)
  | NO_APPLICATION_PROTOCOL         [@id 120] (*RFC7301*)
  [@@uint8_t] [@@sexp]
]

(* TLS handshake type *)
[%%cenum
type handshake_type =
  | HELLO_REQUEST        [@id 0]
  | CLIENT_HELLO         [@id 1]
  | SERVER_HELLO         [@id 2]
  | HELLO_VERIFY_REQUEST [@id 3] (*RFC6347*)
  | NEWSESSIONTICKET     [@id 4] (*RFC4507*)
  | CERTIFICATE          [@id 11]
  | SERVER_KEY_EXCHANGE  [@id 12]
  | CERTIFICATE_REQUEST  [@id 13]
  | SERVER_HELLO_DONE    [@id 14]
  | CERTIFICATE_VERIFY   [@id 15]
  | CLIENT_KEY_EXCHANGE  [@id 16]
  | FINISHED             [@id 20]
  (* from RFC 4366 *)
  | CERTIFICATE_URL      [@id 21]
  | CERTIFICATE_STATUS   [@id 22]
  | SUPPLEMENTAL_DATA    [@id 23] (*RFC4680*)
  [@@uint8_t] [@@sexp]
]

(* TLS certificate types *)
[%%cenum
type client_certificate_type =
  | RSA_SIGN                  [@id 1]  (*RFC5246*)
  | DSS_SIGN                  [@id 2]  (*RFC5246*)
  | RSA_FIXED_DH              [@id 3]  (*RFC5246*)
  | DSS_FIXED_DH              [@id 4]  (*RFC5246*)
  | RSA_EPHEMERAL_DH_RESERVED [@id 5]  (*RFC5246*)
  | DSS_EPHEMERAL_DH_RESERVED [@id 6]  (*RFC5246*)
  | FORTEZZA_DMS_RESERVED     [@id 20] (*RFC5246*)
  | ECDSA_SIGN                [@id 64] (*RFC4492*)
  | RSA_FIXED_ECDH            [@id 65] (*RFC4492*)
  | ECDSA_FIXED_ECDH          [@id 66] (*RFC4492*)
  [@@uint8_t] [@@sexp]
]

(* TLS compression methods, used in hello packets *)
[%%cenum
type compression_method =
  | NULL    [@id 0]
  | DEFLATE [@id 1]
  | LZS     [@id 64]
  [@@uint8_t] [@@sexp]
]

(* TLS extensions in hello packets from RFC 6066, formerly RFC 4366 *)
[%%cenum
type extension_type =
  | SERVER_NAME                            [@id 0]
  | MAX_FRAGMENT_LENGTH                    [@id 1]
  | CLIENT_CERTIFICATE_URL                 [@id 2]
  | TRUSTED_CA_KEYS                        [@id 3]
  | TRUNCATED_HMAC                         [@id 4]
  | STATUS_REQUEST                         [@id 5]
  | USER_MAPPING                           [@id 6]  (*RFC4681*)
  | CLIENT_AUTHZ                           [@id 7]  (*RFC5878*)
  | SERVER_AUTHZ                           [@id 8]  (*RFC5878*)
  | CERT_TYPE                              [@id 9]  (*RFC6091*)
  | ELLIPTIC_CURVES                        [@id 10] (*RFC4492*)
  | EC_POINT_FORMATS                       [@id 11] (*RFC4492*)
  | SRP                                    [@id 12] (*RFC5054*)
  | SIGNATURE_ALGORITHMS                   [@id 13] (*RFC5246*)
  | USE_SRP                                [@id 14] (*RFC5764*)
  | HEARTBEAT                              [@id 15] (*RFC6520*)
  | APPLICATION_LAYER_PROTOCOL_NEGOTIATION [@id 16] (*RFC7301*)
  | STATUS_REQUEST_V2                      [@id 17] (*RFC6961*)
  | SIGNED_CERTIFICATE_TIMESTAMP           [@id 18] (*RFC6962*)
  | CLIENT_CERTIFICATE_TYPE                [@id 19] (*RFC7250*)
  | SERVER_CERTIFICATE_TYPE                [@id 20] (*RFC7250*)
  | PADDING                                [@id 21] (*draft-ietf-tls-padding*)
  | ENCRYPT_THEN_MAC                       [@id 22] (*RFC7366*)
  | EXTENDED_MASTER_SECRET                 [@id 23] (*draft-ietf-tls-session-hash*)
  | SESSIONTICKET_TLS                      [@id 35] (*RFC4507*)
  | RENEGOTIATION_INFO                     [@id 0xFF01] (*RFC5746*)
  [@@uint16_t] [@@sexp]
]

(* TLS maximum fragment length *)
[%%cenum
type max_fragment_length =
  | TWO_9  [@id 1]
  | TWO_10 [@id 2]
  | TWO_11 [@id 3]
  | TWO_12 [@id 4]
  [@@uint8_t] [@@sexp]
]

(* RFC 5246 *)
[%%cenum
type signature_algorithm_type =
  | ANONYMOUS [@id 0]
  | RSA       [@id 1]
  | DSA       [@id 2]
  | ECDSA     [@id 3]
  [@@uint8_t] [@@sexp]
]

[%%cenum
type hash_algorithm =
  | NULL      [@id 0]
  | MD5       [@id 1]
  | SHA       [@id 2]
  | SHA224    [@id 3]
  | SHA256    [@id 4]
  | SHA384    [@id 5]
  | SHA512    [@id 6]
  [@@uint8_t] [@@sexp]
]

(** [hash_algorithm_of_tag tag] is [hash_algorithm] for the given [tag] *)
let hash_algorithm_of_tag = function
  | `MD5    -> MD5
  | `SHA1   -> SHA
  | `SHA224 -> SHA224
  | `SHA256 -> SHA256
  | `SHA384 -> SHA384
  | `SHA512 -> SHA512

(** [tag_of_hash_algorithm hash_algorithm] is [tag] for the given [hash_algorithm] *)
let tag_of_hash_algorithm = function
  | MD5    -> Some `MD5
  | SHA    -> Some `SHA1
  | SHA224 -> Some `SHA224
  | SHA256 -> Some `SHA256
  | SHA384 -> Some `SHA384
  | SHA512 -> Some `SHA512
  | NULL   -> None

(* EC RFC4492*)
[%%cenum
type ec_curve_type =
  | EXPLICIT_PRIME [@id 1]
  | EXPLICIT_CHAR2 [@id 2]
  | NAMED_CURVE    [@id 3]
  [@@uint8_t] [@@sexp]
]

[%%cenum
type named_curve_type =
  | SECT163K1 [@id 1]
  | SECT163R1 [@id 2]
  | SECT163R2 [@id 3]
  | SECT193R1 [@id 4]
  | SECT193R2 [@id 5]
  | SECT233K1 [@id 6]
  | SECT233R1 [@id 7]
  | SECT239K1 [@id 8]
  | SECT283K1 [@id 9]
  | SECT283R1 [@id 10]
  | SECT409K1 [@id 11]
  | SECT409R1 [@id 12]
  | SECT571K1 [@id 13]
  | SECT571R1 [@id 14]
  | SECP160K1 [@id 15]
  | SECP160R1 [@id 16]
  | SECP160R2 [@id 17]
  | SECP192K1 [@id 18]
  | SECP192R1 [@id 19]
  | SECP224K1 [@id 20]
  | SECP224R1 [@id 21]
  | SECP256K1 [@id 22]
  | SECP256R1 [@id 23]
  | SECP384R1 [@id 24]
  | SECP521R1 [@id 25]
  (*RFC7027*)
  | BRAINPOOLP256R1 [@id 26]
  | BRAINPOOLP384R1 [@id 27]
  | BRAINPOOLP512R1 [@id 28]
  (* reserved (0xFE00..0xFEFF), *)
  | ARBITRARY_EXPLICIT_PRIME_CURVES [@id 0xFF01]
  | ARBITRARY_EXPLICIT_CHAR2_CURVES [@id 0xFF02]
  [@@uint16_t] [@@sexp]
]

[%%cenum
type ec_point_format =
  | UNCOMPRESSED              [@id 0]
  | ANSIX962_COMPRESSED_PRIME [@id 1]
  | ANSIX962_COMPRESSED_CHAR2 [@id 2]
  (* reserved 248..255 *)
  [@@uint8_t] [@@sexp]
]

[%%cenum
type ec_basis_type =
  | TRINOMIAL   [@id 0]
  | PENTANOMIAL [@id 1]
  [@@uint8_t] [@@sexp]
]

(** enum of all TLS ciphersuites *)
[%%cenum
type any_ciphersuite =
  | TLS_NULL_WITH_NULL_NULL                [@id 0x0000]
  | TLS_RSA_WITH_NULL_MD5                  [@id 0x0001]
  | TLS_RSA_WITH_NULL_SHA                  [@id 0x0002]
  | TLS_RSA_EXPORT_WITH_RC4_40_MD5         [@id 0x0003]
  | TLS_RSA_WITH_RC4_128_MD5               [@id 0x0004]
  | TLS_RSA_WITH_RC4_128_SHA               [@id 0x0005]
  | TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5     [@id 0x0006]
  | TLS_RSA_WITH_IDEA_CBC_SHA              [@id 0x0007]
  | TLS_RSA_EXPORT_WITH_DES40_CBC_SHA      [@id 0x0008]
  | TLS_RSA_WITH_DES_CBC_SHA               [@id 0x0009]
  | TLS_RSA_WITH_3DES_EDE_CBC_SHA          [@id 0x000A]
  | TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA   [@id 0x000B]
  | TLS_DH_DSS_WITH_DES_CBC_SHA            [@id 0x000C]
  | TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA       [@id 0x000D]
  | TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA   [@id 0x000E]
  | TLS_DH_RSA_WITH_DES_CBC_SHA            [@id 0x000F]
  | TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA       [@id 0x0010]
  | TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA  [@id 0x0011]
  | TLS_DHE_DSS_WITH_DES_CBC_SHA           [@id 0x0012]
  | TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA      [@id 0x0013]
  | TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA  [@id 0x0014]
  | TLS_DHE_RSA_WITH_DES_CBC_SHA           [@id 0x0015]
  | TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA      [@id 0x0016]
  | TLS_DH_anon_EXPORT_WITH_RC4_40_MD5     [@id 0x0017]
  | TLS_DH_anon_EXPORT_WITH_DES40_CBC_SHA  [@id 0x0019]
  (* MITM deprecated *)
  | TLS_DH_anon_WITH_RC4_128_MD5        [@id 0x0018]
  | TLS_DH_anon_WITH_DES_CBC_SHA        [@id 0x001A]
  | TLS_DH_anon_WITH_3DES_EDE_CBC_SHA   [@id 0x001B]
  | RESERVED_SSL3_1                     [@id 0x001C] (* RFC5246 *)
  | RESERVED_SSL3_2                     [@id 0x001D] (* RFC5246 *)
  | TLS_KRB5_WITH_DES_CBC_SHA           [@id 0x001E] (* RFC2712 *)
  | TLS_KRB5_WITH_3DES_EDE_CBC_SHA      [@id 0x001F] (* RFC2712 *)
  | TLS_KRB5_WITH_RC4_128_SHA           [@id 0x0020] (*RFC2712 RFC6347*)
  | TLS_KRB5_WITH_IDEA_CBC_SHA          [@id 0x0021] (*RFC2712*)
  | TLS_KRB5_WITH_DES_CBC_MD5           [@id 0x0022] (*RFC2712*)
  | TLS_KRB5_WITH_3DES_EDE_CBC_MD5      [@id 0x0023] (*RFC2712*)
  | TLS_KRB5_WITH_RC4_128_MD5           [@id 0x0024] (*RFC2712, RFC6347*)
  | TLS_KRB5_WITH_IDEA_CBC_MD5          [@id 0x0025] (*RFC2712*)
  | TLS_KRB5_EXPORT_WITH_DES_CBC_40_SHA [@id 0x0026] (*RFC2712*)
  | TLS_KRB5_EXPORT_WITH_RC2_CBC_40_SHA [@id 0x0027] (*RFC2712*)
  | TLS_KRB5_EXPORT_WITH_RC4_40_SHA     [@id 0x0028] (*RFC2712, RFC6347*)
  | TLS_KRB5_EXPORT_WITH_DES_CBC_40_MD5 [@id 0x0029] (*RFC2712*)
  | TLS_KRB5_EXPORT_WITH_RC2_CBC_40_MD5 [@id 0x002A] (*RFC2712*)
  | TLS_KRB5_EXPORT_WITH_RC4_40_MD5     [@id 0x002B] (*RFC2712, RFC6347*)
  | TLS_PSK_WITH_NULL_SHA               [@id 0x002C] (*RFC4785*)
  | TLS_DHE_PSK_WITH_NULL_SHA           [@id 0x002D] (*RFC4785*)
  | TLS_RSA_PSK_WITH_NULL_SHA           [@id 0x002E] (*RFC4785*)
  (* from RFC 3268 *)
  | TLS_RSA_WITH_AES_128_CBC_SHA      [@id 0x002F]
  | TLS_DH_DSS_WITH_AES_128_CBC_SHA   [@id 0x0030]
  | TLS_DH_RSA_WITH_AES_128_CBC_SHA   [@id 0x0031]
  | TLS_DHE_DSS_WITH_AES_128_CBC_SHA  [@id 0x0032]
  | TLS_DHE_RSA_WITH_AES_128_CBC_SHA  [@id 0x0033]
  | TLS_DH_anon_WITH_AES_128_CBC_SHA  [@id 0x0034]
  | TLS_RSA_WITH_AES_256_CBC_SHA      [@id 0x0035]
  | TLS_DH_DSS_WITH_AES_256_CBC_SHA   [@id 0x0036]
  | TLS_DH_RSA_WITH_AES_256_CBC_SHA   [@id 0x0037]
  | TLS_DHE_DSS_WITH_AES_256_CBC_SHA  [@id 0x0038]
  | TLS_DHE_RSA_WITH_AES_256_CBC_SHA  [@id 0x0039]
  | TLS_DH_anon_WITH_AES_256_CBC_SHA  [@id 0x003A]
  (* from RFC 5246 *)
  | TLS_RSA_WITH_NULL_SHA256                 [@id 0x003B]
  | TLS_RSA_WITH_AES_128_CBC_SHA256          [@id 0x003C]
  | TLS_RSA_WITH_AES_256_CBC_SHA256          [@id 0x003D]
  | TLS_DH_DSS_WITH_AES_128_CBC_SHA256       [@id 0x003E]
  | TLS_DH_RSA_WITH_AES_128_CBC_SHA256       [@id 0x003F]
  | TLS_DHE_DSS_WITH_AES_128_CBC_SHA256      [@id 0x0040]
  | TLS_RSA_WITH_CAMELLIA_128_CBC_SHA        [@id 0x0041] (*RFC5932*)
  | TLS_DH_DSS_WITH_CAMELLIA_128_CBC_SHA     [@id 0x0042] (*RFC5932*)
  | TLS_DH_RSA_WITH_CAMELLIA_128_CBC_SHA     [@id 0x0043] (*RFC5932*)
  | TLS_DHE_DSS_WITH_CAMELLIA_128_CBC_SHA    [@id 0x0044] (*RFC5932*)
  | TLS_DHE_RSA_WITH_CAMELLIA_128_CBC_SHA    [@id 0x0045] (*RFC5932*)
  | TLS_DH_anon_WITH_CAMELLIA_128_CBC_SHA    [@id 0x0046] (*RFC5932*)
  | TLS_DHE_RSA_WITH_AES_128_CBC_SHA256      [@id 0x0067]
  | TLS_DH_DSS_WITH_AES_256_CBC_SHA256       [@id 0x0068]
  | TLS_DH_RSA_WITH_AES_256_CBC_SHA256       [@id 0x0069]
  | TLS_DHE_DSS_WITH_AES_256_CBC_SHA256      [@id 0x006A]
  | TLS_DHE_RSA_WITH_AES_256_CBC_SHA256      [@id 0x006B]
  | TLS_DH_anon_WITH_AES_128_CBC_SHA256      [@id 0x006C]
  | TLS_DH_anon_WITH_AES_256_CBC_SHA256      [@id 0x006D]
  | TLS_RSA_WITH_CAMELLIA_256_CBC_SHA        [@id 0x0084] (*RFC5932*)
  | TLS_DH_DSS_WITH_CAMELLIA_256_CBC_SHA     [@id 0x0085] (*RFC5932*)
  | TLS_DH_RSA_WITH_CAMELLIA_256_CBC_SHA     [@id 0x0086] (*RFC5932*)
  | TLS_DHE_DSS_WITH_CAMELLIA_256_CBC_SHA    [@id 0x0087] (*RFC5932*)
  | TLS_DHE_RSA_WITH_CAMELLIA_256_CBC_SHA    [@id 0x0088] (*RFC5932*)
  | TLS_DH_anon_WITH_CAMELLIA_256_CBC_SHA    [@id 0x0089] (*RFC5932*)
  | TLS_PSK_WITH_RC4_128_SHA                 [@id 0x008A] (*RFC4279, RFC6347*)
  | TLS_PSK_WITH_3DES_EDE_CBC_SHA            [@id 0x008B] (*RFC4279*)
  | TLS_PSK_WITH_AES_128_CBC_SHA             [@id 0x008C] (*RFC4279*)
  | TLS_PSK_WITH_AES_256_CBC_SHA             [@id 0x008D] (*RFC4279*)
  | TLS_DHE_PSK_WITH_RC4_128_SHA             [@id 0x008E] (*RFC4279, RFC6347*)
  | TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA        [@id 0x008F] (*RFC4279*)
  | TLS_DHE_PSK_WITH_AES_128_CBC_SHA         [@id 0x0090] (*RFC4279*)
  | TLS_DHE_PSK_WITH_AES_256_CBC_SHA         [@id 0x0091] (*RFC4279*)
  | TLS_RSA_PSK_WITH_RC4_128_SHA             [@id 0x0092] (*RFC4279, RFC6347*)
  | TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA        [@id 0x0093] (*RFC4279*)
  | TLS_RSA_PSK_WITH_AES_128_CBC_SHA         [@id 0x0094] (*RFC4279*)
  | TLS_RSA_PSK_WITH_AES_256_CBC_SHA         [@id 0x0095] (*RFC4279*)
  | TLS_RSA_WITH_SEED_CBC_SHA                [@id 0x0096] (*RFC4162*)
  | TLS_DH_DSS_WITH_SEED_CBC_SHA             [@id 0x0097] (*RFC4162*)
  | TLS_DH_RSA_WITH_SEED_CBC_SHA             [@id 0x0098] (*RFC4162*)
  | TLS_DHE_DSS_WITH_SEED_CBC_SHA            [@id 0x0099] (*RFC4162*)
  | TLS_DHE_RSA_WITH_SEED_CBC_SHA            [@id 0x009A] (*RFC4162*)
  | TLS_DH_anon_WITH_SEED_CBC_SHA            [@id 0x009B] (*RFC4162*)
  | TLS_RSA_WITH_AES_128_GCM_SHA256          [@id 0x009C] (*RFC5288*)
  | TLS_RSA_WITH_AES_256_GCM_SHA384          [@id 0x009D] (*RFC5288*)
  | TLS_DHE_RSA_WITH_AES_128_GCM_SHA256      [@id 0x009E] (*RFC5288*)
  | TLS_DHE_RSA_WITH_AES_256_GCM_SHA384      [@id 0x009F] (*RFC5288*)
  | TLS_DH_RSA_WITH_AES_128_GCM_SHA256       [@id 0x00A0] (*RFC5288*)
  | TLS_DH_RSA_WITH_AES_256_GCM_SHA384       [@id 0x00A1] (*RFC5288*)
  | TLS_DHE_DSS_WITH_AES_128_GCM_SHA256      [@id 0x00A2] (*RFC5288*)
  | TLS_DHE_DSS_WITH_AES_256_GCM_SHA384      [@id 0x00A3] (*RFC5288*)
  | TLS_DH_DSS_WITH_AES_128_GCM_SHA256       [@id 0x00A4] (*RFC5288*)
  | TLS_DH_DSS_WITH_AES_256_GCM_SHA384       [@id 0x00A5] (*RFC5288*)
  | TLS_DH_anon_WITH_AES_128_GCM_SHA256      [@id 0x00A6] (*RFC5288*)
  | TLS_DH_anon_WITH_AES_256_GCM_SHA384      [@id 0x00A7] (*RFC5288*)
  | TLS_PSK_WITH_AES_128_GCM_SHA256          [@id 0x00A8] (*RFC5487*)
  | TLS_PSK_WITH_AES_256_GCM_SHA384          [@id 0x00A9] (*RFC5487*)
  | TLS_DHE_PSK_WITH_AES_128_GCM_SHA256      [@id 0x00AA] (*RFC5487*)
  | TLS_DHE_PSK_WITH_AES_256_GCM_SHA384      [@id 0x00AB] (*RFC5487*)
  | TLS_RSA_PSK_WITH_AES_128_GCM_SHA256      [@id 0x00AC] (*RFC5487*)
  | TLS_RSA_PSK_WITH_AES_256_GCM_SHA384      [@id 0x00AD] (*RFC5487*)
  | TLS_PSK_WITH_AES_128_CBC_SHA256          [@id 0x00AE] (*RFC5487*)
  | TLS_PSK_WITH_AES_256_CBC_SHA384          [@id 0x00AF] (*RFC5487*)
  | TLS_PSK_WITH_NULL_SHA256                 [@id 0x00B0] (*RFC5487*)
  | TLS_PSK_WITH_NULL_SHA384                 [@id 0x00B1] (*RFC5487*)
  | TLS_DHE_PSK_WITH_AES_128_CBC_SHA256      [@id 0x00B2] (*RFC5487*)
  | TLS_DHE_PSK_WITH_AES_256_CBC_SHA384      [@id 0x00B3] (*RFC5487*)
  | TLS_DHE_PSK_WITH_NULL_SHA256             [@id 0x00B4] (*RFC5487*)
  | TLS_DHE_PSK_WITH_NULL_SHA384             [@id 0x00B5] (*RFC5487*)
  | TLS_RSA_PSK_WITH_AES_128_CBC_SHA256      [@id 0x00B6] (*RFC5487*)
  | TLS_RSA_PSK_WITH_AES_256_CBC_SHA384      [@id 0x00B7] (*RFC5487*)
  | TLS_RSA_PSK_WITH_NULL_SHA256             [@id 0x00B8] (*RFC5487*)
  | TLS_RSA_PSK_WITH_NULL_SHA384             [@id 0x00B9] (*RFC5487*)
  | TLS_RSA_WITH_CAMELLIA_128_CBC_SHA256     [@id 0x00BA] (*RFC5932*)
  | TLS_DH_DSS_WITH_CAMELLIA_128_CBC_SHA256  [@id 0x00BB] (*RFC5932*)
  | TLS_DH_RSA_WITH_CAMELLIA_128_CBC_SHA256  [@id 0x00BC] (*RFC5932*)
  | TLS_DHE_DSS_WITH_CAMELLIA_128_CBC_SHA256 [@id 0x00BD] (*RFC5932*)
  | TLS_DHE_RSA_WITH_CAMELLIA_128_CBC_SHA256 [@id 0x00BE] (*RFC5932*)
  | TLS_DH_anon_WITH_CAMELLIA_128_CBC_SHA256 [@id 0x00BF] (*RFC5932*)
  | TLS_RSA_WITH_CAMELLIA_256_CBC_SHA256     [@id 0x00C0] (*RFC5932*)
  | TLS_DH_DSS_WITH_CAMELLIA_256_CBC_SHA256  [@id 0x00C1] (*RFC5932*)
  | TLS_DH_RSA_WITH_CAMELLIA_256_CBC_SHA256  [@id 0x00C2] (*RFC5932*)
  | TLS_DHE_DSS_WITH_CAMELLIA_256_CBC_SHA256 [@id 0x00C3] (*RFC5932*)
  | TLS_DHE_RSA_WITH_CAMELLIA_256_CBC_SHA256 [@id 0x00C4] (*RFC5932*)
  | TLS_DH_anon_WITH_CAMELLIA_256_CBC_SHA256 [@id 0x00C5] (*RFC5932*)
  | TLS_EMPTY_RENEGOTIATION_INFO_SCSV        [@id 0x00FF] (*RFC5746*)
  | TLS_FALLBACK_SCSV                        [@id 0x5600] (*draft-ietf-tls-downgrade-scsv*)
  (* from RFC 4492 *)
  | TLS_ECDH_ECDSA_WITH_NULL_SHA                 [@id 0xC001]
  | TLS_ECDH_ECDSA_WITH_RC4_128_SHA              [@id 0xC002]
  | TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA         [@id 0xC003]
  | TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA          [@id 0xC004]
  | TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA          [@id 0xC005]
  | TLS_ECDHE_ECDSA_WITH_NULL_SHA                [@id 0xC006]
  | TLS_ECDHE_ECDSA_WITH_RC4_128_SHA             [@id 0xC007]
  | TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA        [@id 0xC008]
  | TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA         [@id 0xC009]
  | TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA         [@id 0xC00A]
  | TLS_ECDH_RSA_WITH_NULL_SHA                   [@id 0xC00B]
  | TLS_ECDH_RSA_WITH_RC4_128_SHA                [@id 0xC00C]
  | TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA           [@id 0xC00D]
  | TLS_ECDH_RSA_WITH_AES_128_CBC_SHA            [@id 0xC00E]
  | TLS_ECDH_RSA_WITH_AES_256_CBC_SHA            [@id 0xC00F]
  | TLS_ECDHE_RSA_WITH_NULL_SHA                  [@id 0xC010]
  | TLS_ECDHE_RSA_WITH_RC4_128_SHA               [@id 0xC011]
  | TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA          [@id 0xC012]
  | TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA           [@id 0xC013]
  | TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA           [@id 0xC014]
  | TLS_ECDH_anon_WITH_NULL_SHA                  [@id 0xC015]
  | TLS_ECDH_anon_WITH_RC4_128_SHA               [@id 0xC016]
  | TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA          [@id 0xC017]
  | TLS_ECDH_anon_WITH_AES_128_CBC_SHA           [@id 0xC018]
  | TLS_ECDH_anon_WITH_AES_256_CBC_SHA           [@id 0xC019]
  | TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA            [@id 0xC01A] (*RFC5054*)
  | TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA        [@id 0xC01B] (*RFC5054*)
  | TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA        [@id 0xC01C] (*RFC5054*)
  | TLS_SRP_SHA_WITH_AES_128_CBC_SHA             [@id 0xC01D] (*RFC5054*)
  | TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA         [@id 0xC01E] (*RFC5054*)
  | TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA         [@id 0xC01F] (*RFC5054*)
  | TLS_SRP_SHA_WITH_AES_256_CBC_SHA             [@id 0xC020] (*RFC5054*)
  | TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA         [@id 0xC021] (*RFC5054*)
  | TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA         [@id 0xC022] (*RFC5054*)
  | TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256      [@id 0xC023] (*RFC5289*)
  | TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384      [@id 0xC024] (*RFC5289*)
  | TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA256       [@id 0xC025] (*RFC5289*)
  | TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA384       [@id 0xC026] (*RFC5289*)
  | TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256        [@id 0xC027] (*RFC5289*)
  | TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384        [@id 0xC028] (*RFC5289*)
  | TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256         [@id 0xC029] (*RFC5289*)
  | TLS_ECDH_RSA_WITH_AES_256_CBC_SHA384         [@id 0xC02A]  (*RFC5289*)
  | TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256      [@id 0xC02B] (*RFC5289*)
  | TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384      [@id 0xC02C] (*RFC5289*)
  | TLS_ECDH_ECDSA_WITH_AES_128_GCM_SHA256       [@id 0xC02D] (*RFC5289*)
  | TLS_ECDH_ECDSA_WITH_AES_256_GCM_SHA384       [@id 0xC02E] (*RFC5289*)
  | TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256        [@id 0xC02F] (*RFC5289*)
  | TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384        [@id 0xC030] (*RFC5289*)
  | TLS_ECDH_RSA_WITH_AES_128_GCM_SHA256         [@id 0xC031] (*RFC5289*)
  | TLS_ECDH_RSA_WITH_AES_256_GCM_SHA384         [@id 0xC032] (*RFC5289*)
  | TLS_ECDHE_PSK_WITH_RC4_128_SHA               [@id 0xC033] (*RFC5489*)(*RFC6347*)
  | TLS_ECDHE_PSK_WITH_3DES_EDE_CBC_SHA          [@id 0xC034] (*RFC5489*)
  | TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA           [@id 0xC035] (*RFC5489*)
  | TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA           [@id 0xC036] (*RFC5489*)
  | TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA256        [@id 0xC037] (*RFC5489*)
  | TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA384        [@id 0xC038] (*RFC5489*)
  | TLS_ECDHE_PSK_WITH_NULL_SHA                  [@id 0xC039] (*RFC5489*)
  | TLS_ECDHE_PSK_WITH_NULL_SHA256               [@id 0xC03A] (*RFC5489*)
  | TLS_ECDHE_PSK_WITH_NULL_SHA384               [@id 0xC03B] (*RFC5489*)
  | TLS_RSA_WITH_ARIA_128_CBC_SHA256             [@id 0xC03C] (*RFC6209*)
  | TLS_RSA_WITH_ARIA_256_CBC_SHA384             [@id 0xC03D] (*RFC6209*)
  | TLS_DH_DSS_WITH_ARIA_128_CBC_SHA256          [@id 0xC03E] (*RFC6209*)
  | TLS_DH_DSS_WITH_ARIA_256_CBC_SHA384          [@id 0xC03F] (*RFC6209*)
  | TLS_DH_RSA_WITH_ARIA_128_CBC_SHA256          [@id 0xC040] (*RFC6209*)
  | TLS_DH_RSA_WITH_ARIA_256_CBC_SHA384          [@id 0xC041] (*RFC6209*)
  | TLS_DHE_DSS_WITH_ARIA_128_CBC_SHA256         [@id 0xC042] (*RFC6209*)
  | TLS_DHE_DSS_WITH_ARIA_256_CBC_SHA384         [@id 0xC043] (*RFC6209*)
  | TLS_DHE_RSA_WITH_ARIA_128_CBC_SHA256         [@id 0xC044] (*RFC6209*)
  | TLS_DHE_RSA_WITH_ARIA_256_CBC_SHA384         [@id 0xC045] (*RFC6209*)
  | TLS_DH_anon_WITH_ARIA_128_CBC_SHA256         [@id 0xC046] (*RFC6209*)
  | TLS_DH_anon_WITH_ARIA_256_CBC_SHA384         [@id 0xC047] (*RFC6209*)
  | TLS_ECDHE_ECDSA_WITH_ARIA_128_CBC_SHA256     [@id 0xC048] (*RFC6209*)
  | TLS_ECDHE_ECDSA_WITH_ARIA_256_CBC_SHA384     [@id 0xC049] (*RFC6209*)
  | TLS_ECDH_ECDSA_WITH_ARIA_128_CBC_SHA256      [@id 0xC04A] (*RFC6209*)
  | TLS_ECDH_ECDSA_WITH_ARIA_256_CBC_SHA384      [@id 0xC04B] (*RFC6209*)
  | TLS_ECDHE_RSA_WITH_ARIA_128_CBC_SHA256       [@id 0xC04C] (*RFC6209*)
  | TLS_ECDHE_RSA_WITH_ARIA_256_CBC_SHA384       [@id 0xC04D] (*RFC6209*)
  | TLS_ECDH_RSA_WITH_ARIA_128_CBC_SHA256        [@id 0xC04E] (*RFC6209*)
  | TLS_ECDH_RSA_WITH_ARIA_256_CBC_SHA384        [@id 0xC04F] (*RFC6209*)
  | TLS_RSA_WITH_ARIA_128_GCM_SHA256             [@id 0xC050] (*RFC6209*)
  | TLS_RSA_WITH_ARIA_256_GCM_SHA384             [@id 0xC051] (*RFC6209*)
  | TLS_DHE_RSA_WITH_ARIA_128_GCM_SHA256         [@id 0xC052] (*RFC6209*)
  | TLS_DHE_RSA_WITH_ARIA_256_GCM_SHA384         [@id 0xC053] (*RFC6209*)
  | TLS_DH_RSA_WITH_ARIA_128_GCM_SHA256          [@id 0xC054] (*RFC6209*)
  | TLS_DH_RSA_WITH_ARIA_256_GCM_SHA384          [@id 0xC055] (*RFC6209*)
  | TLS_DHE_DSS_WITH_ARIA_128_GCM_SHA256         [@id 0xC056] (*RFC6209*)
  | TLS_DHE_DSS_WITH_ARIA_256_GCM_SHA384         [@id 0xC057] (*RFC6209*)
  | TLS_DH_DSS_WITH_ARIA_128_GCM_SHA256          [@id 0xC058] (*RFC6209*)
  | TLS_DH_DSS_WITH_ARIA_256_GCM_SHA384          [@id 0xC059] (*RFC6209*)
  | TLS_DH_anon_WITH_ARIA_128_GCM_SHA256         [@id 0xC05A] (*RFC6209*)
  | TLS_DH_anon_WITH_ARIA_256_GCM_SHA384         [@id 0xC05B] (*RFC6209*)
  | TLS_ECDHE_ECDSA_WITH_ARIA_128_GCM_SHA256     [@id 0xC05C] (*RFC6209*)
  | TLS_ECDHE_ECDSA_WITH_ARIA_256_GCM_SHA384     [@id 0xC05D] (*RFC6209*)
  | TLS_ECDH_ECDSA_WITH_ARIA_128_GCM_SHA256      [@id 0xC05E] (*RFC6209*)
  | TLS_ECDH_ECDSA_WITH_ARIA_256_GCM_SHA384      [@id 0xC05F] (*RFC6209*)
  | TLS_ECDHE_RSA_WITH_ARIA_128_GCM_SHA256       [@id 0xC060] (*RFC6209*)
  | TLS_ECDHE_RSA_WITH_ARIA_256_GCM_SHA384       [@id 0xC061] (*RFC6209*)
  | TLS_ECDH_RSA_WITH_ARIA_128_GCM_SHA256        [@id 0xC062] (*RFC6209*)
  | TLS_ECDH_RSA_WITH_ARIA_256_GCM_SHA384        [@id 0xC063] (*RFC6209*)
  | TLS_PSK_WITH_ARIA_128_CBC_SHA256             [@id 0xC064] (*RFC6209*)
  | TLS_PSK_WITH_ARIA_256_CBC_SHA384             [@id 0xC065] (*RFC6209*)
  | TLS_DHE_PSK_WITH_ARIA_128_CBC_SHA256         [@id 0xC066] (*RFC6209*)
  | TLS_DHE_PSK_WITH_ARIA_256_CBC_SHA384         [@id 0xC067] (*RFC6209*)
  | TLS_RSA_PSK_WITH_ARIA_128_CBC_SHA256         [@id 0xC068] (*RFC6209*)
  | TLS_RSA_PSK_WITH_ARIA_256_CBC_SHA384         [@id 0xC069] (*RFC6209*)
  | TLS_PSK_WITH_ARIA_128_GCM_SHA256             [@id 0xC06A] (*RFC6209*)
  | TLS_PSK_WITH_ARIA_256_GCM_SHA384             [@id 0xC06B] (*RFC6209*)
  | TLS_DHE_PSK_WITH_ARIA_128_GCM_SHA256         [@id 0xC06C] (*RFC6209*)
  | TLS_DHE_PSK_WITH_ARIA_256_GCM_SHA384         [@id 0xC06D] (*RFC6209*)
  | TLS_RSA_PSK_WITH_ARIA_128_GCM_SHA256         [@id 0xC06E] (*RFC6209*)
  | TLS_RSA_PSK_WITH_ARIA_256_GCM_SHA384         [@id 0xC06F] (*RFC6209*)
  | TLS_ECDHE_PSK_WITH_ARIA_128_CBC_SHA256       [@id 0xC070] (*RFC6209*)
  | TLS_ECDHE_PSK_WITH_ARIA_256_CBC_SHA384       [@id 0xC071] (*RFC6209*)
  | TLS_ECDHE_ECDSA_WITH_CAMELLIA_128_CBC_SHA256 [@id 0xC072] (*RFC6367*)
  | TLS_ECDHE_ECDSA_WITH_CAMELLIA_256_CBC_SHA384 [@id 0xC073] (*RFC6367*)
  | TLS_ECDH_ECDSA_WITH_CAMELLIA_128_CBC_SHA256  [@id 0xC074] (*RFC6367*)
  | TLS_ECDH_ECDSA_WITH_CAMELLIA_256_CBC_SHA384  [@id 0xC075] (*RFC6367*)
  | TLS_ECDHE_RSA_WITH_CAMELLIA_128_CBC_SHA256   [@id 0xC076] (*RFC6367*)
  | TLS_ECDHE_RSA_WITH_CAMELLIA_256_CBC_SHA384   [@id 0xC077] (*RFC6367*)
  | TLS_ECDH_RSA_WITH_CAMELLIA_128_CBC_SHA256    [@id 0xC078] (*RFC6367*)
  | TLS_ECDH_RSA_WITH_CAMELLIA_256_CBC_SHA384    [@id 0xC079] (*RFC6367*)
  | TLS_RSA_WITH_CAMELLIA_128_GCM_SHA256         [@id 0xC07A] (*RFC6367*)
  | TLS_RSA_WITH_CAMELLIA_256_GCM_SHA384         [@id 0xC07B] (*RFC6367*)
  | TLS_DHE_RSA_WITH_CAMELLIA_128_GCM_SHA256     [@id 0xC07C] (*RFC6367*)
  | TLS_DHE_RSA_WITH_CAMELLIA_256_GCM_SHA384     [@id 0xC07D] (*RFC6367*)
  | TLS_DH_RSA_WITH_CAMELLIA_128_GCM_SHA256      [@id 0xC07E] (*RFC6367*)
  | TLS_DH_RSA_WITH_CAMELLIA_256_GCM_SHA384      [@id 0xC07F] (*RFC6367*)
  | TLS_DHE_DSS_WITH_CAMELLIA_128_GCM_SHA256     [@id 0xC080] (*RFC6367*)
  | TLS_DHE_DSS_WITH_CAMELLIA_256_GCM_SHA384     [@id 0xC081] (*RFC6367*)
  | TLS_DH_DSS_WITH_CAMELLIA_128_GCM_SHA256      [@id 0xC082] (*RFC6367*)
  | TLS_DH_DSS_WITH_CAMELLIA_256_GCM_SHA384      [@id 0xC083] (*RFC6367*)
  | TLS_DH_anon_WITH_CAMELLIA_128_GCM_SHA256     [@id 0xC084] (*RFC6367*)
  | TLS_DH_anon_WITH_CAMELLIA_256_GCM_SHA384     [@id 0xC085] (*RFC6367*)
  | TLS_ECDHE_ECDSA_WITH_CAMELLIA_128_GCM_SHA256 [@id 0xC086] (*RFC6367*)
  | TLS_ECDHE_ECDSA_WITH_CAMELLIA_256_GCM_SHA384 [@id 0xC087] (*RFC6367*)
  | TLS_ECDH_ECDSA_WITH_CAMELLIA_128_GCM_SHA256  [@id 0xC088] (*RFC6367*)
  | TLS_ECDH_ECDSA_WITH_CAMELLIA_256_GCM_SHA384  [@id 0xC089] (*RFC6367*)
  | TLS_ECDHE_RSA_WITH_CAMELLIA_128_GCM_SHA256   [@id 0xC08A] (*RFC6367*)
  | TLS_ECDHE_RSA_WITH_CAMELLIA_256_GCM_SHA384   [@id 0xC08B] (*RFC6367*)
  | TLS_ECDH_RSA_WITH_CAMELLIA_128_GCM_SHA256    [@id 0xC08C] (*RFC6367*)
  | TLS_ECDH_RSA_WITH_CAMELLIA_256_GCM_SHA384    [@id 0xC08D] (*RFC6367*)
  | TLS_PSK_WITH_CAMELLIA_128_GCM_SHA256         [@id 0xC08E] (*RFC6367*)
  | TLS_PSK_WITH_CAMELLIA_256_GCM_SHA384         [@id 0xC08F] (*RFC6367*)
  | TLS_DHE_PSK_WITH_CAMELLIA_128_GCM_SHA256     [@id 0xC090] (*RFC6367*)
  | TLS_DHE_PSK_WITH_CAMELLIA_256_GCM_SHA384     [@id 0xC091] (*RFC6367*)
  | TLS_RSA_PSK_WITH_CAMELLIA_128_GCM_SHA256     [@id 0xC092] (*RFC6367*)
  | TLS_RSA_PSK_WITH_CAMELLIA_256_GCM_SHA384     [@id 0xC093] (*RFC6367*)
  | TLS_PSK_WITH_CAMELLIA_128_CBC_SHA256         [@id 0xC094] (*RFC6367*)
  | TLS_PSK_WITH_CAMELLIA_256_CBC_SHA384         [@id 0xC095] (*RFC6367*)
  | TLS_DHE_PSK_WITH_CAMELLIA_128_CBC_SHA256     [@id 0xC096] (*RFC6367*)
  | TLS_DHE_PSK_WITH_CAMELLIA_256_CBC_SHA384     [@id 0xC097] (*RFC6367*)
  | TLS_RSA_PSK_WITH_CAMELLIA_128_CBC_SHA256     [@id 0xC098] (*RFC6367*)
  | TLS_RSA_PSK_WITH_CAMELLIA_256_CBC_SHA384     [@id 0xC099] (*RFC6367*)
  | TLS_ECDHE_PSK_WITH_CAMELLIA_128_CBC_SHA256   [@id 0xC09A] (*RFC6367*)
  | TLS_ECDHE_PSK_WITH_CAMELLIA_256_CBC_SHA384   [@id 0xC09B] (*RFC6367*)
  | TLS_RSA_WITH_AES_128_CCM                     [@id 0xC09C] (*RFC6655*)
  | TLS_RSA_WITH_AES_256_CCM                     [@id 0xC09D] (*RFC6655*)
  | TLS_DHE_RSA_WITH_AES_128_CCM                 [@id 0xC09E] (*RFC6655*)
  | TLS_DHE_RSA_WITH_AES_256_CCM                 [@id 0xC09F] (*RFC6655*)
  | TLS_RSA_WITH_AES_128_CCM_8                   [@id 0xC0A0] (*RFC6655*)
  | TLS_RSA_WITH_AES_256_CCM_8                   [@id 0xC0A1] (*RFC6655*)
  | TLS_DHE_RSA_WITH_AES_128_CCM_8               [@id 0xC0A2] (*RFC6655*)
  | TLS_DHE_RSA_WITH_AES_256_CCM_8               [@id 0xC0A3] (*RFC6655*)
  | TLS_PSK_WITH_AES_128_CCM                     [@id 0xC0A4] (*RFC6655*)
  | TLS_PSK_WITH_AES_256_CCM                     [@id 0xC0A5] (*RFC6655*)
  | TLS_DHE_PSK_WITH_AES_128_CCM                 [@id 0xC0A6] (*RFC6655*)
  | TLS_DHE_PSK_WITH_AES_256_CCM                 [@id 0xC0A7] (*RFC6655*)
  | TLS_PSK_WITH_AES_128_CCM_8                   [@id 0xC0A8] (*RFC6655*)
  | TLS_PSK_WITH_AES_256_CCM_8                   [@id 0xC0A9] (*RFC6655*)
  | TLS_PSK_DHE_WITH_AES_128_CCM_8               [@id 0xC0AA] (*RFC6655*)
  | TLS_PSK_DHE_WITH_AES_256_CCM_8               [@id 0xC0AB] (*RFC6655*)
  [@@uint16_t] [@@sexp]
]
