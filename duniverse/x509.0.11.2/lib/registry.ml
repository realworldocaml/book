
(*
  * Object Identifiers: magic numbers with a tie. Some OIDs also have an MBA.
  *
  * http://www.alvestrand.no/objectid/
  * http://oid-info.com/
  *)

open Asn.OID

let pkix   = base 1 3 <| 6 <| 1 <| 5 <| 5 <| 7

let usa    = base 1 2 <| 840
let rsadsi = usa <| 113549
let pkcs   = rsadsi <| 1

let us_govt   = base 2 16 <| 840 <| 1 <| 101
let nist_alg  = us_govt <| 3 <| 4
let hash_algs = nist_alg <| 2

(* PKCS1 *)

let md2  = rsadsi <| 2 <| 2
and md4  = rsadsi <| 2 <| 4
and md5  = rsadsi <| 2 <| 5
and sha1 = base 1 3 <| 14 <| 3 <| 2 <| 26

(* rfc5758 *)

let sha256     = hash_algs <| 1
and sha384     = hash_algs <| 2
and sha512     = hash_algs <| 3
and sha224     = hash_algs <| 4
and sha512_224 = hash_algs <| 5
and sha512_256 = hash_algs <| 6

module ANSI_X9_62 = struct

  let ansi_x9_62 = usa <| 10045

  let ecdsa_sha1             = ansi_x9_62 <| 1
  let prime_field            = ecdsa_sha1 <| 1
  and characteristic_2_field = ecdsa_sha1 <| 2

  let key_type   = ansi_x9_62 <| 2
  let ec_pub_key = key_type <| 1

  let signatures = ansi_x9_62 <| 4
  let field_type = signatures <| 1
  and ecdsa_sha2 = signatures <| 3

  let ecdsa_sha224 = ecdsa_sha2 <| 1
  and ecdsa_sha256 = ecdsa_sha2 <| 2
  and ecdsa_sha384 = ecdsa_sha2 <| 3
  and ecdsa_sha512 = ecdsa_sha2 <| 4
end

module PKCS1 = struct
  let pkcs1 = pkcs <| 1

  let rsa_encryption           = pkcs1 <| 1
  and md2_rsa_encryption       = pkcs1 <| 2
  and md4_rsa_encryption       = pkcs1 <| 3
  and md5_rsa_encryption       = pkcs1 <| 4
  and sha1_rsa_encryption      = pkcs1 <| 5
  and ripemd160_rsa_encryption = pkcs1 <| 6
  and rsaes_oaep               = pkcs1 <| 7
  and rsassa_pss               = pkcs1 <| 10
  and sha256_rsa_encryption    = pkcs1 <| 11
  and sha384_rsa_encryption    = pkcs1 <| 12
  and sha512_rsa_encryption    = pkcs1 <| 13
  and sha224_rsa_encryption    = pkcs1 <| 14
end

module PKCS2 = struct
  let pkcs2 = rsadsi <| 2

  let md4         = pkcs2 <| 4
  and hmac_sha1   = pkcs2 <| 7
  and hmac_sha224 = pkcs2 <| 8
  and hmac_sha256 = pkcs2 <| 9
  and hmac_sha384 = pkcs2 <| 10
  and hmac_sha512 = pkcs2 <| 11
end

module PKCS5 = struct
  let pkcs5 = pkcs <| 5

  let pbe_md2_des_cbc  = pkcs5 <| 1
  and pbe_md5_des_cbc  = pkcs5 <| 3
  and pbe_md2_rc2_cbc  = pkcs5 <| 4
  and pbe_md5_rc2_cbc  = pkcs5 <| 6
  and pbe_md5_xor      = pkcs5 <| 9
  and pbe_sha1_des_cbc = pkcs5 <| 10
  and pbe_sha1_rc2_cbc = pkcs5 <| 11
  and pbkdf2           = pkcs5 <| 12
  and pbes2            = pkcs5 <| 13
  and pbmac1           = pkcs5 <| 14
end

module PKCS7 = struct
  let pkcs7 = pkcs <| 7

  let data                   = pkcs7 <| 1
  and signedData             = pkcs7 <| 2
  and envelopedData          = pkcs7 <| 3
  and signedAndEnvelopedData = pkcs7 <| 4
  and digestedData           = pkcs7 <| 5
  and encryptedData          = pkcs7 <| 6
end

module PKCS9 = struct
  let pkcs9 = pkcs <| 9

  let email                = pkcs9 <| 1
  and unstructured_name    = pkcs9 <| 2
  and content_type         = pkcs9 <| 3
  and message_digest       = pkcs9 <| 4
  and signing_time         = pkcs9 <| 5
  and challenge_password   = pkcs9 <| 7
  and unstructured_address = pkcs9 <| 8
  and signing_description  = pkcs9 <| 13
  and extension_request    = pkcs9 <| 14
  and smime_capabilities   = pkcs9 <| 15
  and smime_oid_registry   = pkcs9 <| 16
  and friendly_name        = pkcs9 <| 20
  and cert_types           = pkcs9 <| 22
end

module X520 = struct
  let x520 = base 2 5 <| 4

  let object_class                  = x520 <| 0
  and aliased_entry_name            = x520 <| 1
  and knowldgeinformation           = x520 <| 2
  and common_name                   = x520 <| 3
  and surname                       = x520 <| 4
  and serial_number                 = x520 <| 5
  and country_name                  = x520 <| 6
  and locality_name                 = x520 <| 7
  and state_or_province_name        = x520 <| 8
  and street_address                = x520 <| 9
  and organization_name             = x520 <| 10
  and organizational_unit_name      = x520 <| 11
  and title                         = x520 <| 12
  and description                   = x520 <| 13
  and search_guide                  = x520 <| 14
  and business_category             = x520 <| 15
  and postal_address                = x520 <| 16
  and postal_code                   = x520 <| 17
  and post_office_box               = x520 <| 18
  and physical_delivery_office_name = x520 <| 19
  and telephone_number              = x520 <| 20
  and telex_number                  = x520 <| 21
  and teletex_terminal_identifier   = x520 <| 22
  and facsimile_telephone_number    = x520 <| 23
  and x121_address                  = x520 <| 24
  and internationa_isdn_number      = x520 <| 25
  and registered_address            = x520 <| 26
  and destination_indicator         = x520 <| 27
  and preferred_delivery_method     = x520 <| 28
  and presentation_address          = x520 <| 29
  and supported_application_context = x520 <| 30
  and member                        = x520 <| 31
  and owner                         = x520 <| 32
  and role_occupant                 = x520 <| 33
  and see_also                      = x520 <| 34
  and user_password                 = x520 <| 35
  and user_certificate              = x520 <| 36
  and ca_certificate                = x520 <| 37
  and authority_revocation_list     = x520 <| 38
  and certificate_revocation_list   = x520 <| 39
  and cross_certificate_pair        = x520 <| 40
  and name                          = x520 <| 41
  and given_name                    = x520 <| 42
  and initials                      = x520 <| 43
  and generation_qualifier          = x520 <| 44
  and unique_identifier             = x520 <| 45
  and dn_qualifier                  = x520 <| 46
  and enhanced_search_guide         = x520 <| 47
  and protocol_information          = x520 <| 48
  and distinguished_name            = x520 <| 49
  and unique_member                 = x520 <| 50
  and house_identifier              = x520 <| 51
  and supported_algorithms          = x520 <| 52
  and delta_revocation_list         = x520 <| 53
  and attribute_certificate         = x520 <| 58
  and pseudonym                     = x520 <| 65
end

let ucl_data_networks = base 0 9 <| 2342 <| 19200300
let directory_pilot = ucl_data_networks <| 100 <| 1
(* The single rfc4519 oid rfc5280 requires us to be aware of.... *)
let domain_component = directory_pilot <| 25
(* rfc4514 oid required for compliance *)
let userid = directory_pilot <| 1

module Cert_extn = struct
  let ce = base 2 5 <| 29

  let authority_key_identifier_old  = ce <| 1
  and primary_key_attributes_old    = ce <| 2
  and certificate_policies_1        = ce <| 3
  and primary_key_usage_restriction = ce <| 4
  and subject_directory_attributes  = ce <| 9
  and subject_key_identifier        = ce <| 14
  and key_usage                     = ce <| 15
  and private_key_usage_period      = ce <| 16
  and subject_alternative_name      = ce <| 17
  and issuer_alternative_name       = ce <| 18
  and basic_constraints             = ce <| 19
  and crl_number                    = ce <| 20
  and reason_code                   = ce <| 21
  and hold_instruction_code         = ce <| 23
  and invalidity_date               = ce <| 24
  and delta_crl_indicator           = ce <| 27
  and issuing_distribution_point    = ce <| 28
  and certificate_issuer            = ce <| 29
  and name_constraints              = ce <| 30
  and crl_distribution_points       = ce <| 31
  and certificate_policies_2        = ce <| 32
  and policy_mappings               = ce <| 33
  and authority_key_identifier      = ce <| 35
  and policy_constraints            = ce <| 36
  and extended_key_usage            = ce <| 37
  and freshest_crl                  = ce <| 46
  and inhibit_any_policy            = ce <| 54

  module Extended_usage = struct
    let any              = extended_key_usage <| 0
    let key_purpose      = pkix <| 3
    let server_auth      = key_purpose <| 1
    and client_auth      = key_purpose <| 2
    and code_signing     = key_purpose <| 3
    and email_protection = key_purpose <| 4
    and ipsec_end_system = key_purpose <| 5
    and ipsec_tunnel     = key_purpose <| 6
    and ipsec_user       = key_purpose <| 7
    and time_stamping    = key_purpose <| 8
    and ocsp_signing     = key_purpose <| 9
  end

  module Cert_policy = struct
    let qt      = pkix <| 2
    let cps     = qt <| 1
    let unotice = qt <| 2

    let any_policy = certificate_policies_2 <| 0
  end
end

module Name_extn = struct

  (* For the rarely-used feature of GeneralName: AnotherName. *)

  let id_other_name = pkix <| 8

  (* rfc6120 *)
  let xmpp_addr = id_other_name <| 5
  (* rfc4985 *)
  let srv_name = id_other_name <| 7 (* an IA5String _Service.Name *)

  let venezuela   = base 2 16 <| 862
  let venezuela_1 = venezuela <| 2 <| 1
  and venezuela_2 = venezuela <| 2 <| 2

  let is_utf8_id oid =
    List.mem oid [ xmpp_addr ; venezuela_1 ; venezuela_2 ]
end
