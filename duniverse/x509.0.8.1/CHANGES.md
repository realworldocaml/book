## v0.8.1 (2019-10-10)

* export Private_key.decode_der and encode_der

## v0.8.0 (2019-10-10)

* export X509.Distinguished_name.common_name : t -> string option, which
  extracts the common name of a distinguished name
* Distinguished_name.t is now a Relative_distinguished_name.t list, a
  Relative_distinguished_name is a Set.S with element type attribute, a variant.
  It used to be an attribute (expressed as GADT) Gmap.t, but this representation
  did not conform to RFC 5280, reported by @paurkedal (#117, fixed by #118)
* Now using Set.find_first_opt, which bumps lower OCaml bound to 4.05.0
* Improved pretty-printing for DNs including RFC 4514 conformance (@paurkedal, #119).
* Extension.pp now outputs extension key and its value (#120)
* rename Distinguished_name.SP constructor (stateOrProvince) to ST, as widely used (#121)
* support Street and UID in Distinguished_name to satisfy RFC 4514 demands (#121)

## v0.7.1 (2019-08-09)

* revert General_name.t (DNS and IP components) to string/Cstruct.t list
  - NameConstraints uses DNSname with strings with leading dots (.example.com),
    which are not valid RFC1034 domain names
  - NameConstraints uses IP with IP/netmask, i.e. using 8 octets for an IPv4
    address
  - X509.Certificate.hostnames still return a Domain_name.Set.t
  - reported by @reynir
* drop ipaddr dependency

## 0.7.0 (2019-07-24)

* major restructuring, it is unlikely any pre-0.7.0 users will work with 0.7.0+
* remove sexp de&encoders
* provide pretty-printers for validation errors (and types) instead of to_string functions
* use result type and Rresult instead of custom result types and control monad
* use a GADT map for certificate & csr extensions, distinguished names, general names (avoiding multiple extensions with the same OID, uses the gmap library)
* use domain-name library for hostname validation (instead of custom string matching)
* use ipaddr library for IPs in SubjectAlternativeName extension
* remove Encoding module, provide {en,de}code_{der,pem} in the respective modules (which decoders return (_, [> `Msg of string ]) result, no exceptions raised)
* fix DistributionPoint extension: the CRLissuer is a GeneralName, not a DistinguishedName
* remove Extension.reason_code (Extension.reason was there before, and is now used)
* remove bindings from toplevel, t is now Certificate.t, public_key is now Public_key.t
* use alcotest instead of oUnit

## 0.6.3 (2019-04-02)

* provide X509.Encoding.distinguished_name_of_cs -- similar to #87 which provided distinguished_name_to_cs
* provide X509.Encoding.{public_key_of_cstruct,public_key_to_cstruct}, as requested by @dinosaure
* support of cstruct 4.0.0, which split up the sexp de&encoders
* removes result dependency (now requires >= 4.04.2)
* upgrades opam file to version 2.0
* build system is now dune

## 0.6.2 (2018-08-24)

* compatibility with ppx_sexp_conv >v0.11.0 (#109), required for 4.07.0

## 0.6.1 (2017-12-21)

* provide X509.distinguished_name sexp converter (#103)
* drop non-exported X509_types module from distinguished_name (#102, @yomimono)

## 0.6.0 (2017-12-13)

* Certificate Revocation List (CRL) support (#99)
* track asn1-combinators 0.2.0 changes (#97)
* provide Extension.subject_alt_names (#95)
* compute length of certificate length, instead of hardcoding 4 (#95)
* enable safe-string (#89)
* use astring instead of custom String_ext.split (#89)
* use topkg instead of oasis (#88, #89)
* provide Encoding.cs_of_distinguished_name (#87 by @reynir)

## 0.5.3 (2016-09-13)

* provide Encoding.parse_signing_request and Encoding.cs_of_signing_request (#81)
* provide validity : t -> (Time.t * Time.t) (#86, fixes #85)

## 0.5.2 (2016-04-13)

* fix building of certificate paths

## 0.5.1 (2016-03-21)

* use ppx_sexp_conv instead of sexplib.syntax
* no more Stream syntax, use lists

## 0.5.0 (2015-12-04)

* avoid dependency on sexplib.syntax (#55)
* document how to combine extensions and a CSR into a certificate (@reynir, #63 #64)
* expose `fingerprint : t -> hash -> Cstruct.t`, the hash of the certificate (@cfcs, #66)
* trust_fingerprint / server_fingerprint are renamed to trust_cert_fingerprint / server_cert_fingerprint (now deprecated!)
* fingerprint public keys (rather than certificates): trust_key_fingerprint / server_key_fingerprint
* build certificate paths from the received set (RFC 4158) instead of requiring a strict chain (#74)
* the given trust anchors to `Authenticator.chain_of_trust` are not validated (to contain KeyUsage / BasicConstraint extensions) anymore, users can use `valid_ca` and `valid_cas` to filter CAs upfront

## 0.4.0 (2015-07-02)

* certificate signing request support (PKCS10)
* basic CA functionality (in CA module): create and sign certificate signing requests
* PEM encoding of X.509 certificates, RSA public and private keys, and certificate signing requests
* new module Extension contains X509v3 extensions as polymorphic variants
* expose distinguished_name as polymorphic variant
* type pubkey is now public_key
* function cert_pubkey is now public_key
* functions supports_usage, supports_extended_usage are now in Extension module
* types key_usage, extended_key_usage are now in Extension module
* Encoding.Pem.Cert has been renamed to Encoding.Pem.Certificate
* Encoding.Pem.PK has been renamed to Encoding.Pem.Private_key (now uses type private_key instead of Nocrypto.Rsa.priv)

## 0.3.1 (2015-05-02)

* PKCS8 private key info support (only unencrypted keys so far)

## 0.3.0 (2015-03-19)

* more detailed error messages (type certificate_failure modified)
* no longer Printf.printf debug messages
* error reporting: `Ok of certificate option | `Fail of certificate_failure
* fingerprint verification can work with None as host (useful for client authentication where host is not known upfront)
* API reshape: X509 is the only public module, X509.t is the abstract certificate

## 0.2.1 (2014-12-21)

* server_fingerprint authenticator which validates the server certificate based on a hash algorithm and (server_name * fingerprint) list instead of a set of trust anchors
* whitelist CAcert certificates (which do not include mandatory X.509v3 KeyUsage extension)

## 0.2.0 (2014-10-30)

* expose Certificate.cert_hostnames, wildcard_matches
* Certificate.verify_chain_of_trust and X509.authenticate both return now
  [ `Ok of certificate | `Fail of certificate_failure ], where [certificate] is the trust anchor

## 0.1.0 (2014-07-08)

* initial beta release
