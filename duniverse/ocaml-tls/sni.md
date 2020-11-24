### Server Name Indication

Some TLS servers might want to provide service for various services,
all on the same port, but with different names. The SNI extension
allows a client to request a specific server name. The server may use
the requested server name to select the X.509 certificate chain which
it presents to the client.

### Configuration interface

A user provides a full certificate chain and a private key
corresponding to the first certificate in the list to OCaml-TLS,
captured by the type `certchain`.

````
type certchain = Certificate.certificate list * Nocrypto.Rsa.priv
````

The `own_cert` polymorphic variant covers the various configuration
options: either no certificate is provided, a single one, multiple
ones (whose common name/subject alternative name are used for
disambiguation), and multiple with a default one.

````
type own_cert = [
  | `None
  | `Single of certchain
  | `Multiple of certchain list
  | `Multiple_default of certchain * certchain list
]

````

### Validation

The configuration of certificates is intertwined with ciphersuites:
each ciphersuite which requires a certificate furthermore depends on
properties of this certificate - RSA and DHE_RSA require the key to be
RSA, RSA requires the X.509v3 extension key_usage to contain
encipherment, DHE_RSA requires key_usage to contain digital_signature.
There must exist at least one certificate with the mentioned
properties for each configured ciphersuite.

Furthermore, to avoid ambiguity, the hostnames in ``Multiple` and
``Multiple_default` certificate lists must be non-overlapping.

### Certificate selection

If the server is configured with only a default certificate, this is
always used.

If the client does not request for a server name, the default
certificate is used.

If the client requests a specific server name:
 - find a strict match
 - find a wildcard match
 - use the default one if present

Only after a certificate is set for the session, the ciphersuite is
selected, depending on the properties of the certificate.
