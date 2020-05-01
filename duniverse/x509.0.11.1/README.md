## X.509 - Public Key Infrastructure purely in OCaml

%%VERSION%%
X.509 is a public key infrastructure used mostly on the Internet.  It consists
of certificates which include public keys and identifiers, signed by an
authority.  Authorities must be exchanged over a second channel to establish the
trust relationship.  This library implements most parts of
[RFC5280](https://tools.ietf.org/html/rfc5280) and
[RFC6125](https://tools.ietf.org/html/rfc6125). The
[Public Key Cryptography Standards (PKCS)](https://en.wikipedia.org/wiki/PKCS)
defines encoding and decoding in ASN.1 DER and PEM format, which is also
implemented by this library - namely PKCS 1, PKCS 7, PKCS 8, PKCS 9 and PKCS 10.

Read [further](https://nqsb.io) and our [Usenix Security 2015 paper](https://usenix15.nqsb.io).

## Documentation

[![Build Status](https://travis-ci.org/mirleft/ocaml-x509.svg?branch=master)](https://travis-ci.org/mirleft/ocaml-x509)

[API documentation](https://mirleft.github.io/ocaml-x509/doc)

## Installation

`opam install x509` will install this library.
