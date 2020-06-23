## TLS - Transport Layer Security purely in OCaml

%%VERSION%%

Transport Layer Security (TLS) is probably the most widely deployed security
protocol on the Internet. It provides communication privacy to prevent
eavesdropping, tampering, and message forgery. Furthermore, it optionally
provides authentication of the involved endpoints. TLS is commonly deployed for
securing web services ([HTTPS](http://tools.ietf.org/html/rfc2818)), emails,
virtual private networks, and wireless networks.

TLS uses asymmetric cryptography to exchange a symmetric key, and optionally
authenticate (using X.509) either or both endpoints. It provides algorithmic
agility, which means that the key exchange method, symmetric encryption
algorithm, and hash algorithm are negotiated.

Read [further](https://nqsb.io) and our [Usenix Security 2015 paper](https://usenix15.nqsb.io).

## Documentation

[![Build Status](https://travis-ci.org/mirleft/ocaml-tls.svg?branch=master)](https://travis-ci.org/mirleft/ocaml-tls)

[API documentation](https://mirleft.github.io/ocaml-tls/doc)

## Installation

`opam install tls` will install this library.
