# Fiat-p256

`fiat-p256` contains primitives for ECDH key exchange algorithm over NIST curve P-256.

It internally uses bindings to C code generated using the correct-by-construction implementations from
[fiat-crypto](https://github.com/mit-plv/fiat-crypto).

Please be aware that cryptographic primitives should not be used in end applications, they are better
used as part of a higher level cryptographic library.

[![Build Status](https://travis-ci.org/mirage/fiat.svg?branch=master)](https://travis-ci.org/mirage/fiat) [![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/fiat/doc/)

## Installation

`fiat-p256` is available on opam and can be install as follows:

```
opam install fiat-p256
```

## Usage

The entry point to this library is the `Fiat_p256` module and the main function is `dh` which let
you perform a key exchange given your private key `scalar` and the other party's public key `point`:

```ocaml
let secret = Fiat_p256.dh ~scalar ~point
```

Note that the `point` values built or parsed using `Fiat_p256`'s interface are checked
according to NIST's
[Recommendation for Pair-Wise Key Establishment Schemes Using Discrete Logarithm Cryptography](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-56Ar2.pdf)
section _5.6.2.3.2_ so that you shouldn't be able to provide an invalid point or the point at
infinity to the functions exposed in this module.

You can also compute the public P-256 key corresponding to your private key `scalar` using the `public`
function:

```ocaml
let public_key = Fiat_p256.public ~scalar
```

Note that the `scalar` values parsed using `Fiat_p256`'s interface must be within P-256's generator
subgroup order range so that your public key can't be the point at infinity. Any scalar not in the
range `[1 - (n-1)]`, `n` being the group order, will be rejected.
