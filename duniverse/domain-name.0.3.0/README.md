## Domain-name - [RFC 1035](https://tools.ietf.org/html/rfc1035) Internet domain names

%%VERSION%%

A domain name is a sequence of labels separated by dots, such as `foo.example`.
Each label may contain any bytes. The length of each label may not exceed 63
charactes.  The total length of a domain name is limited to 253 (byte
representation is 255), but other protocols (such as SMTP) may apply even
smaller limits.  A domain name label is case preserving, comparison is done in a
case insensitive manner.

The invariants on the length of domain names are preserved throughout the
module.

## Documentation

[![Build Status](https://travis-ci.org/hannesm/domain-name.svg?branch=master)](https://travis-ci.org/hannesm/domain-name)

[API documentation](https://hannesm.github.io/domain-name/doc/) is available online.

## Installation

You need [opam](https://opam.ocaml.org) installed on your system.  The command

`opam install domain-name`

will install this library.
