# ipaddr: IP and MAC address manipulation

A library for manipulation of IP and MAC address representations.

Features:

 * oUnit-based tests
 * IPv4 and IPv6 support
 * IPv4 and IPv6 CIDR prefix support
 * IPv4 and IPv6 [CIDR-scoped address](http://tools.ietf.org/html/rfc4291#section-2.3) support
 * `Ipaddr.V4` and `Ipaddr.V4.Prefix` modules are `Map.OrderedType`
 * `Ipaddr.V6` and `Ipaddr.V6.Prefix` modules are `Map.OrderedType`
 * `Ipaddr` and `Ipaddr.Prefix` modules are `Map.OrderedType`
 * `Ipaddr_unix` in findlib subpackage `ipaddr.unix` provides compatibility with the standard library `Unix` module
 * `Ipaddr_top` in findlib subpackage `ipaddr.top` provides top-level pretty printers
 * IP address scope classification
 * IPv4-mapped addresses in IPv6 (::ffff:0:0/96) are an embedding of IPv4
 * MAC-48 (Ethernet) address support
 * `Macaddr` is a `Map.OrderedType`
 * All types have sexplib serializers/deserializers optionally via the `Ipaddr_sexp` and `Macaddr_sexp` libraries.

## Usage

There are the following opam packages included:

- `ipaddr`: the `Ipaddr` and associated modules
- `ipaddr-sexp`
- `ipaddr-cstruct`
- `macaddr`: the `Macaddr` and associated modules.
- `macaddr-sexp`
- `macaddr-cstruct`

There are the following ocamlfind libraries included as part of this
repository, included as part of the respective opam packages.

- `ipaddr`: The `Ipaddr` module for IPv4/6 manipulation.
- `ipaddr.top`: Toplevel printers for Ipaddr.
- `ipaddr-cstruct`: The `Ipaddr_cstruct` module
- `macaddr`: The `Macaddr` module for MAC address manipulation.
- `macaddr.top`: Toplevel printers for Macaddr.
- `macaddr-cstruct`: The `Macaddr_cstruct` module
- `ipaddr-sexp`: S-expression converters for Ipaddr.
- `macaddr-sexp`: S-expression converters for Macaddr.

## Contact

- Issues: <https://github.com/mirage/ocaml-ipaddr/issues>
- E-mail: <mirageos-devel@lists.xenproject.org>
- API Documentation: <http://docs.mirage.io/ipaddr/>
- Discussion: <https://discuss.ocaml.org> with the `mirageos` tag.
