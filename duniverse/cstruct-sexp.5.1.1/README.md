Cstruct -- access C-like structures directly from OCaml
-------------------------------------------------------
%%VERSION%%

Cstruct is a library and syntax extension to make it easier to access C-like
structures directly from OCaml.  It supports both reading and writing to these
structures, and they are accessed via the `Bigarray` module.

## Installation

This repository provides several packages that can be installed via the
[opam](https://opam.ocaml.org) package manager:

- `cstruct`: the core Cstruct library
- `cstruct-sexp`: serialisers into s-expression format of Cstructs
- `cstruct-unix`: provide Unix variations of the read/write functions using file descriptors
- `cstruct-async`: provide [Async](https://github.com/janestreet/async) Pipe and Bigstring support
- `cstruct-lwt`: provide [Lwt](https://ocsigen.org/lwt) variants of read/write functions
- `ppx_cstruct`: a [PPX](https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec248) syntax extension (see below)

The libraries depend on OCaml version 4.03.0 and later, since it provides a
[ppx](http://whitequark.org/blog/2014/04/16/a-guide-to-extension-points-in-ocaml/)
extension point. The old
[camlp4](http://caml.inria.fr/pub/docs/manual-camlp4/manual002.html)
syntax extension is nolonger available; the last cstruct release which contained it
was v1.9.0.

### Local development

You can build the library via [dune](https://github.com/ocaml/dune),
using `make` or `dune build` directly.  Since everything is built via dune,
you can also place this repository within a wider dune workspace in order to
make local modifications across repositories. 

## Usage

### PPX

The PPX processor is used by passing the OCaml source code through the
`ppx_cstruct` binary. An example pcap description is:

```
[%%cstruct
type pcap_header = {
  magic_number: uint32_t;   (* magic number *)
  version_major: uint16_t;  (* major version number *)
  version_minor: uint16_t;  (* minor version number *)
  thiszone: uint32_t;       (* GMT to local correction *)
  sigfigs: uint32_t;        (* accuracy of timestamps *)
  snaplen: uint32_t;        (* max length of captured packets, in octets *)
  network: uint32_t;        (* data link type *)
} [@@little_endian]]

[%%cstruct
type pcap_packet = {
  ts_sec: uint32_t;         (* timestamp seconds *)
  ts_usec: uint32_t;        (* timestamp microseconds *)
  incl_len: uint32_t;       (* number of octets of packet saved in file *)
  orig_len: uint32_t;       (* actual length of packet *)
} [@@little_endian]]

[%%cstruct
type ethernet = {
  dst: uint8_t [@len 6];
  src: uint8_t [@len 6];
  ethertype: uint16_t;
} [@@big_endian]]

[%%cstruct
type ipv4 = {
  hlen_version: uint8_t;
  tos: uint8_t;
  len: uint16_t;
  id: uint16_t;
  off: uint16_t;
  ttl: uint8_t;
  proto: uint8_t;
  csum: uint16_t;
  src: uint8_t [@len 4];
  dst: uint8_t [@len 4];
} [@@big_endian]]
```

This auto-generates generates functions of the form below in the `ml` file:

```
let sizeof_pcap_packet = 16
let get_pcap_packet_ts_sec v = Cstruct.LE.get_uint32 v 0
let set_pcap_packet_ts_sec v x = Cstruct.LE.set_uint32 v 0 x
let get_pcap_packet_ts_usec v = Cstruct.LE.get_uint32 v 4
let set_pcap_packet_ts_usec v x = Cstruct.LE.set_uint32 v 4 x
let get_pcap_packet_incl_len v = Cstruct.LE.get_uint32 v 8
let set_pcap_packet_incl_len v x = Cstruct.LE.set_uint32 v 8 x
let get_pcap_packet_orig_len v = Cstruct.LE.get_uint32 v 12
let set_pcap_packet_orig_len v x = Cstruct.LE.set_uint32 v 12 x

let sizeof_ethernet = 14
let get_ethernet_dst src = Cstruct.sub src 0 6
let copy_ethernet_dst src = Cstruct.copy src 0 6
let set_ethernet_dst src srcoff dst =
  Cstruct.blit_from_string src srcoff dst 0 6
let blit_ethernet_dst src srcoff dst = Cstruct.blit src srcoff dst 0 6
let get_ethernet_src src = Cstruct.sub src 6 6
let copy_ethernet_src src = Cstruct.copy src 6 6
let set_ethernet_src src srcoff dst =
  Cstruct.blit_from_string src srcoff dst 6 6
let blit_ethernet_src src srcoff dst = Cstruct.blit src srcoff dst 6 6
let get_ethernet_ethertype v = Cstruct.BE.get_uint16 v 12
let set_ethernet_ethertype v x = Cstruct.BE.set_uint16 v 12 x
```

The `mli` file will have signatures of this form:

```
val sizeof_pcap_packet : int
val get_pcap_packet_ts_sec : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_ts_sec : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_ts_usec : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_ts_usec : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_incl_len : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_incl_len : Cstruct.t -> Cstruct.uint32 -> unit
val get_pcap_packet_orig_len : Cstruct.t -> Cstruct.uint32
val set_pcap_packet_orig_len : Cstruct.t -> Cstruct.uint32 -> unit
val hexdump_pcap_packet_to_buffer : Buffer.t -> pcap_packet -> unit
val hexdump_pcap_packet : Cstruct.t -> unit

val sizeof_ethernet : int
val get_ethernet_dst : Cstruct.t -> Cstruct.t
val copy_ethernet_dst : Cstruct.t -> string
val set_ethernet_dst : string -> int -> Cstruct.t -> unit
val blit_ethernet_dst : Cstruct.t -> int -> Cstruct.t -> unit
val get_ethernet_src : Cstruct.t -> Cstruct.t
val copy_ethernet_src : Cstruct.t -> string
val set_ethernet_src : string -> int -> Cstruct.t -> unit
val blit_ethernet_src : Cstruct.t -> int -> Cstruct.t -> unit
val get_ethernet_ethertype : Cstruct.t -> Cstruct.uint16
val set_ethernet_ethertype : Cstruct.t -> Cstruct.uint16 -> unit
val hexdump_ethernet_to_buffer : Buffer.t -> Cstruct.t -> unit
val hexdump_ethernet : Cstruct.t -> unit
```

The `hexdump` functions above are convenient pretty-printing functions
to help you debug, and aren't intended to be high performance.

You can also declare C-like enums:

```
[%%cenum
type foo32 =
  | ONE32
  | TWO32 [@id 0xfffffffel]
  | THREE32
  [@@uint32_t]
]

[%%cenum
type bar16 =
  | ONE [@id 1]
  | TWO
  | FOUR [@id 4]
  | FIVE
  [@@uint16_t]
]
```

This generates signatures of the form:

```
type foo32 = | ONE32 | TWO32 | THREE32
val int_to_foo32 : int32 -> foo32 option
val foo32_to_int : foo32 -> int32
val foo32_to_string : foo32 -> string
val string_to_foo32 : string -> foo32 option
val compare_foo32 : foo32 -> foo32 -> int
type bar16 = | ONE | TWO | FOUR | FIVE
val int_to_bar16 : int -> bar16 option
val bar16_to_int : bar16 -> int
val bar16_to_string : bar16 -> string
val string_to_bar16 : string -> bar16 option
val compare_bar16 : bar16 -> bar16 -> int
```

Comparisons will be done relatively to the constructor ids.

You can also add a `(sexp)` decorator to output s-expression convertors
for use with the `sexplib` library.

```
[%%cenum
type foo64 =
  | ONE64
  | TWO64
  | THREE64
  [@@uint64_t] [@@sexp]
]
```

And `sexp_of_foo64` and `foo64_of_sexp` functions will also be available.
The representation of the Sexp is the string representation of the enum.

If you do use the sexp decorator, then you will also need to add
`sexplib` to the dependency list for your package (both in the
`dune` file and the `opam` file).

Please see the `ppx_test/` directory for more in-depth examples.

[![Build Status](https://travis-ci.org/mirage/ocaml-cstruct.svg)](https://travis-ci.org/mirage/ocaml-cstruct)
