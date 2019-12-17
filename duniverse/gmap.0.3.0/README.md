## Gmap - heterogenous maps over a GADT

%%VERSION%%

Gmap exposes the functor `Make` which takes a key type (a
[GADT](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type) 'a key)
and outputs a type-safe Map where each 'a key is associated with a 'a value.
This removes the need for additional packing.  It uses OCaml's stdlib
[Map](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.html) data
structure.

```OCaml
type _ key =
  | I : int key
  | S : string key

module K = struct
  type 'a t = 'a key

  let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t = fun t t' ->
    let open Gmap.Order in
    match t, t' with
    | I, I -> Eq | I, _ -> Lt | _, I -> Gt
    | S, S -> Eq
end

module M = Gmap.Make(K)


let () =
  let m = M.empty in
  ...
  match M.find I m with
  | Some x -> Printf.printf "got %d\n" x
  | None -> Printf.printf "found nothing\n"
```

This is already an exhaustive pattern match: there is no need for another case
(for the constructor `B`) since the type system knows that looking for `A` will
result in an `int`.

Motivation came from parsing of protocols which usually specify optional values
and extensions via a tag-length-value (TLV) mechanism: for a given tag the
structure of value is different - see for example IP options, TCP options, DNS
resource records, TLS hello extensions, etc.

Discussing this problem with Justus Matthiesen during summer 2017, we came up
with this design. Its main difference to Daniel C. Bünzli's
[hmap](http://erratique.ch/software/hmap) is that in gmap the key-value GADT
type must be provided when instantiating the functor.  In hmap, keys are created
dynamically.

## Documentation

[![Build Status](https://travis-ci.org/hannesm/gmap.svg?branch=master)](https://travis-ci.org/hannesm/gmap)

[API documentation](https://hannesm.github.io/gmap/doc/) is available online.

## Installation

You need [opam](https://opam.ocaml.org) installed on your system.  The command

`opam install gmap`

will install this library.
