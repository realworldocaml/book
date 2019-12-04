---
title: Bin_prot_shape
parent: ../../../ppx/ppx_bin_prot/README.md
---

What is `Bin_prot_shape`?
=========================

An extension to `bin_prot` to check safe use of deserialization.

The `bin_prot` library supports reading and writing OCaml-values via a
binary protocol.  Serialization & deserialization is performed by code
generated from the `[@@deriving bin_io]` syntax extension.  However
the type safety only pertains if the serialization & deserialization
are using the same OCaml type definitions.

Since the readers & writer may well be running in different processes,
in different executables, compiled and installed at different times,
from different OCaml code bases - it is very difficult to be sure that
the OCaml types are the same.

Values deserialized at the wrong type are essentially garbage, and
their use is unsafe.

The idea of `bin_prot_shape` is to generate a `Bin_prot.Shape.t`
description for every `Binable` type, so that shape equivalence
guarantees safety of `bin_prot`.

Readers & writers can exchange a `Shape.Digest.t` to ensure both sides
have the same opinion of the types which will be communicated.

## Two uses of shapes

* We can write unit tests for the expected shapes of types transmitted
  over a given protocol.  These would fail if the shape changes,
  perhaps unexpectedly, giving a clear indication that the protocol
  version must be incremented.

* We extend `async_rpc` to check dynamically that the protocol
  expected by client and server are shape-compatible.  Shape
  incompatibility prevents communication, and fails in a well-defined
  manner.

Motivation for shape incompatibility
====================================

## Reordering Record field

Record type definitions that differ only in the field order are
serialized differently by `bin_io`.  The shapes for these types will
not be equivalent.

```ocaml
    module R1 = struct
      type t = { foo : int; bar : string; } [@@deriving bin_io]
    end
    module R2 = struct
      type t = { bar : string; foo : int; } [@@deriving bin_io]
    end
```

## Reordering Variants

In a similar way, reordering the constructors in a variant type
changes the bin_prot serialization.  Again, the shapes for these types
will not be equivalent.

```ocaml
    type variant1 = Foo | Bar [@@deriving bin_io]
    type variant2 = Bar | Foo [@@deriving bin_io]
```

The above examples strongly motivate shape checking.  In each case the
two alternative type versions are equivalent at the OCaml type level;
changing between versions will elicit no warning from the type
checker.  However the generated `bin_prot` serializations are
incompatible and the existing runtime checking done by `bin_prot` is
insufficient to detect this and so prevent communication.

For the case of reordering variants the broken behaviour is clear:
values of `Foo` and `Bar` will be swapped when communicated.  A
catastrophic bug.

For the case of reordering record fields, the values communicated
will be garbage.  For example:

```ocaml
    utop: Binable.of_string (module R2) (Binable.to_string (module R1) { foo = 3; bar = "abc"; });;
    - : R2.t = {R2.bar = "\003ab"; foo = 99}
```

Syntax extension
====================================

Normal use of `bin_prot_shape` is via the existing
`[@@deriving bin_io]` syntax extension.  This is extended to generate
a shape description alongside the existing generation of the
`bin_prot` reader & writers.  For example, from:

```ocaml
    type t = ... [@@deriving bin_io]
```

we generate: `val bin_shape_t : Shape.t`.

For polymorphic types:

```ocaml
    type 'a t1 = ...      [@@deriving bin_io]
    type ('a,'b) t2 = ... [@@deriving bin_io]
```

We generate shape combinators or the corresponding arity:

```ocaml
    val bin_shape_t1 : Shape.t -> Shape.t
    val bin_shape_t2 : Shape.t -> Shape.t -> Shape.t
```

We also support: `[@@deriving bin_shape]` to generate only the
`[bin_shape_..]` value.  and `[%bin_shape: TYPE]` to generate an
expression of type `Shape.t`.

It is not allowed for `TYPE` to contain free type
variables. i.e. `[%bin_shape: 'a list]`

The shape generated when deriving the `bin_shape_` for a given type,
makes use of the shapes of the composed types.  For example, given:

```ocaml
    type t1 = ...      [@@deriving bin_io]
    type t2 = ...t1... [@@deriving bin_io]
```

The generated definition of `bin_shape_t2` makes use of `bin_shape_t1`
and so a binding must exist for that name.

In the case of a user defined `bin_prot` (i.e. no use of
`[@@deriving bin_io]`), then the user is responsible for declaring a
suitable `bin_shape_t` value.  Commonly this will be a new `basetype`
shape, distinct from all other shapes.  See below.

We also have syntax for new base shapes.

```ocaml
    [@@deriving bin_shape ~basetype:NAME]
```

And annotated shapes

```ocaml
    [@@deriving bin_shape ~annotate:NAME]
```

See below for details.

Runtime support
====================================

The `Shape.t` values generated and composed by `[@@deriving bin_io]`
encode a description from which shape equivalence can be determined.

The [Bin_prot] runtime library distinguishes distinct types for:

```ocaml
    Shape.t           [@@deriving          sexp_of]
    Shape.Canonical.t [@@deriving compare, sexp_of]
    Shape.Digest.t    [@@deriving compare, sexp]
```

## `Shape.t`

The base `Shape.t` corresponds directly to the OCaml type definition
or type expression.  Base shapes compose nicely, but are not directly
comparable since they contain irrelevant details such as names of type
definitions.  For example, given:

```ocaml
    type 'a pair = 'a * 'a
```

base shapes distinguish `int * int` from `int pair`.

## `Shape.Canonical.t`

A shape can be evaluated to a canonical shape: As the name implies,
the representation of canonical shapes is canonical.  Equivalence is
structural equality.

```ocaml
    val eval : Shape.t -> Shape.Canonical.t
```

A canonical shape provides a human-level description of the (shape of
a) type, which is important if we wish to explain to a human why two
types are considered incompatible

```ocaml
    val to_string_hum : Shape.Canonical.t -> string
```

## `Shape.Digest.t`

A canonical shape can be digested to a cryptographic hash, and except
for hash collisions, equality of the digests implies equality of
canonical shapes and hence equivalence at the `Shape.t` level.

```ocaml
    val to_digest : Shape.Canonical.t -> Shape.Digest.t
```

The intention is that a shape digest can be passed between
server/client of an RPC protocol to check that the both sides have the
same opinion of the types being passed.

We can convert directly from a base shape to its digest, avoiding
construction of intermediate `Shape.Canonical.t`, which can be much
more expensive.

```ocaml
    val eval_to_digest : Shape.t -> Shape.Digest.t
```

In the following when we talk about compatible or equivalent types, we
mean that the following definition of `=` would return true:

```ocaml
    let (=) x y = Shape.(eval_to_digest x = eval_to_digest y)
```

Definition of shape equivalence
====================================

We define the notion of shape equivalence w.r.t what aspects of a type
are considered significant for distinguishing one type from another,
and hence causing non-equivalence of the corresponding shapes.  Shapes
with no significant differences should be equivalent.

## Shapes corresponding to the following type construction are mutually distinct:

* built-in types: int, string..
* built-in type constructors: 'a list, 'a array,..
* user base types
* tuples
* records
* (normal) variants
* polymorphic variants
* annotated shapes

## For structured types:

* The name of built-in/base types is significant.
* The name and sub-shape of built-in/base type-constructors is
  significant.
* The type and order of tuple components is significant.
* The name, type and order of record fields is significant.
* The name, type and order of variant constructors is significant.
* The name and type (but not the order) of polymorphic variant
  constructors is significant.
* The annotation and sub-shape of an annotated shape is significant.

## Types for which shape generation is not supported:

* Polymorphic-variant inheritance from a recursive or annotated
  polymorphic-variant type.
* Function types are not supported, since functions are not
  serializable by `bin_io`.
* Universal types within records are not supported.
* GADTs, object types, class types, first-class module types are not
  supported.

## Equivalence of type aliases

Names chosen for types and type-vars are NOT significant. i.e

```ocaml
    type myint = int   [@@deriving bin_io]

    type 'a t1 = ...   [@@deriving bin_io]
    type 'b t2 = 'b t1 [@@deriving bin_io]
```

Then: `myint` and `int` are compatible, and `t1` and `t2` are compatible.

In a similar way, the order of type definitions within a mutual block
of type definitions is NOT significant.  Given:

```ocaml
    type t1 = TT of t1 | TU of u1 | TB
    and u1 = UT of t1 | UU of u1 | UB

    type u2 = UT of t2 | UU of u2 | UB
    and t2 = TT of t2 | TU of u2 | TB
```

Then: `t1` and `t2` are compatible, and `u1` and `u2` are compatible.

User defined `bin_prot`
====================================

For some types, the bin-io readers and writers are constructed by
hand, and there is no relationship between the representation of the
type in memory, and the way the type is serialized over-the-wire.  In
this case we should like a new shape distinct from any other. This is
obtained from: `Bin_prot.Shape.basetype`.

```ocaml
    val basetype : string -> Shape.t list -> Shape.t
```

The `string` argument identifies the name of the base type. The `Shape.t list` argument
allows for base types to be polymorphic. For example:

```ocaml
    type 'a t = ...
    let bin_writer_t = ...
    let bin_reader_t = ...
    ...
    let bin_shape_t bin_shape_a = Bin_prot.Shape.basetype "My.Special.t" [bin_shape_a]
```

Alternatively, we can use syntax:

```ocaml
    [@@deriving bin_shape ~basetype:NAME]
```

The above example is rewritten:

```ocaml
    type 'a t = ... [@@deriving bin_shape ~basetype:"My.Special.t"]
```

Since compatibility for basetypes is determined from their names, and
is not generative, the names are best chosen to involve something
globally unique, for example an identifier produced by `uuid` tool:

```ocaml
    type t = int [@@deriving bin_shape ~basetype:"f53adba2-4aa1-11e6-983f-479189aad583"]
```

Given the above declaration, `t` is not compatible with `int`.

Shape annotations
====================================

Sometimes we have types that are structurally identical, but
semantically incompatible.  Without user intervention this will result
in equivalent shapes.  Shape annotation allows otherwise compatible
types to be distinguished at the shape level.  Annotated shapes are
created by using: `Bin_prot.Shape.annotate`:

```ocaml
    val annotate : string -> Shape.t -> Shape.t
```

Or using syntax:

```ocaml
    [@@deriving bin_shape ~annotate:NAME]
```

For example:

```ocaml
    type dollars = float [@@deriving bin_io, bin_shape ~annotate:"dollars"]
```

Other semantic invariants might be captured in a similar way:

```ocaml
    type t = int list [@@deriving bin_io, bin_shape ~annotate:"sorted"]
```

Annotations have a similar shape-level benefit as could be achieved
via use of a record type, but without the associated cost of an extra
level of boxing.

```ocaml
    type t2 = {
      sorted : int list;
    } [@@deriving bin_io]
```

Note: the shape for `t` and `t2` are not compatible.

Basetype vs Shape annotations
====================================

It is worth emphasizing the difference between `basetype` and
`annotate`.  For example, given:

```ocaml
   type dollars1 = float [@@deriving bin_io, bin_shape ~basetype:"dollars"]
   type dollars2 = float [@@deriving bin_io, bin_shape ~annotate:"dollars"]
```

The shape for `dollars1` and `dollars2` are not compatible, and
neither is compatible with `float`.

In the first example we are defining a new basetype `dollars1` which
just happens to be serialized over the wire in the same way as a
float, but the fact of serialization like a float is not captured by
the shape for `dollars1`.

In the second example we are defining an annotated type `dollars2`
which is serialized over the wire as a float, and this fact is
captured by the shape for `dollars2.

We can regard the shape resulting from use of `annotate` as making a
stronger claim that that which results from `basetype`.

Given additionally:
```ocaml
   type dollars3 = Bignum.t [@@deriving bin_io, bin_shape ~basetype:"dollars"]
   type dollars4 = Bignum.t [@@deriving bin_io, bin_shape ~annotate:"dollars"]
```

The shape for `dollars3` is compatible with `dollars1`, whereas
`dollars4` is incompatible with `dollars2`.

Overall in this example: we can say that it is use of annotation which
is appropriate, and not a new basetype. Furthermore, the definitions
of `dollars1` and `dollars3` are something of an anti-pattern: When
`[@@deriving bin_shape ~basetype ...]` is used, we wouldn't normally
have `[@@deriving bin_io]` but instead have hand-written readers and
writers.

Async_rpc
====================================

We extend `Async_rpc` to support dynamically checking that protocols
as expected by client and server are shape-compatible.  Shape
incompatibility prevents communication, and fails in a well-defined
manner.

Shape checking is achieved via a new protocol in the `async_rpc`
framework.  To avoid confusion with the existing use of the term
"version" in `async_rpc`, which refers to the version of a specific
rpc, we refer to the version of the entire `async_rpc` framework as
the "edition" of the `async_rpc` protocol.

* `Edition.V1` - original shapeless protocol.
* `Edition.V2` - new shape-checking protocol.

When a connection is created, the set of allowed editions is
specified.  The edition is negotiated during the handshake phase of
establishing a connection.  For a successful negotiation, there must
be an edition common to both parties.

```ocaml
  Connection.create : ...
      -> ?protocol_editions : Edition.t list
      ...
```

The expected migration path for client/server apps is to move through
the following stages of allowed connections:

* `[V1]`    -- don't check shapes; even if the other side is shape-aware
* `[V1;V2]` -- check shapes as long as the other side is shape-aware
* `[   V2]` -- insist on checking shapes; refuse to communicate to
  parties not shape-aware
