ppx_compare
===========

Generation of fast comparison and equality functions from type
expressions and definitions.

Ppx_compare is a ppx rewriter that derives comparison and equality
functions from type representations. The scaffolded functions are
usually much faster than ocaml's `Pervasives.compare` and
`Pervasives.(=)`. Scaffolding functions also gives you more
flexibility by allowing you to override them for a specific type and
more safety by making sure that you only compare comparable values.

Syntax
------

Type definitions: `[@@deriving compare, equal]`
Expressions: `[%compare: TYPE]`, `[%equal: TYPE]` and `[%compare.equal: TYPE]`
Types, record fields: `[@compare.ignore]`, `[@equal.ignore]`

Basic usage
-----------

We use `ppx_deriving`/`ppx_type_conv`, so type definitions are annotated this way:

```ocaml
type s = v * w [@@deriving compare]
```

This will generate `compare_s : s -> s -> int` function that relies on
`compare_v : v -> v -> int` and `compare_w : w -> w -> int`.

Compare is not DWIM (do what I mean): it will scaffold a fast well behaved comparison
(reflexive, transitive, symmetric...) function however it does not try to follow any
"natural ordering". For instance arrays of characters are not sorted lexicographically.

Base types (options,int,array,lists,char,floats...) have the same comparison order as
Pervasives.compare (provided their type parameters also do for the polymorphic ones).

Records fields are compared in the order they are defined (left to right); tuples fields
are compared left to right. When we compare two branches of a sum whichever ones comes
first in the definition is considered lowest. Variants compare in the order they are
listed (increasing top-to-bottom). Polymorphic variants use the same ordering as the
ocaml runtime.

The same applies to equality functions.

### Float equality

The functions derived by `[@@deriving equal]` are consistent with the
compare functions derived by `[@@deriving compare]` and in particular
do not respect IEEE float comparison.

Calling `compare` for type `t`s
-------------------------------

In compliance (or conformance) with Janestreet's coding standard we assume that
type named `t` are the main types in a module and

```ocaml
type t = S.t * T.t [@@deriving compare]
```

will call the functions `S.compare` and `T.compare` instead of calling `S.compare_t` and
`T.compare_t`. This will also generate a `compare : t -> t -> int` function.

The same applies to equality functions.

Signature
---------

`type t [@@deriving compare]` in a module signature will add `val compare : t -> t -> int`
in the signature.

The same applies to equality functions.

Comparison without a type definition
------------------------------------

Sometimes you just want a comparison without having to create a new type. You can create
such a comparison function using the `[%compare: ..]` extension point:

```ocaml
let gt x y = [%compare: float * int * [`A | `B | `C] ] x y
```

You can use the type `_`, in which case the corresponding values will be
ignored (i.e. compared using `fun _ _ -> 0`). For instance:

```ocaml
assert ([%compare: _ list] [ true ] [ false ] = 0);
assert ([%compare: _ list] [] [ false ] <> 0);
```

The same applies to equality functions.

You can also check for equality using `[%compare.equal: ..]`, which
produces a function that returns `true` precisely when `[%compare:
..]` returns `0`. `[%equal: ..]` is prefered over `[%compare.equal:
..]` and in particular is expected to be slightly faster. However,
`[%compare.equal: ..]` can come in handy for types that only have
`[@@deriving compare]`. In particular, support for `[@@deriving
equal]` was added long after the project started, which means that
many types out there only support `[@deriving compare]`.

Ignoring part of types
----------------------

The comparison ignores any part of the type declaration that is under
a `[@compare.ignore]` annotation:

```ocaml
    type t = (float [@compare.ignore]) * string
    [@@deriving compare]
```

The same applies for `[@@deriving equal]` by using
`[@equal.ignore]`. In order to ignore part of a type for both
comparison and equality, you can simply use `[@ignore]`. However, be
aware that the general `[@ignore]` attribute will apply to any deriver
that recognize it, not just `compare` and `equal`.

Note that if you use both the `compare` and `equal` derivers, you need
to use either both `[@compare.ignore]` and `[@equal.ignore]` or
`[@ignore]`. However, you cannot use only one of them.

For convenience, you can also ignore record fields instead of the
type of record field. In other words,

```ocaml
    type t =
      { a : (float [@compare.ignore])
      ; b : string
      }
    [@@deriving compare]
```

can be abbreviated:

```ocaml
    type t =
      { a : float [@compare.ignore]
      ; b : string
      }
    [@@deriving compare]
```
