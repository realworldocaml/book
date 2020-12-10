# ppx_quickcheck

Generation of Base_quickcheck generators, observers, and shrinkers from types.

Syntax
-------

Type definitions: `[@@deriving quickcheck]`
Expressions: `[%quickcheck.generator: TYPE]`, `[%quickcheck.observer: TYPE]`,
and `[%quickcheck.shrinker: TYPE]`.

Basic usage
-----

We use `ppx_deriving`/`ppx_type_conv`, so type definitions are annotated this way:

```ocaml
type a = b * c [@@deriving quickcheck]
```

This generates definitions for `quickcheck_generator_a`, `quickcheck_observer_a`, and
`quickcheck_shrinker_a`. The generator definition is based on `quickcheck_generator_b` and
`quickcheck_generator_c`; likewise for the observer and shrinker.

Type `t`
--------

Following Jane Street's naming conventions, we assume that a type named `t` is the main
type in a module, and we omit the `_t` suffix for its generated definitions.

```ocaml
type t = A.t * B.t [@@deriving quickcheck]
```

This generates definitions for `quickcheck_generator`, `quickcheck_observer`, and
`quickcheck_shrinker`. The definitions are based on `A.quickcheck_generator`,
`B.quickcheck_generator`, and so on.

Signature
---------

`type t [@@deriving quickcheck]` in a module signature adds
`val quickcheck_generator : t Base_quickcheck.Generator.t`,
`val quickcheck_observer : t Base_quickcheck.Observer.t`, and
`val quickcheck_shrinker : t Base_quickcheck.Shrinker.t` to the module type.

Deriving generators, observers, and shrinkers without a type definition
-----------------------------------------------------------------------

Sometimes you just want to run Quickcheck without having to create a new type. You can
create generators, observers, and shrinkers using `[%quickcheck.generator: ...]`,
`[%quickcheck.observer: ...]`, and `[%quickcheck.shrinker: ...]`:

```ocaml
let generator = [%quickcheck.generator: float * int * [`A | `B | `C]]
let observer = [%quickcheck.observer: float * int * [`A | `B | `C]]
let shrinker = [%quickcheck.shrinker: float * int * [`A | `B | `C]]
```

Attributes
----------

The `@quickcheck.generator` attribute overrides the distribution for a type.

```ocaml
type ranking =
  { name : (string [@quickcheck.generator Generator.string_of Generator.char_alpha])
  ; high_score : (int [@quickcheck.generator Generator.int_inclusive 0 999_999])
  }
[@@deriving quickcheck]
```

The `@quickcheck.weight` attribute overrides the weight with which a variant clause is
chosen. The default weight for each clause is 1.

```ocaml
type tree =
  | Leaf
  | Node1 of tree * int * tree [@quickcheck.weight 1. /. 2.]
  | Node2 of tree * int * tree * int * tree [@quickcheck.weight 1. /. 3.]
[@@deriving quickcheck]
```

Escaping
--------

The expression extensions allow custom generators, observers, and shrinkers beyond just
the default for a given type. In place of any type, use `[%custom ...]` to fill in an
arbitrary expression.

```ocaml
let generator = [%quickcheck.generator: [%custom Generator.int_uniform] * char * string]
let observer = [%quickcheck.observer: int * [%custom Observer.opaque] * string]
let shrinker = [%quickcheck.shrinker: int * char * [%custom Shrinker.atomic]]
```
