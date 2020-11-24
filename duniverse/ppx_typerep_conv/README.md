ppx_typerep_conv
================

Automatic generation of runtime types from type definitions.

This syntax extension defines the type-conv generator `[@@deriving typerep]`, which
creates a (runtime) value (called `typerep_of_$typename`) representing the type definition
(see `typerep` for more information). It is intended to be the main creator of values of
type `Typerep.t`.

This generator supports mostly core types, not all fancy types like union of polymorphic
variants.

## A note about signatures

In signatures, `ppx_typerep_conv` tries to generate an include of a named
interface, instead of a list of value bindings.
That is:

```ocaml
type 'a t [@@deriving typerep]
```

will generate:

```ocaml
include Typerepable.S1 with type 'a t := 'a t
```

instead of:

```ocaml
val typerep_of_t : 'a Typerep_lib.Std.Typerep.t ‑> 'a t Typerep_lib.Std.Typerep.t
val typename_of_t : 'a Typerep_lib.Std.Typename.t ‑> 'a t Typerep_lib.Std.Typename.t
```

There are however a number of limitations:
- the type has to be named t
- the type can only have up to 3 parameters
- there shouldn't be any constraint on the type parameters

If these aren't met, then `ppx_typerep_conv` will simply generate a list of value
bindings.

### Weird looking type errors

In some cases, a type can meet all the conditions listed above, in which case the
rewriting will apply, but lead to a type error. This happens when the type [t]
is an alias to a type which does have constraints on the parameters, for
instance:

```ocaml
type 'a s constraint 'a = [> `read ]
val typerep_of_s : ...
val typename_of_s : ...
type 'a t = 'a s [@@deriving_inline typerep]
include Typerep_lib.Typerepable.S1 with type 'a t := 'a t
[@@@end]
```

will give an error looking like:

```
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type 'a t = 'a t constraint 'a = [> `read ]
       is not included in
         type 'a t
       File "typerepable.mli", line 11, characters 2-11: Expected declaration
       Their constraints differ.
```

To workaround that error, simply copy the constraint on the type which has the
`[@@deriving]` annotation. This will force generating a list of value bindings.
