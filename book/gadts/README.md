# GADTs

GADTs are an increasingly common, and still somewhat mysterious corner
of the OCaml type system. This tutorial is meant to demystify them a
little.

This document introduces GADTs through a classic use-case: building
DSLs that can encode their type constraints in terms of the OCaml type
system.  I'll leave it to a later tutorial to discuss some of the ways
that we tend to use GADTs in our codebase, but this should give a
taste of what GADTs are, and why they're an effective tool.

## A little language

Let's start by showing off a little programming language implemented
in a simple style.  First, here's a signature for our language:

```ocaml env=main
open! Base

module type Simple_lang = sig
  type value =
    | Int of int
    | Bool of bool

  type expr =
    | Value of value
    | Eq of expr * expr
    | Plus of expr * expr
    | If of expr * expr * expr

  val eval : expr -> value
end
```

This is easy enough to implement as well, though the implementation is
a little ugly:

```ocaml env=main
let ill_typed () = failwith "Ill-typed expression"

module Simple_lang : Simple_lang = struct
  type value =
    | Int of int
    | Bool of bool

  type expr =
    | Value of value
    | Eq of expr * expr
    | Plus of expr * expr
    | If of expr * expr * expr

  let rec eval = function
    | Value v -> v
    | If (c, t, e) ->
      (match eval c with
       | Bool b -> if b then eval t else eval e
       | Int _ -> ill_typed ())
    | Eq (x, y) ->
      (match eval x, eval y with
       | Bool _, _ | _, Bool _ -> ill_typed ()
       | Int f1, Int f2 -> Bool (f1 = f2))
    | Plus (x, y) ->
      (match eval x, eval y with
       | Bool _, _ | _, Bool _ -> ill_typed ()
       | Int f1, Int f2 -> Int (f1 + f2))
end
```

The implementation is ugly mostly because it has a lot of dynamic
checks that are required for what are effectively type errors. And
indeed, it's totally possible to construct an ill-typed expression, so
these dynamic checks are actually necessary.

Here, you can see what happens when we construct and then evaluate an
ill-typed expression in the DSL.

```ocaml env=main
# let expr = Simple_lang.(Plus (Value (Bool true), Value (Bool false)))
val expr : Simple_lang.expr =
  Simple_lang.Plus (Simple_lang.Value (Simple_lang.Bool true),
   Simple_lang.Value (Simple_lang.Bool false))
# let x = Simple_lang.eval expr
Exception: (Failure "Ill-typed expression")
```

We can make this better, by use of another technique called phantom
types.

### Better type safety with phantom types

[*Phantom types*](./phantom-types.mlt) aren't a language feature so much
as a programming technique.  Maybe a slightly better name is *phantom
type parameters*. A phantom type parameter is a type parameter that
shows up on the left-hand-side of the definition of a type, but not
the right, e.g.:

```ocaml env=main
type _ foo = int
```

Since the phantom parameter isn't tied to the implementation of the
type, it's free, and can be constrained for whatever purpose the
designer of the interface through which `foo` is exposed wants.

Let's apply this to our little language. First, we can write down the
desired signature of our module:

```ocaml env=main
module type Phantom_lang = sig
  type 'a expr

  val int : int -> int expr
  val bool : bool -> bool expr
  val if_ : bool expr -> 'a expr -> 'a expr -> 'a expr
  val eq : 'a expr -> 'a expr -> bool expr
  val plus : int expr -> int expr -> int expr
  val int_eval : int expr -> int
  val bool_eval : bool expr -> bool
end
```

The parameter of the `expr` type is, as we'll see when we look at the
implementation in a moment, a phantom.  The constraints on
constructing ill-typed expressions here come from the way we've
written the signature. Let's write an implementation that matches this
signature:

```ocaml env=main
module Phantom_lang : Phantom_lang = struct
  (* Here is where the phantom type parameter is added *)
  type _ expr = Simple_lang.expr

  open Simple_lang

  let int x = Value (Int x)
  let bool x = Value (Bool x)
  let if_ c t e = If (c, t, e)
  let eq x y = Eq (x, y)
  let plus x y = Plus (x, y)

  let int_eval expr =
    match eval expr with
    | Int x -> x
    | Bool _ -> ill_typed ()

  let bool_eval expr =
    match eval expr with
    | Bool x -> x
    | Int _ -> ill_typed ()
end
```

Now, if we want to use this version of our language, we'll discover
that we can no longer express the ill-typed expression we had trouble
with before:

```ocaml env=main
# let expr = Phantom_lang.(plus (bool true) (bool false))
Line 1, characters 31-42:
Error: This expression has type bool expr
       but an expression was expected of type int expr
       Type bool is not compatible with type int
```

This is an improvement at the API level, but the implementation is if
anything worse.  We still have all of the messiness of the old
assert-filled implementation, and the associated performance costs.
But we've had to write yet more wrapper code to make this work.

Also, the phantom-type discipline is quite error prone.  You might
have failed to notice that I put the wrong type on the `eq` function
above.

```
# Phantom_lang.eq
- : 'a Phantom_lang.expr -> 'a Phantom_lang.expr -> bool Phantom_lang.expr =
<fun>
```

It looks like it's polymorphic over the type of expressions, but
actually it only works on integers. And, as a result, we can trigger a
dynamic failure that we didn't expect.

```ocaml env=main
# let expr = Phantom_lang.(eq (bool true) (bool false))
val expr : bool Phantom_lang.expr = <abstr>
# let x = Phantom_lang.bool_eval expr
Exception: (Failure "Ill-typed expression")
```

## Trying to do a better job with ordinary variants

We can solve the problems we had with phantom types by switching to a
new technique, GADTs, or generalized algebraic data types. GADTs are
an extension of the ordinary variants we know and love.

To see why GADTs are useful, it's helpful to see where ordinary
variants fall down.  Let's think about what would happen if we tried
to encode the typing rules we want for our DSL directly into the
definition of the expression type. In particular, we'll put an
ordinary (non-phantom) type parameter on our `expr` type, and try to
encode the type of the overall expression with that:

```ocaml env=main
module Ordinary_variant_lang = struct
  type 'a value =
    | Int of 'a
    | Bool of 'a

  type 'a expr =
    | Value of 'a value
    | Eq of 'a expr * 'a expr
    | Plus of 'a expr * 'a expr
    | If of 'a expr * 'a expr * 'a expr
end
```

This looks sort of promising at first, but it doesn't quite do what we
want.  Let's experiment a little with it:

```ocaml env=main
# module OL = Ordinary_variant_lang
module OL = Ordinary_variant_lang
# OL.(Int 3)
- : int OL.value = OL.Int 3
# OL.(Bool false)
- : bool OL.value = OL.Bool false
# OL.(Plus (Value (Int 3), Value (Int 4)))
- : int OL.expr = OL.Plus (OL.Value (OL.Int 3), OL.Value (OL.Int 4))
```

So far so good. But if you think about it for a minute, you'll realize
this doesn't actually do what we want.  For one thing, the type of the
outer expression is always just equal to the type of the inner
expression, which means that some things we want to be able to say
don't type-check.

```ocaml env=main
# OL.(If (Value (Bool true), Value (Int 3), Value (Int 4)))
Line 1, characters 39-40:
Error: This expression has type int but an expression was expected of type
         bool
```

Also, some things that shouldn't typecheck do, like this example.

```ocaml env=main
# OL.(Bool 3)
- : int OL.value = OL.Bool 3
```

## GADTs to the rescue

The problem here is that the way we want to use the type parameter
isn't supported by ordinary variants. In particular, we want the type
parameter to be populated in different ways in the different tags.
And that's precisely the extra bit of expressiveness that GADTs
provide.

Here, let's write out the type signature for a GADT type that
correctly enforces our desired typing rules:

```ocaml env=main
type _ value =
  | Int : int -> int value
  | Bool : bool -> bool value

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
```

The syntax here is worth commenting on. You can tell that you've got a
GADT by the colon after the constructor. The thing on the right
hand side of the colon looks like an ordinary function but isn't,
quite. The left-hand side of the arrow contains the arguments to the
GADT, and the right hand side tells you the type of the GADT in this
case.

Note that the right hand side always has to be an instance of the type
of the GADT; the only choice you get to make is how you fill in the
type parameter.  This lets us express things like the fact that an
`Int` instance of `value` can only contain values of type `int`, but
that a `Value` instance of `expr` can contain something of arbitrary
type.

With these type definitions, we can try some of our earlier examples.

```ocaml env=main
# Int 3
- : int value = Int 3
# Bool 3
Line 1, characters 6-7:
Error: This expression has type int but an expression was expected of type
         bool
# If (Eq (Value (Int 4), Value (Int 3)), Value (Bool false), Value (Bool true))
- : bool expr =
If (Eq (Value (Int 4), Value (Int 3)), Value (Bool false), Value (Bool true))
```

And we can also write a simple and exception-free evaluator for this type,
secure in the knowledge that no ill-typed expressions can be fed to it.

```ocaml env=main
# let value : type a. a value -> a = function
    | Int x -> x
    | Bool x -> x
val value : 'a value -> 'a = <fun>
# let rec eval : type a. a expr -> a = function
    | Value v -> value v
    | If (c, t, e) -> if eval c then eval t else eval e
    | Eq (x, y) -> eval x = eval y
    | Plus (x, y) -> eval x + eval y
val eval : 'a expr -> 'a = <fun>
```

(Don't worry about the weird type annotations just yet. We'll get to
those shortly.)

Note that the type of the eval function is exactly the polymorphic one
that we wanted, as opposed to the phantom type case, where we had two
different versions of `eval`, one for int, and one for bool.

## GADTs, type annotations, and locally abstract types

The above example lets us see some of the downsides of GADTs, which
is that code using them needs extra type annotations. Look at what
happens if we write the definition of `value` without the annotation:

```ocaml env=main
# let value = function
    | Int x -> x
    | Bool x -> x
Line 3, characters 7-13:
Error: This pattern matches values of type bool value
       but a pattern was expected which matches values of type int value
       Type bool is not compatible with type int
```

The issue here is that OCaml by default isn't willing to instantiate
ordinary type variables in different ways in the body of the same
function, which is what is required here. We can fix that by adding a
*locally abstract type*, which OCaml is willing to do that with. If
this feels like technical nonsense, it kind of is.  We actually hope
to fix the compiler so that this is no longer necessary. In any case,
this is how you'd write it:


```ocaml env=main
# let value (type a) (v : a value) : a =
    match v with
    | Int x -> x
    | Bool x -> x
val value : 'a value -> 'a = <fun>
```

Note that this isn't quite the annotation we wrote earlier.  That's because
this trick doesn't work with `eval`, as you can see below.

```ocaml env=main
# let rec eval (type a) (e : a expr) : a =
    match e with
    | Value v -> value v
    | If (c, t, e) -> if eval c then eval t else eval e
    | Eq (x, y) -> eval x = eval y
    | Plus (x, y) -> eval x + eval y
Line 4, characters 43-44:
Error: This expression has type a expr but an expression was expected of type
         bool expr
       The type constructor a would escape its scope
```

This is a pretty confusing and unhelpful error message. The real
problem is that `eval` is trying to use *polymorphic recursion*, which
is to say that it's a recursive function that tries to call itself at
a different type. In particular, the calls to `eval` within `eval` can
be for expressions of different type.  Polymorphic recursion in OCaml
can be enabled with a type annotation that makes the type explicitly
polymorphic, which is how we got to this form:


```ocaml env=main
# let rec eval : type a. a expr -> a = function
    | Value v -> value v
    | If (c, t, e) -> if eval c then eval t else eval e
    | Eq (x, y) -> eval x = eval y
    | Plus (x, y) -> eval x + eval y
val eval : 'a expr -> 'a = <fun>
```

## Summary

Phew. That's a lot to absorb.

One important takeaway is that GADTs are kind of complicated, and you
shouldn't use them lightly. There's some fundamental complexity there,
and it's made worse by the fact that OCaml's support for GADTs has
some unnecessary limitations, though that will hopefully get better
with time.

I hope to follow this up with a tutorial that covers a bit more of the
practical side of GADTs, focusing on the places where we've found
GADTs to be especially helpful.
