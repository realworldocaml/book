# GADTs

GADTs, short for Generalized Algebraic Data Types, are a more powerful
version of the variants we saw in
[Variants](variants.html){data-type=xref}.  GADTs are a powerful
feature in OCaml, and they can help you get more precise types that
make your code simpler and more concise.  At the same time, GADTs are
harder to use and less intuitive than ordinary variants, and you
should avoid them when they don't provide a lot of value.

That said, when you find the right use-case, GADTs can be really
transformative. (SAY MORE examples like: little languages, more
precise state machine, typed fields, performance optimizations.)

GADTs provide two extra features above and beyond ordinary variants:

- They let you learn more type information when you descend into a
  case of a pattern match
- They provide a form of *existential types*, which you can think of
  as a form of data-hiding, similar to what you find in
  object-oriented languages.

These benefits are a little hard to explain without examples, so
instead of explaining them from the beginning, we'll instead show how
GADTs can be useful in some concrete examples, and explain how GADTs
work in that context.

## A little language

We'll start off by showing how to express a simple expression language
using ordinary variants, similar to the boolean expression language
described in
[Variants](variants.html#variants-and-recursive-data-structures){data-type=xref}.
The big difference here is that this language will allow us to mix
arithemtic and boolean expression.

Let's start by writing out the type representing expressions.  We
declare two types here: `value`, which represents a primitive value in
the language (i.e., an `int` or a `bool`), and `expr`, which
represents the full set of possible expressions.

```ocaml env=main
open! Base

type value =
  | Int of int
  | Bool of bool

type expr =
  | Value of value
  | Eq of expr * expr
  | Plus of expr * expr
  | If of expr * expr * expr
```

Now, we can write a recursive evaluator in a pretty straight-ahead
style.

```ocaml env=main
# let ill_typed () = failwith "Ill-typed expression"
val ill_typed : unit -> 'a = <fun>
# let rec eval expr =
    match expr with
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
val eval : expr -> value = <fun>
```

This implementation is a bit ugly because it has a lot of dynamic
checks to detect what are effectively type errors.  And indeed, it's
totally possible to pass an ill-typed expression, so these dynamic
checks are quite necessary, as you can see below.

```ocaml env=main
# eval (Plus (Value (Int 3), Value (Bool false)))
Exception: (Failure "Ill-typed expression")
```

This is not just a problem for the implementation that needs to detect
these type errors: it's also a problem for users, since it's all too
easy for them to create ill-typed expressions.

Before getting in to GADTs, let's see how much progress we can make
without them, first, by making the language type-safe for users.

### Making the language type-safe

Let's consider what a type-safe version of this API might look like.
We'll need a type for expressions that has a type parameter, to
distinguish integer expressions from booleain expressions.  That
expression might look something like this:

```ocaml env=main
module type Typesafe_lang_sig = sig
  type 'a t

  (** functions for constructing expressions *)

  val int : int -> int t
  val bool : bool -> bool t
  val if_ : bool t -> 'a t -> 'a t -> 'a t
  val eq : _ t -> _ t -> bool t
  val plus : int t -> int t -> int t

  (** Evaluation functions *)

  val int_eval : int t -> int
  val bool_eval : bool t -> bool
end
```

The oddity here is that we have two evaluation functions: one for
evaluating integer expressions, and one for evaluating bool
expressions.  Ideally, you'd like a single eval function with the type
`'a t -> 'a`.  But as we'll see, we don't have a good way of
implementing a function with this type without GADts.

The constraints on constructing ill-typed expressions here come from
the way we've written the signature.  Let's write an implementation
that matches this signature.

```ocaml env=main
module Typesafe_lang : Typesafe_lang_sig = struct
  type 'a t = expr

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

With this language, we'll discover that we can no longer express the
ill-typed expression we had trouble with before.

```ocaml env=main
# let expr = Typesafe_lang.(plus (int 3) (bool false))
Line 1, characters 40-52:
Error: This expression has type bool t but an expression was expected of type
         int t
       Type bool is not compatible with type int
```

So, what happened here? How did we add the type-safety we wanted?  The
fundamental trick is to add what's called a *phantom type*.
Specifically, the definition `type 'a t = expr` is where the phantom
type comes in.  In particular, the type parameter `'a` is the phantom,
because it doesn't show up in the body of the definition of `t`.

Because the type parameter is unused, it's free to take on any value
you want, and that's why the resulting code is able to fit the
signature `Typesafe_lang_sig`.  So, the constraints that prevent us
from constructing ill-typed expressions, really come from the
signature, not the implementation.

Indeed, the way it's written, there's a bug in the signature! The
signature of `eq` is:

```ocaml skip
val eq : _ t -> _ t -> bool t
```

but should have been:

```ocaml skip
val eq : 'a t -> 'a t -> bool t
```

As a result, it turns out we can still create an ill-typed expression,
which will then cause the evaluator to fail at runtime.

```ocaml env=main
# let expr = Typesafe_lang.(eq (int 3) (bool false))
val expr : bool Typesafe_lang.t = <abstr>
# Typesafe_lang.bool_eval expr
Exception: (Failure "Ill-typed expression")
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
Line 1, characters 12-24:
Error: Unbound module Phantom_lang
# let x = Phantom_lang.bool_eval expr
Line 1, characters 9-31:
Error: Unbound module Phantom_lang
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



## Detritus

Aren't a language feature so much
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
