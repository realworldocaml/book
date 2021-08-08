# GADTs

GADTs, short for Generalized Algebraic Data Types, are a more powerful
version of the variants we saw in
[Variants](variants.html){data-type=xref}.  GADTs are a powerful
feature in OCaml, and they can help you get more precise types that
make your code simpler and more concise.  At the same time, GADTs are
harder to use and less intuitive than ordinary variants, and you
should only use them when it's a big win.

That said, when you find the right use-case, GADTs can be really
transformative, and this chapter, will cover some examples to
demnostrate the range of use-cases that GADTs support.

At their heart, GADTs provide two extra features above and beyond
ordinary variants:

- They let you learn more type information when you descend into a
  case of a pattern match
- They provide a form of *existential types*, which you can think of
  as a form of data-hiding, similar to what you find in
  object-oriented languages.

It's a little hard to understand these features without working
through some examples, so we'll do that next.

## A little language

One classic use-case for GADTs is to use it for making it easier to
write simple typed expression languages, similar to the boolean
expression language described in
[Variants](variants.html#variants-and-recursive-data-structures){data-type=xref}.
The difference is that this language will allow us to mix arithemtic
and boolean expression, and which means that we have to deal with the
possibility of ill-typed expressions, e.g., one that adds a bool and
an int.

Let's start by writing out an ordinary variant for the expression
type.  We declare two types here: `value`, which represents a
primitive value in the language (i.e., an `int` or a `bool`), and
`expr`, which represents the full set of possible expressions.

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
# exception Ill_typed
exception Ill_typed
# let rec eval expr =
    match expr with
    | Value v -> v
    | If (c, t, e) ->
      (match eval c with
       | Bool b -> if b then eval t else eval e
       | Int _ -> raise Ill_typed)
    | Eq (x, y) ->
      (match eval x, eval y with
       | Bool _, _ | _, Bool _ -> raise Ill_typed
       | Int f1, Int f2 -> Bool (f1 = f2))
    | Plus (x, y) ->
      (match eval x, eval y with
       | Bool _, _ | _, Bool _ -> raise Ill_typed
       | Int f1, Int f2 -> Int (f1 + f2))
val eval : expr -> value = <fun>
```

This implementation is a bit ugly because it has a lot of dynamic
checks to detect what are effectively type errors.  Indeed, it's
entirely possible to create an ill-typed expression, so these dynamic
checks are necessary, as you can see below.

```ocaml env=main
# eval (Plus (Value (Int 3), Value (Bool false)))
Exception: Ill_typed.
```

This is not just a problem for the implementation that needs to detect
these type errors: it's also a problem for users, since it's all too
easy to create ill-typed expressions by mistake.

Before seeing how GADTs can help here, let's see how much progress we
can make without them.

### Making the language type-safe

Let's consider what a type-safe version of this API might look like.
To even express the type constraints, we'll need expressions to have a
type parameter to distinguish integer expressions from booleain
expressions.  Given such a paramter, the signature for such a language
might look like this.

```ocaml env=main
module type Typesafe_lang_sig = sig
  type 'a t

  (** functions for constructing expressions *)

  val int : int -> int t
  val bool : bool -> bool t
  val if_ : bool t -> 'a t -> 'a t -> 'a t
  val eq : 'a t -> 'a t -> bool t
  val plus : int t -> int t -> int t

  (** Evaluation functions *)

  val int_eval : int t -> int
  val bool_eval : bool t -> bool
end
```

There's one surprising bit in this signature, which is the evaluation
functions.  You might expect there to be a single evaluation function,
with this signature.

```ocaml skip
val eval : 'a t -> 'a
```

But as we'll see, we're not going to be able to implement that, at
least, not yet. So for now, we're stuck with two different evaluators,
one for each type of expression.

Now let's write an implementation that matches this signature.

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
    | Bool _ -> raise Ill_typed

  let bool_eval expr =
    match eval expr with
    | Bool x -> x
    | Int _ -> raise Ill_typed
end
```

As you can see, the ill-typed expression we had trouble with before no
longer type-checks.

```ocaml env=main
# let expr = Typesafe_lang.(plus (int 3) (bool false))
Line 1, characters 40-52:
Error: This expression has type bool t but an expression was expected of type
         int t
       Type bool is not compatible with type int
```

So, what happened here? How did we add the type-safety we wanted?  The
fundamental trick is to add what's called a *phantom type*.
In this definition:

```ocaml skip
type 'a t = expr
```

the type parameter `'a` is the phantom type, since it doesn't show up
in the body of the definition of `t`.

Because the type parameter is unused, it's free to take on any value,
and that's why the resulting code is able to fit the signature
`Typesafe_lang_sig`.  As such, the constraints that prevent us from
constructing ill-typed expressions come from the signature, not the
implementation.

This all amounts to an improvement in terms of the API, but the
implementation is if anything worse.  We still have the same evaluator
with all of it's dynamic checking for type errors.  But we've had to
write yet more wrapper code to make this work.

Also, the phantom-type discipline is quite error prone.  You might
have missed the fact that I put the wrong type on the `eq` function
above.

```ocaml env=main
# Typesafe_lang.eq
- : 'a Typesafe_lang.t -> 'a Typesafe_lang.t -> bool Typesafe_lang.t = <fun>
```

It looks like it's polymorphic over the type of expressions, but
actually it only works on integers. And, as a result, we can trigger a
dynamic failure that we didn't expect.

```ocaml env=main
# let expr = Typesafe_lang.(eq (bool true) (bool false))
val expr : bool Typesafe_lang.t = <abstr>
# Typesafe_lang.bool_eval expr
Exception: Ill_typed.
```

This highlights why we still need the dynamic checks in the
implementation: the types within the implementation simply don't rule
out ill-typed expressions. The same fact explains why we needed two
different `eval` functions: the eval function doesn't have any
type-level guarantee of when it's handling a bool expression versus an
int expression, so it can't safely give results where the type of the
result varies based on the result of the expression.

### Trying to do a better job with ordinary variants

To see why we need GADTs, let's see what would happen if instead of
using phantom types, we tried to encode the typing rules we want for
our DSL directly into the definition of the expression type.  In
particular, we'll put an ordinary type parameter on our `expr` type,
and try to encode the type of the overall expression with that.

```ocaml env=main
type 'a value =
  | Int of 'a
  | Bool of 'a

type 'a expr =
  | Value of 'a value
  | Eq of 'a expr * 'a expr
  | Plus of 'a expr * 'a expr
  | If of 'a expr * 'a expr * 'a expr
```

This looks sort of promising at first, but it doesn't quite do what we
want.  Let's experiment a little with it:

```ocaml env=main
# Int 3
- : int value = Int 3
# Bool false
- : bool value = Bool false
# Plus (Value (Int 3), Value (Int 4))
- : int expr = Plus (Value (Int 3), Value (Int 4))
```

So far so good. But if you think about it for a minute, you'll realize
this doesn't actually do what we want.  For one thing, the type of the
outer expression is always just equal to the type of the inner
expression, which means that some things we want to be able to say
don't type-check.

```ocaml env=main
# If (Value (Bool true), Value (Int 3), Value (Int 4))
Line 1, characters 35-36:
Error: This expression has type int but an expression was expected of type
         bool
```

Also, some things that shouldn't typecheck do, like this example.

```ocaml env=main
# Bool 3
- : int value = Bool 3
```

### GADTs to the rescue

The problem here is that the way we want to use the type parameter
isn't supported by ordinary variants. In particular, we want the type
parameter to be populated in different ways in the different tags.
And that's one of the features that GADTs provide.

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

The syntax here is requires some decoding. The colon to the right of
the tag si what tells you that this is a GADT.  To the right of the
colon, you'll see what looks like an ordinary function signature, and
you can almost think of it that way. You can almost think of it as the
type signature for that particular constructor: the left-hand side of
the arrow says the types of the arguments to the constructor, and the
right hand side tells you the type of the value that you get back.

But to be clear, when defining the GADT `expr`, the right-hand side is
always of type `expr`; but the type parameter of `expr` can be
different in each case, and importantly, can depend both on the
constructor (e.g., the `Eq` constructor always corresponds to a `bool
expr`) and on the type of the arguments (e.g., the type of expression
produced by the `If` constructor depends on the type of the
expressions that form the then and else clauses.)

This lets us express things like the fact that an `Int` instance of
`value` can only contain values of type `int`, but that a `Value`
instance of `expr` can contain either an `int` or `bool`.

With these type definitions, we can try some of our earlier examples.

```ocaml env=main
# Int 3
- : int value = Int 3
# Bool 3
Line 1, characters 6-7:
Error: This expression has type int but an expression was expected of type
         bool
# Plus (Value (Int 3), Value (Int 6))
- : int expr = Plus (Value (Int 3), Value (Int 6))
# Plus (Value (Int 3), Value (Bool false))
Line 1, characters 28-40:
Error: This expression has type bool value
       but an expression was expected of type int value
       Type bool is not compatible with type int
```

What we see here is that the type-safety rules we previously enforced
with signature-level restrictions on phantom types are here directly
encoded in the definition of the expression types.

These type-safety rules apply not just when constructing an
expression, but also when desconstructiong one, which means we can
write a nice and simple evaluator that doesn't need any type-safety
checks.

```ocaml env=main
# let eval_value : type a. a value -> a = function
    | Int x -> x
    | Bool x -> x
val eval_value : 'a value -> 'a = <fun>
# let rec eval : type a. a expr -> a = function
    | Value v -> eval_value v
    | If (c, t, e) -> if eval c then eval t else eval e
    | Eq (x, y) -> eval x = eval y
    | Plus (x, y) -> eval x + eval y
val eval : 'a expr -> 'a = <fun>
```

(Don't worry about the unfamiliar type annotations just yet. We'll get
to those shortly.)

Note that the type of the eval function is exactly the polymorphic one
that we wanted, as opposed to the phantom type case, where we had two
different versions of `eval`, one for int, and one for bool.

### Type annotations and locally abstract types

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
*locally abstract type*, which doesn't have that restriction.

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




## Ideas

- Changing the return value of a function
- Controlling memory layout / zero-copy networking. Maybe too
  advanced?
- Heterogenous containers (i.e., existentials)


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
