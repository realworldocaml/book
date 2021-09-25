# GADTs

GADTs, short for Generalized Algebraic Data Types, are an extension of
the variants we saw in [Variants](variants.html){data-type=xref}, and
they can help you create precise types that are more precise and
thereby help you make your code safer, more concise, and more
efficient.

At the same time, GADTs are an advanced feature of OCaml, and their
power comes at a distinct cost.  GADTs are harder to use and less
intuitive than ordinary variants, and it can sometimes be a bit of a
puzzle to figure out how to use them effectively. All of which is to
say that you should only use a GADT when it makes a big qualitative
improvement to your design.

But don't get me wrong, for the the right use-case, GADTs can be
really transformative, and this chapter will cover some examples to
demnostrate the range of use-cases that GADTs support.

At their heart, GADTs provide two extra features above and beyond
ordinary variants:

- They let the compiler learn more type information when you descend
  into a case of a pattern match.
- They provide a form of *existential types*, which you can think of
  as a flexible and dynamic kind of data-hiding.

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
# let i x = Value (Int x) and b x = Value (Bool x) and (+:) x y = Plus (x,y)
val i : int -> expr = <fun>
val b : bool -> expr = <fun>
val ( +: ) : expr -> expr -> expr = <fun>
# eval (i 3 +: b false)
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

### Trying to do better with ordinary variants

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
# let i x = Value (Int x) and b x = Value (Bool x) and (+:) x y = Plus (x,y)
val i : 'a -> 'a expr = <fun>
val b : 'a -> 'a expr = <fun>
val ( +: ) : 'a expr -> 'a expr -> 'a expr = <fun>
# i 3
- : int expr = Value (Int 3)
# b false
- : bool expr = Value (Bool false)
# i 3 +: i 4
- : int expr = Plus (Value (Int 3), Value (Int 4))
```

So far so good. But if you think about it for a minute, you'll realize
this doesn't actually do what we want.  For one thing, the type of the
outer expression is always just equal to the type of the inner
expression, which means that some things we want to be able to say
don't type-check.

```ocaml env=main
# If (b true, i 3, i 4)
Line 1, characters 13-16:
Error: This expression has type int expr
       but an expression was expected of type bool expr
       Type int is not compatible with type bool
```

Also, some things that shouldn't typecheck do, like this example.

```ocaml env=main
# b 3
- : int expr = Value (Bool 3)
```

The problem here is that the way we want to use the type parameter
isn't supported by ordinary variants. In particular, we want the type
parameter to be populated in different ways in the different tags.
And that's one of the features that GADTs provide.

### GADTs to the rescue

Now we're ready write our first GADT. Here's a new version of our
`value` and `expr` types that correctly encode our desired typing
rules.

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
the tag is what tells you that this is a GADT.  To the right of the
colon, you'll see what looks like an ordinary function signature, and
you can almost think of it that way; specifically, as the type
signature for that particular constructor. The left-hand side of the
arrow states the types of the arguments to the constructor, and the
right hand side determines the type of the constructed value.

Note that in the definition of each constructor in a GADT, the
right-hand side is always the same type as the overall GADT; but the
type parameter can be different in each case, and importantly, can
depend both on the constructor and on the type of the arguments.  `Eq`
is an example where the type parameter is determined entirely by the
constructor: it always corresponds to a `bool expr`.  `If` is an
example where the type parameter depends on the arguments to the
constructor, in particular the type parameter of the `If` is the type
parameter of the then and else clauses.

Now let's construct some simple examples.

```ocaml env=main
# let i x = Value (Int x) and b x = Value (Bool x) and (+:) x y = Plus (x,y)
val i : int -> int expr = <fun>
val b : bool -> bool expr = <fun>
val ( +: ) : int expr -> int expr -> int expr = <fun>
# i 3
- : int expr = Value (Int 3)
# b 3
Line 1, characters 3-4:
Error: This expression has type int but an expression was expected of type
         bool
# i 3 +: i 6
- : int expr = Plus (Value (Int 3), Value (Int 6))
# i 3 +: b false
Line 1, characters 8-15:
Error: This expression has type bool expr
       but an expression was expected of type int expr
       Type bool is not compatible with type int
```

What we see here is that the type-safety rules we previously enforced
with signature-level restrictions on phantom types are now directly
encoded in the definition of the expression type.

These type-safety rules apply not just when constructing an
expression, but also when deconstructing one, which means we can write
a simpler and more concise evaluator that doesn't need any type-safety
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

Note that the type of the eval function is exactly the polymorphic one
that we wanted, as opposed to the phantom-type version, where we had
two different versions of `eval`, one for int, and one for bool.

### GADTs, locally abstract types, and polymorphic recursion

The above example lets us see some of the downsides of GADTs, which is
that code using them needs extra type annotations. Look at what
happens if we write the definition of `value` without the annotation:

```ocaml env=main
# let eval_value = function
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
[locally abstract types]{.idx}

```ocaml env=main
# let eval_value (type a) (v : a value) : a =
    match v with
    | Int x -> x
    | Bool x -> x
val eval_value : 'a value -> 'a = <fun>
```

Note that this isn't quite the annotation we wrote earlier.  That's
because this trick doesn't work with `eval`, as you can see below.

```ocaml env=main
# let rec eval (type a) (e : a expr) : a =
    match e with
    | Value v -> eval_value v
    | If (c, t, e) -> if eval c then eval t else eval e
    | Eq (x, y) -> eval x = eval y
    | Plus (x, y) -> eval x + eval y
Line 4, characters 43-44:
Error: This expression has type a expr but an expression was expected of type
         bool expr
       The type constructor a would escape its scope
```

This is a pretty unhelpful error message, but the basic problem is
that `eval` is recursive, and inference of GADTs doesn't play well
with recursive calls.

More specifically, the issue is that the type-checker is trying to
merge the locally abstract type `a` into the type of the recursive
function `eval`, and merging it into the outer scope within which
`eval` is defined is the way in which `a` is escaping its scope.

We can fix this by explicitly marking `eval` as polymorphic, which
OCaml has a handy type annotation for.

```ocaml env=main
# let rec eval : 'a. 'a expr -> 'a =
    fun (type a) (x : a expr) ->
     match x with
     | Value v -> eval_value v
     | If (c, t, e) -> if eval c then eval t else eval e
     | Eq (x, y) -> eval x = eval y
     | Plus (x, y) -> eval x + eval y
val eval : 'a expr -> 'a = <fun>
```

This works because, because the fact that `eval` is explicitly
polymorphic means the type of `eval` isn't specialized to `a`, and so
`a` doesn't escape its scope.

It's also helpful here because `eval` itself is an example of
*polymorphic recursion*, which is to say that `eval` needs to call
itself at multiple different types, to accommodate the fact that, for
example, the condition of an `If` is of type `bool`, even if the
overall `If` expression is of type `int`.  [polymorphic
recursion]{.idx}

As such, `eval` needs to see itself as polymorphic.  This kind of
polymorphism is basically impossible to infer automatically, which is
a second reason we need to annotate `eval`'s polymorphism explicitly.

The above syntax is a bit verbose, so OCaml has the following
syntactic sugar to combine the polymorphism annotation and the
creation of the locally abstract types.

```ocaml env=main
# let rec eval : type a. a expr -> a = function
    | Value v -> eval_value v
    | If (c, t, e) -> if eval c then eval t else eval e
    | Eq (x, y) -> eval x = eval y
    | Plus (x, y) -> eval x + eval y
val eval : 'a expr -> 'a = <fun>
```

This type of annotation is the right one to pick when you write any
recursive function that makes use of GADTs.

## When are GADTs useful?

The typed language we showed above is a fine example as far as it
goes. But GADTs are useful far beyond the realm of designing little
languages.  In this section, we'll go over some broader examples of
where GADTs can be usefully applied.

### Varying your return type

Sometimes, you want to write a single function that returns different
types in different situations.  In some sense, we do this all the
time.  After all, polymorphic functions can have return types that
depend on the types of the values they're fed.  `List.find` is a good
example.  The signature indicates that the type of the result varies
with the type of the input list.

```ocaml env=main
# List.find
- : 'a list -> f:('a -> bool) -> 'a option = <fun>
```

And of course you can use `List.find` to produce values of different
types.

```ocaml env=main
# List.find ~f:(fun x -> x > 3) [1;3;5;2]
- : int option = Some 5
# List.find ~f:(Char.is_uppercase) ['a';'B';'C']
- : char option = Some 'B'
```

But this approach is limited to simple dependencies between input and
output types that correspond concretely to how data flows through your
code.  Sometimes you want a function's return value to depend on the
arguments passed to it in less constrained ways.

For a concrete example, let's say we wanted to create a version of
`find` that is configurable in terms of how it handles the case of not
finding an item.  There are three different behaviors you might want:

- Throw an exception.
- Return `None`.
- Return a default value.

Let's try to write a function that exhibits these behaviors without
using GADTs.  First, we'll create a variant type that represents the
three possible behaviors.

```ocaml env=main
module If_not_found = struct
  type 'a t =
    | Raise
    | Return_none
    | Default_to of 'a
end
```

Now, we can write `flexible_find` which takes a `If_not_found.t` as a
parameter, and varies its behavior accordingly.

```ocaml env=main
# let rec flexible_find list ~f (if_not_found : _ If_not_found.t) =
    match list with
    | hd :: tl -> if f hd then Some hd else flexible_find ~f tl if_not_found
    | [] ->
      (match if_not_found with
      | Raise -> failwith "Element not found"
      | Return_none -> None
      | Default_to x -> Some x)
val flexible_find :
  'a list -> f:('a -> bool) -> 'a If_not_found.t -> 'a option = <fun>
```

And here's how it works.

```ocaml env=main
# flexible_find ~f:(fun x -> x > 10) [1;2;5] Return_none
- : int option = None
# flexible_find ~f:(fun x -> x > 10) [1;2;5] (Default_to 10)
- : int option = Some 10
# flexible_find ~f:(fun x -> x > 10) [1;2;5] Raise
Exception: (Failure "Element not found")
# flexible_find ~f:(fun x -> x > 10) [1;2;20] Raise
- : int option = Some 20
```

This mostly does what we want, but the problems is that
`flexible_find` always returns an option, even when it's passed
`Raise` or `Default_to`, which guarantee that the `None` case is never
used.

If we want to vary the type according to which mode we're operating
in, we're going to have to turn `If_not_found.t` into a GADT, in
particular, a GADT with two type parameters: one for the type of the
list element, and one for the return type of the function.

```ocaml env=main
module If_not_found = struct
  type (_, _) t =
    | Raise : ('a, 'a) t
    | Return_none : ('a, 'a option) t
    | Default_to : 'a -> ('a, 'a) t
end
```

The first type parameter is for the element, and the second is for the
return type. As you can see, `Raise` and `Default_to` both have the
same element type and return type, but `Return_none` provides an
optional return value.

Here's a definition of `flexible_find` that takes advantage of this
GADT.

```ocaml env=main
# let rec flexible_find
   : type a b. f:(a -> bool) -> a list -> (a, b) If_not_found.t -> b =
   fun ~f list if_not_found ->
    match list with
    | [] ->
      (match if_not_found with
      | Raise -> failwith "No matching item found"
      | Return_none -> None
      | Default_to x -> x)
    | hd :: tl ->
      if f hd
      then (
        match if_not_found with
        | Raise -> hd
        | Return_none -> Some hd
        | Default_to _ -> hd)
      else flexible_find ~f tl if_not_found
val flexible_find :
  f:('a -> bool) -> 'a list -> ('a, 'b) If_not_found.t -> 'b = <fun>
```

As you can see from the signature of `flexible_find` we now allows the
return value to vary according to `If_not_found.t`, and indeed the
functions works as you might hope, with no unnecessary options.

```ocaml env=main
# flexible_find ~f:(fun x -> x > 10) [1;2;5] Return_none
- : int option = Base.Option.None
# flexible_find ~f:(fun x -> x > 10) [1;2;5] (Default_to 10)
- : int = 10
# flexible_find ~f:(fun x -> x > 10) [1;2;5] Raise
Exception: (Failure "No matching item found")
# flexible_find ~f:(fun x -> x > 10) [1;2;20] Raise
- : int = 20
```

### Narrowing the possibilities

Another good use-case for GADTs is to narrow the set of possible
states for a given data-type in different circumstances.

This can be useful when you're managing complex state transitions,
where the data available is different at different stages.  Let's
consider a simple example, where we're writing code to handle a logon
request from a user, where we want to check if the user in question is
authorized to logon.

We'll assume that the user logging in is authenticated as a particular
user-name, but that in order to authenticate, we need to do two
things: we need to translate that user-name into a numeric user-id,
and we need to fetch permissions for the service in question; once we
have both, we can check if the user-id is in fact permitted to log on.

Without GADTs, we might model this as follows.


```ocaml env=main
module Logon_request = struct
  type t =
    { request_time : Time_ns.t
    ; user_name : User_name.t
    ; user_id : User_id.t option
    ; permissions : Permissions.t option
    }
end
```

Then, we could write functions for filling in information as it comes
in, specifically the `user_id` and `permissions`, as well as write a
function for testing whether the


### Heterogenous containers

### Controlling memory layout


### Ideas

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

1 2 3
