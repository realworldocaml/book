# GADTs

GADTs, short for Generalized Algebraic Data Types, are an extension of
the variants we saw in
[Variants](variants.html#variants){data-type=xref}. They provide more
precise types that you can use to make your code safer, more concise,
and more efficient.

At the same time, GADTs are an advanced feature of OCaml, and their
power comes at a distinct cost. GADTs are harder to use and less
intuitive than ordinary variants, and it can sometimes be a bit of a
puzzle to figure out how to use them effectively. All of which is to
say that you should only use a GADT when it makes a big qualitative
improvement to your design.

But don't get me wrong, for the right use-case, GADTs can be
really transformative, and this chapter will cover some examples to
demonstrate  the range of use-cases that GADTs support.

At their heart, GADTs provide two extra features above and beyond
ordinary variants:

- They let the compiler learn more type information when you descend
  into a case of a pattern match.
- They provide a form of *existential types*, which is a form of
  data-hiding.

It's a little hard to understand these features without working
through some examples, so we'll do that next.

## A little language

One classic use-case for GADTs for writing simple typed expression
languages, similar to the boolean expression language described in
[Variants](variants.html#variants-and-recursive-data-structures){data-type=xref}.
In this section, we'll create a slightly richer language that lets us
mix arithmetic and boolean expressions. This means that we have to
deal with the possibility of ill-typed expressions, e.g., an
expression that adds a `bool` and an `int`.

Let's first try to do this with an ordinary variant. We'll declare two
types here: `value`, which represents a primitive value in the
language (i.e., an `int` or a `bool`), and `expr`, which represents
the full set of possible expressions.

```ocaml env=main
open Base

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
checks to detect what are effectively type errors. Indeed, it's
entirely possible to create an ill-typed expression which will trip
these checks.

```ocaml env=main
# let i x = Value (Int x)
  and b x = Value (Bool x)
  and (+:) x y = Plus (x,y)
val i : int -> expr = <fun>
val b : bool -> expr = <fun>
val ( +: ) : expr -> expr -> expr = <fun>
# eval (i 3 +: b false)
Exception: Ill_typed.
```

This possibility of ill-typed expressions doesn't just complicate the
implementation: it's also a problem for users, since it's all too easy
to create ill-typed expressions by mistake.

### Making the language type-safe

Let's consider what a type-safe version of this API might look like in
the absence of GADTs. To even express the type constraints, we'll need
expressions to have a type parameter to distinguish integer
expressions from boolean expressions. Given such a parameter, the
signature for such a language might look like this.

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
least, not without using GADTs. So for now, we're stuck with two
different evaluators, one for each type of expression.

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
# let i x = Value (Int x)
  and b x = Value (Bool x)
  and (+:) x y = Plus (x,y)
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
# let i x = Value (Int x)
  and b x = Value (Bool x)
  and (+:) x y = Plus (x,y)
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

The typed language we showed above is a perfectly reasonable example,
but it might give you an overly narrow sense of where GADTs show up.
In this section, we'll try to give you a sampling of other places
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
- : char option = Some B
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

Another use-case for GADTs is to narrow the set of possible states for
a given data-type in different circumstances.

One context where this can be useful is when managing complex
application state, where the available data changes over time. Let's
consider a simple example, where we're writing code to handle a logon
request from a user, and we want to check if the user in question is
authorized to logon.

We'll assume that the user logging in is authenticated as a particular
name, but that in order to authenticate, we need to do two things: to
translate that user-name into a numeric user-id, and to fetch
permissions for the service in question; once we have both, we can
check if the user-id is permitted to log on.

Without GADTs, we might model the state of a single logon request as
follows.

```ocaml env=main
type logon_request =
  { user_name : User_name.t
  ; user_id : User_id.t option
  ; permissions : Permissions.t option
  }
```

Here, `User_name.t` represents a textual name, `User_id.t` represents
an integer identifier associated with a user, and a `Permissions.t`
lets you determine which `User_id.t`'s are authorized to log in.

Here's how we might write a function for testing whether a given
request is authorized.

```ocaml env=main
# let authorized request =
    match request.user_id, request.permissions with
    | None, _ | _, None -> Error "Can't check authorization: data incomplete"
    | Some user_id, Some permissions ->
      Ok (Permissions.check permissions user_id)
val authorized : logon_request -> (bool, string) result = <fun>
```

The idea with this function is to only call it once the data is
complete, i.e., when the `user_id` and `permissions` fields have been
filled in, which is why it errors out if the data is incomplete.

This error-handling isn't really problematic in a simple case like
this. But in a real system, your code can get more complicated in
multiple ways, e.g.,

- more fields to manage, including more optional fields,
- more operations that depend on these optional fields,
- multiple requests to be handled in parallel, each of which might be
  in a different state of completion.

As this kind of complexity creeps in, it can be useful to be able to
track the state of a given request at the type level, and to use that
to narrow the set of states a given request can be in, thereby
removing some extra case analysis and error handling, which can both
reduce the complexity of the code and remove opportunities for
mistakes.

One way of doing this is to mint different types to represent
different states of the request, e.g., one type for an incomplete
request where various fields are optional, and a different type where
all of the data is mandatory.

While this works, it can be awkward and verbose. With GADTs, we can
track the state of the request in a type parameter, and have that
parameter be used to narrow the set of available cases, without
duplicating the type.

#### A completion-sensitive option type

We'll start by creating an option type that is sensitive to whether
our request is in a complete or incomplete state.

To do that, we'll need to mint types to represent the states of being
complete and incomplete. We'll define these as empty variant types,
which is a way of minting an *uninhabited type*, i.e., a type that has
no associated values. These types might as well be uninhabited, since
we're just using these types as abstract markers of different states,
and so will never need to construct a value.

```ocaml env=main
type incomplete = |
type complete = |
```

Now we can mint our new completeness-sensitive option type. Note the
two type variables: the first indicates the type of the contents of
the option, and the second indicates whether this is being used in an
incomplete state.

```ocaml env=main
type ('a, _) coption =
  | Absent : (_, incomplete) coption
  | Present : 'a -> ('a, _) coption
```

We use `Absent` and `Present` rather than `Some` or `None` to make the
code less confusing when both `option` and `coption` are used
together.

You might notice that we haven't used `complete` here explicitly.
Instead, what we've done is to ensure that a `coption` that's
`incomplete` can be `Absent` or `Present`, and a `coption` that's any
other distinct type can only be `Present`. Accordingly, a `coption`
that's `complete` (and therefore not `incomplete`) can only be a
`Present`.

This is easier to understand with some examples. Consider the
following function for getting the value out of a `coption`, returning
a default value if `Absent` is found.

```ocaml env=main
# let get ~default o =
     match o with
     | Present x -> x
     | Absent -> default
val get : default:'a -> ('a, incomplete) coption -> 'a = <fun>
```

Note that the `incomplete` type was inferred here.  If we annotate the
`coption` as `complete`, the code no longer compiles.

```ocaml env=main
# let get ~default (o : (_,complete) coption) =
    match o with
    | Absent -> default
    | Present x -> x
Line 3, characters 7-13:
Error: This pattern matches values of type ('a, incomplete) coption
       but a pattern was expected which matches values of type
         ('a, complete) coption
       Type incomplete is not compatible with type complete
```

We can make this compile by deleting the `None` branch (and the now
useless `default` argument).

```ocaml env=main
# let get (o : (_,complete) coption) =
    match o with
    | Present x -> x
val get : ('a, complete) coption -> 'a = <fun>
```

We could write this more simply as:

```ocaml env=main
# let get (Present x : (_,complete) coption) = x
val get : ('a, complete) coption -> 'a = <fun>
```

As we can see, when the `coption` is known to be `complete`, the
pattern matching is narrowed to just the `Present` case.

#### A completion-sensitive request type

We can use `coption` to define a completion-sensitive version of
`logon_request`.

```ocaml env=main
type 'c logon_request =
  { user_name : User_name.t
  ; user_id : (User_id.t, 'c) coption
  ; permissions : (Permissions.t, 'c) coption
  }
```

There's a single type parameter for the `logon_request` that marks
whether it's `complete`, at which point, both the `user_id` and
`permissions` fields will be `complete` as well.

As before, it's easy to fill in the `user_id` and `permissions`
fields.

```ocaml env=main
# let set_user_id request x = { request with user_id = Present x }
val set_user_id : 'a logon_request -> User_id.t -> 'a logon_request = <fun>
# let set_permissions request x = { request with permissions = Present x }
val set_permissions : 'a logon_request -> Permissions.t -> 'a logon_request =
  <fun>
```

Note that filling in the fields doesn't automatically mark a request
as `complete`. To do that, we need to explicitly test for
completeness, and then construct a version of the record with just the
completed fields filled in.

```ocaml env=main
# let check_completeness request =
    match request.user_id, request.permissions with
    | Absent, _ | _, Absent -> None
    | (Present _ as user_id), (Present _ as permissions) ->
      Some { request with user_id; permissions }
val check_completeness : incomplete logon_request -> 'a logon_request option =
  <fun>
```

Note that the result is polymorphic, meaning it can return a logon
request of any kind, which includes the possibility of returning a
complete request.  In practice, the function type is easier to
understand if we constrain

```ocaml env=main
# let check_completeness request : complete logon_request option =
    match request.user_id, request.permissions with
    | Absent, _ | _, Absent -> None
    | (Present _ as user_id), (Present _ as permissions) ->
      Some { request with user_id; permissions }
val check_completeness :
  incomplete logon_request -> complete logon_request option = <fun>
```

Finally, we can write an authorization checker that works
unconditionally on a complete login request.

```ocaml env=main
# let authorized (request : complete logon_request) =
    let { user_id = Present user_id; permissions = Present permissions; _ } = request in
    Permissions.check permissions user_id
val authorized : complete logon_request -> bool = <fun>
```

After all that work, the result may seem a bit underwhelming, and
indeed, most of the time, this kind of narrowing isn't worth the
complexity of setting it up. But for a sufficiently complex state
machine, cutting down on the possibilities that your code needs to
contemplate can make a big difference to the comprehensibility and
correctness of the result.

#### Other ways of narrowing

The above example might lead you to think that narrowing of pattern
matches is something that can only happen with GADTs, but that's not
quite right. OCaml can eliminate impossible cases from ordinary
variants too, but to do so, it needs to be that the case in question
is impossible from a type level.

One way to do this is via an uninhabitable type, like the ones we
discussed earlier in the chapter. We can declare our own uninhabitable
type:

```ocaml env=main
# type nothing = |
type nothing = |
```

But `Base` already has a standard uninhabited type, `Nothing.t`.

So, how does an uninhabited type help? Well, consider the `Result.t`
type, discussed in as described in [Error
Handling](error-handling.html#encoding-errors-with-result){data-type=xref}.
Normally, to match a `Result.t`, you need to handle both the `Ok` and
`Error` cases.


```ocaml env=main
# open Stdio
# let print_result (x : (int,string) Result.t) =
    match x with
    | Ok x -> printf "%d\n" x
    | Error x -> printf "ERROR: %s\n" x
val print_result : (int, string) result -> unit = <fun>
```

But if the `Error` case contains an uninhabitable, well, that case
becomes impossible, and OCaml will tell you as much.

```ocaml env=main
# let print_result (x : (int, Nothing.t) Result.t) =
    match x with
    | Ok x -> printf "%d\n" x
    | Error _ -> printf "ERROR\n"
Line 4, characters 7-14:
Warning 56 [unreachable-case]: this match case is unreachable.
Consider replacing it with a refutation case '<pat> -> .'
val print_result : (int, Nothing.t) result -> unit = <fun>
```

We can follow the advice above, and add a so-called *refutation case*.
[refutation case]{.idx}

```ocaml env=main
# let print_result (x : (int, Nothing.t) Result.t) =
    match x with
    | Ok x -> printf "%d\n" x
    | Error _ -> .
val print_result : (int, Nothing.t) result -> unit = <fun>
```

The period in the final case tells the compiler that we believe this
case can never be reached, and OCaml will verify that it's true. In
some simple cases, however, the compiler will automatically add the
refutation case for you, in particular when there is just one variant.

```ocaml env=main
# let print_result (x : (int, Nothing.t) Result.t) =
    match x with
    | Ok x -> printf "%d\n" x
val print_result : (int, Nothing.t) result -> unit = <fun>
```

One real-world case where this becomes useful is when you're using a
highly configurable library that supports multiple different modes of
use, not all of which are necessarily needed for any given
application. One example of this comes from `Async`'s RPC (remote
procedure-call) library. Async RPCs support a particular flavor of
interaction called a `State_rpc`. Such an RPC is parameterized by four types:

- A type for the initial client request
- A type for the initial snapshot returned by the server
- A type for a sequence of updates to that snapshot
- A type for an error to terminate the interaction.

Now, imagine you want to use this type, but there's no need for the
final error type. We can go ahead and instantiate the RPC using the
type `unit` for the error type.


```ocaml env=async
# open Core
# open Async
# #require "ppx_jane"
# let rpc =
    Rpc.State_rpc.create
      ~name:"int-map"
      ~version:1
      ~bin_query:[%bin_type_class: unit]
      ~bin_state:[%bin_type_class: int Map.M(String).t]
      ~bin_update:[%bin_type_class: int Map.M(String).t]
      ~bin_error:[%bin_type_class: unit]
      ()
val rpc :
  (unit, (string, int, String.comparator_witness) Map.t,
   (string, int, String.comparator_witness) Map.t, unit)
  Rpc.State_rpc.t = <abstr>
```

When you write code to dispatch the RPC, you still have to handle the
error case, even though it isn't supposed to happen.

```ocaml env=async
# let dispatch conn =
    match%bind Rpc.State_rpc.dispatch rpc conn () >>| ok_exn with
    | Ok (initial_state, updates, _) -> handle_state_changes initial_state updates
    | Error () -> failwith "this is not supposed to happen"
val dispatch : Rpc.Connection.t -> unit Deferred.t = <fun>
```

An alternative approach is to use an uninhabited type for the error:

```ocaml env=async
# let rpc =
    Rpc.State_rpc.create
      ~name:"foo"
      ~version:1
      ~bin_query:[%bin_type_class: unit]
      ~bin_state:[%bin_type_class: int Map.M(String).t]
      ~bin_update:[%bin_type_class: int Map.M(String).t]
      ~bin_error:[%bin_type_class: Nothing.t]
      ()
val rpc :
  (unit, (string, int, String.comparator_witness) Map.t,
   (string, int, String.comparator_witness) Map.t, never_returns)
  Rpc.State_rpc.t = <abstr>
```

Now, our dispatch function needs only deal with the `Ok` case.

```ocaml env=async
# let dispatch conn =
    match%bind Rpc.State_rpc.dispatch rpc conn () >>| ok_exn with
    | Ok (initial_state, updates, _) -> handle_state_changes initial_state updates
val dispatch : Rpc.Connection.t -> unit Deferred.t = <fun>
```

As you can see, narrowing can show up when using code that isn't
designed with narrowing in mind, and without any GADTs at all.

### Capturing the unknown

Code that that works with unknown types is routine in OCaml, and comes
up in the simplest of examples:

```ocaml env=main
# let tuple x y = (x,y)
val tuple : 'a -> 'b -> 'a * 'b = <fun>
```

The type variables `'a` and `'b` indicate that there are two unknown
types here, and these type variables are *universally quantified*.
Which is to say, the type of `tuple` is: *for all* types `a` and `b`,
`a -> b -> a * b`.

And indeed, we can restrict the type of `tuple` to any `'a` and `'b`
we want.

```ocaml env=main
# (tuple : int -> float -> int * float)
- : int -> float -> int * float = <fun>
# (tuple : string -> string * string -> string * (string * string))
- : string -> string * string -> string * (string * string) = <fun>
```

Sometimes, however, we want to type variables that are *existentially
quantified*, meaning that instead of being compatible with all types,
the type represents a particular but unknown type.

GADTs provide one natural way of encoding such type variables. Here's
a simple example.

```ocaml env=main
type stringable =
  Stringable : { value: 'a; to_string: 'a -> string } -> stringable
```

This type packes together a value of some arbitrary type, along with a
function for converting values of that type to strings.

We can tell that `'a` is existentially quantified because it shows up
on the left-hand side of the arrow but not on the right, so the `'a`
that shows up internally doesn't appear in a type parameter for
`stringable` itself. Essentially, the existentially quantified type is
bound within the definition of `stringable`.

This function can print an arbitrary `stringable`:

```ocaml env=main
# let print (Stringable s) =
    print_endline (s.to_string s.value)
val print : stringable -> unit = <fun>
```

And we can use this function on a collection of `stringable`s of
different underlying types.

```ocaml env=main
# let values =
    (let s value to_string = Stringable { to_string; value } in
      [ s 100 Int.to_string
      ; s 12.3 Float.to_string
      ; s "foo" Fn.id
      ])
val values : stringable list =
  [Stringable {value = <poly>; to_string = <fun>};
   Stringable {value = <poly>; to_string = <fun>};
   Stringable {value = <poly>; to_string = <fun>}]
# List.iter ~f:print values
100
12.3
foo
- : unit = ()
```

The thing that lets this all work is that the type of the underlying
object is existentially bound within the type `stringable`. As such,
the type of the underlying values can't escape the scope of
`stringable`, which means that a function that tries to do that won't
type-check.

```ocaml env=main
# let get_value (Stringable s) = s.value
Line 1, characters 32-39:
Error: This expression has type $Stringable_'a
       but an expression was expected of type 'a
       The type constructor $Stringable_'a would escape its scope
```

It's worth spending a moment to decode this error message, and the
meaning of the type variable `$Stringable_'a` in particular. You can
think of this variable as having three parts:

- The `$` marks the variable as an existential.
- `Stringable` is the name of the constructor that this variable came
  from.
- `'a` is the name of the type variable from inside that constructor.


### Abstracting computational machines

A common idiom in OCaml is to create functions, sometimes called
combinators, for building up a computational machines out of small
components.  Let's walk through a simple example of such a system, and
show how GADTs can help.

In this example, we'll consider the construction of *pipelines*, where
a pipeline is a sequence of operations arranged in linear sequence,
where each step consumes the output of the previous step, potentially
does some side effects, and exports information that can be consumed
by the next step.  This is analogous to a shell pipeline, and is
useful for all sorts of system automation tasks.

It's not totally obvious why we need anything new here. After all,
OCaml comes with a perfectly serviceable pipeline operator for
functions, as can be seen below.

```ocaml
# let extract_first_lines dir ~output =
    Sys.ls_dir dir
    |> List.filter ~f:Sys.is_file_exn
    |> List.map ~f:(fun file_name ->
         In_channel.with_file file_name ~f:(fun inc ->
             file_name, In_channel.input_line_exn inc))
# extract_first_lines "."
```

### (TODO)

Let's delve in to a more realistic example of how to use existentially
quantified types. Let's say we wanted to build a library for putting
together *pipelines*, which is to say, sequences of steps, where each
step consumes the data output by the last step, and the overall
computation returns the final result.

Now in some sense, we already have nice ways of constructing pipelines.
The `|>` operator in particular is useful for writing this kind of code.

```ocaml env=main
# let p dir =
    Core.Sys.readdir dir
    |> Array.to_list
    (* |> List.filter ~f:Core.Sys.is_file_exn *)
    |> List.iter ~f:print_endline
val p : string -> unit = <fun>
# p "."
prelude.ml
.mdx
dune
README.md
- : unit = ()
```

Such pipelines can be useful for
automating various systems-management tasks, in much the same way that
shell piplines in Bash are useful.

The signature for such a pipeline might look like this:

```ocaml env=main
module type Pipeline = sig
  type 'a t

  (** The empty pipeline *)
  val empty : 'a t

  (** Operator to build up actions into a pipeline *)
  val ( @> ) : ('a -> 'b) -> 'b t -> 'a t

  (** Executes a pipeline *)
  val exec : 'a t -> 'a -> unit
end
```

This API lets you first build up a pipeline, and then later, execute
it against different inputs.

This API is pretty straightforward to implement. Here, we'll represent
a pipeline simply as a function.

```ocaml env=main
# module Simple_pipeline : Pipeline = struct
    type 'a t = 'a -> unit

    let empty _ = ()
    let ( @> ) f p = (fun input -> p (f input))
    let exec p input = p input
  end
module Simple_pipeline : Pipeline
```

```ocaml env=main
# let p =
    let open Simple_pipeline in
    (fun dir -> Core.Sys.readdir dir)
    @> (fun dirs -> List.filter (Array.to_list dirs) ~f:Core.Sys.is_file_exn)
    @> (fun files -> List.iter files ~f:print_endline)
    @> empty
val p : string Simple_pipeline.t = <abstr>
```

```ocaml env=main
# Simple_pipeline.run p "."
Line 1, characters 1-20:
Error: Unbound value Simple_pipeline.run
```

But this isn't really buying us anything over writing the
straight-line code. After all, we could just have written:


```ocaml env=main
# let p dir =
    let dirs = Core.Sys.readdir dir in
    let files = List.filter (Array.to_list dirs) ~f:Core.Sys.is_file_exn in
    List.iter files ~f:print_endline
val p : string -> unit = <fun>
# p "."
prelude.ml
dune
README.md
- : unit = ()
```

## Limitations of GADTs

A few things.

Limitations on or-patterns.

You can't do this:

```ocaml env=main
# type _ ty =
    | I1 : int ty
    | I2 : int ty
    | B1 : bool ty
    | B2 : bool ty
type _ ty = I1 : int ty | I2 : int ty | B1 : bool ty | B2 : bool ty
# let foo (type a) (ty : a ty) (x : a) =
    match ty with
    | I1 | I2 -> Int.sexp_of_t x
    | B1 | B2 -> Bool.sexp_of_t x
Line 3, characters 32-33:
Error: This expression has type a but an expression was expected of type int
```

But you can do this:

```ocaml env=main
# let foo (type a) (ty : a ty) (x : a) =
    match ty with
    | I1 -> Int.sexp_of_t x
    | I2 -> Int.sexp_of_t x
    | B1 -> Bool.sexp_of_t x
    | B2 -> Bool.sexp_of_t x
val foo : 'a ty -> 'a -> Sexp.t = <fun>
```

So, that's weird.

Also, derivers don't always work! So, you typically don't have
`t_of_sexp` with GADTs, since the result type would need to be wrapped
in an existential type.

### Ideas

- Controlling memory layout / zero-copy networking. Maybe too
  advanced?
- Free monads and the like


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
