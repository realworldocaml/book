ppx_let
=======

A ppx rewriter for monadic and applicative let bindings, match expressions, and
if expressions.

Overview
--------

The aim of this rewriter is to make monadic and applicative code look nicer by
writing custom binders the same way that we normally bind variables. In OCaml,
the common way to bind the result of a computation to a variable is:

```ocaml
let VAR = EXPR in BODY
```

ppx\_let simply adds two new binders: `let%bind` and `let%map`. These are
rewritten into calls to the `bind` and `map` functions respectively. These
functions are expected to have

```ocaml
val map  : 'a t -> f:('a -> 'b)   -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
```

for some type `t`, as one might expect.

These functions are to be provided by the user, and are generally expected to be
part of the signatures of monads and applicatives modules. This is the case for
all monads and applicatives defined by the Jane Street's Core suite of
libraries. (see the section below on getting the right names into scope).

### Parallel bindings

ppx\_let understands parallel bindings as well. i.e.:

```ocaml
let%bind VAR1 = EXPR1 and VAR2 = EXPR2 and VAR3 = EXPR3 in BODY
```

The `and` keyword is seen as a binding combination operator. To do so it expects
the presence of a `both` function, that lifts the OCaml pair operation to the
type `t` in question:

```ocaml
val both : 'a t -> 'b t -> ('a * 'b) t
```

Some applicatives have optimized `map` functions for more than two arguments.
These applicatives will export functions like `map4` shown below:

```ocaml
val map4: 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'r) -> 'r t
```

In order to use these optmized functions, ppx\_let provides the `let%mapn`
syntax, which picks the right `map{n}` function to call based on the amount of
applicatives bound by the syntax.

### Match statements

We found that this form was quite useful for match statements as well. So for
convenience ppx\_let also accepts `%bind` and `%map` on the `match` keyword.
Morally `match%bind expr with cases` is seen as `let%bind x = expr in match x
with cases`.

### If statements

As a further convenience, ppx\_let accepts `%bind` and `%map` on the `if`
keyword. The expression `if%bind expr1 then expr2 else expr3` is morally
equivalent to `let%bind p = expr1 in if p then expr2 else expr3`.

### Function statements

We accept `function%bind` and `function%map` too.

```ocaml
let f = function%bind
  | Some a -> g a
  | None -> h
```

is equivalent to

```ocaml
let f = fun temp ->
  match%bind temp with
  | Some a -> g a
  | None -> h
```

### While statements

We also expand `while%bind expr1 do expr2 done` as

```ocaml
let rec loop () =
  if%bind expr1
  then (
    let%bind () = expr2 in
    loop ())
  else return ()
in loop ()
```

Note that this form will (potentially) evaluate the textual form of
expr1 multiple times!

We do not support `while%map`, as that cannot be implemented without
`bind`.

Syntactic forms and actual rewriting
------------------------------------

`ppx_let` adds seven syntactic forms

```ocaml
let%bind P = M in E

let%map  P = M in E

let%sub P = M in E

match%bind M with P1 -> E1 | P2 -> E2 | ...

match%map  M with P1 -> E1 | P2 -> E2 | ...

if%bind M then E1 else E2

if%map  M then E1 else E2

while%bind M do E done
```

that expand into

```ocaml
bind M ~f:(fun P -> E)


map  M ~f:(fun P -> E)

sub M ~f:(fun P -> E)

bind M ~f:(function P1 -> E1 | P2 -> E2 | ...)

map  M ~f:(function P1 -> E1 | P2 -> E2 | ...)

bind M ~f:(function true -> E1 | false -> E2)

map  M ~f:(function true -> E1 | false -> E2)

let rec loop () = bind M ~f:(function true -> bind E ~f:loop | false -> return ()) in loop ()
```

respectively.

As with `let`, `let%bind` and `let%map` also support multiple *parallel*
bindings via the `and` keyword:

```ocaml
let%bind P1 = M1 and P2 = M2 and P3 = M3 and P4 = M4 in E

let%map  P1 = M1 and P2 = M2 and P3 = M3 and P4 = M4 in E
```

that expand into

```ocaml
let x1 = M1 and x2 = M2 and x3 = M3 and x4 = M4 in
bind
  (both x1 (both x2 (both x3 x4)))
  ~f:(fun (P1, (P2, (P3, P4))) -> E)

let x1 = M1 and x2 = M2 and x3 = M3 and x4 = M4 in
map
  (both x1 (both x2 (both x3 x4)))
  ~f:(fun (P1, (P2, (P3, P4))) -> E)
```

respectively. (Instead of `x1`, `x2`, ... ppx\_let uses variable names that are
unlikely to clash with other names)

Unlike `let%map` and `let%bind`, `let%sub` does _not_ permit
multiple bindings via the `and` keyword.

As with `let`, names introduced by left-hand sides of the let bindings are not
available in subsequent right-hand sides of the same sequence.

Getting the right names in scope
--------------------------------

The description of how the `%bind` and `%map` syntax extensions expand left out
the fact that the names `bind`, `map`, `both`, and `return` are not used
directly., but rather qualified by `Let_syntax`. For example, we use
`Let_syntax.bind` rather than merely `bind`.

This means one just needs to get a properly loaded `Let_syntax` module
in scope to use `%bind` and `%map`. The intended way to do this is to
create a module `Let_syntax` with a signature like:
```ocaml
module Let_syntax : sig
  module Let_syntax : sig
    val bind : ...
    val map : ...
    ...
  end
  ...
end
```
and then use `open Let_syntax` to make the inner `Let_syntax` module
available.

Alternatively, the extension can use values from a `Let_syntax` module
other than the one in scope. If you write `%map.A.B.C` instead of
`%map`, the expansion will use `A.B.C.Let_syntax.Let_syntax.map`
instead of `Let_syntax.map` (and similarly for all extension points).

For monads, `Core.Monad.Make` produces a submodule `Let_syntax` of the
appropriate form.

For applicatives, the convention for these modules is to have a submodule
`Let_syntax` of the form:

```ocaml
module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val map    : 'a t -> f:('a -> 'b) -> 'b t
    val both   : 'a t -> 'b t -> ('a * 'b) t
    module Open_on_rhs : << some signature >>
  end
end
```

The `Open_on_rhs` submodule is used by variants of `%map` and `%bind` called
`%map_open` and `%bind_open`. It is locally opened on the right hand sides of
the rewritten let bindings in `%map_open` and `%bind_open` expressions. For
`match%map_open` and `match%bind_open` expressions, `Open_on_rhs` is opened for
the expression being matched on.

`Open_on_rhs` is useful when programming with applicatives, which operate in a
staged manner where the operators used to construct the applicatives are
distinct from the operators used to manipulate the values those applicatives
produce. For monads, `Open_on_rhs` contains `return`.


let%sub
-------

`let%sub` is a form almost equivalent to `let%bind` but calling a function
called [sub] instead of [bind]. The intended use case is for things which have
a "bind-like" operation with a type like:

```ocaml
val sub : 'a t -> f:('a s -> 'b t) -> 'b t
```

(e.g. a relative monad) The name comes from the quintessential example
of such an operation: substitution of terms for variables.  We didn't
want to just use [let%bind] for such functions as it might confuse
people.

There is one large difference between `let%sub` and `let%bind` stemming from
the difference in the expected signatures of `sub` and `bind`. Since the value
passed into `f` is not totally "unwrapped", it cannot be directly destructured.
Because accessing the components of complex structures is often desirable,
`let%sub` does the extra work. The below code snippet

```ocaml
let%sub a, b = c in
BODY
```

gets roughly translated to

```ocaml
let%sub temp_var = c in
let%sub a = return (map ~f:(fun (a, _) -> a) temp_var) in
let%sub b = return (map ~f:(fun (_, b) -> a) temp_var) in
BODY
```

The one potentially unexpected part of this is the usage of `return` followed
immediately by `let%sub`, which seems like a no-op. Why not do this instead?

```ocaml
let%sub temp_var = c in
let a = map ~f:(fun (a, _) -> a) temp_var in
let b = map ~f:(fun (_, b) -> a) temp_var in
BODY
```

The difference is that the second option binds `a` and `b` to the
*computations* that map `temp_var` to its components, but the first option
binds `a` and `b` to the components *after the mappings have occurred*.
Conceptually this means that for the first, correct desugaring, reusing `a` and
`b` does not duplicate the mapping computation, but for the second desugaring, every
usage of `a` or `b` refers to a duplicate of its computation.

match%sub
---------

Rather than depending on a `bind` operation, `match%sub` depends on the
presence of a `switch` function (the concrete Bonsai version is shown below).
Note that the type of the expression being matched is `Value.t` rather than
`Computation.t`.

```ocaml
val switch
  :  match_:int Value.t
  -> branches:int
  -> with_:(int -> 'a Computation.t)
  -> 'a Computation.t
```

Example:

```ocaml
let f (either_value : (_, _) Either.t Value.t) page1 page2 =
  let open Bonsai.Let_syntax in
  match%sub either_value with
  | First (a, b) -> page1 a b
  | Second x -> page2 x
;;
```

expands to roughly the following

```ocaml
let f (either_value : (_, _) Either.t Value.t) page1 page2 =
  let open Bonsai.Let_syntax in
  let%sub either_value = return either_value in
  Let_syntax.switch
    ~match_:
      (match%map either_value with
       | First (_, _) -> 0
       | Second _ -> 1)
    ~branches:2
    ~with_:(function
      | 0 ->
        let%sub a =
          return
            (match%map either_value with
             | First (a, _) -> a
             | _ -> assert false)
        in
        let%sub b =
          return
            (match%map either_value with
             | First (_, b) -> b
             | _ -> assert false)
        in
        page1 a b
      | 1 ->
        let%sub x =
          Bonsai.Let_syntax.return
            (match%map either_value with
             | Second x -> x
             | _ -> assert false)
        in
        page2 x
      | _ -> assert false)
;;
```

let%arr
-------

One way of looking at arrow programs (i.e. programs involving
`let%sub`) is that `let%map...and` builds a computation that does
work, and `let%sub` saves the work to a variable so that other
computations can share the result.

A common mistake is to forget to save a computation; when the
computation gets used twice, the work must be done twice, rather than
getting shared between the two occurrences. The form `let%arr...and`
aims to eliminate such mistakes via the type-system. It extends the
`Let_syntax` module with an `arr` function that lifts a function to an
arrow.

```ocaml
val arr : 'a Value.t -> f:('a -> 'b) -> 'b Computation.t
```

The signature is the same as `map` except the result is
a `Computation.t`, and not a `Value.t`. The implementation of `arr` is
equivalent to `return (map x ~f)`. Roughly the only thing you can do
with a `Computation.t` is use `let%sub` to gain access to a `Value.t`
handle to the computation. Thus, using `arr` forces the user to use as
much sharing as possible. Of course, you can always duplicate work by
copy-pasting code, but it won't happen accidentally.

An unrelated distinction of `let%arr` is that it tracks
`Lexing.position`. That is, the signature of `arr` is actually the
following:

```ocaml
val arr : ?here:Lexing.position -> 'a Value.t -> f:('a -> 'b) -> 'b Computation.t
```

The `let%arr` form is as a testing ground for source location
tracking. It's likely that eventually all the other binding forms will
also track location.
