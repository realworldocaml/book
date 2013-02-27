Lists, Options and Patterns
================================================

_(yminsky:

The overall structure of this section seems a little off to me.  Here
are the headings, which I got from grepping out everything starting
with two hashes:

    ## Lists
    ## Pattern matching
    ## Options
    ## Pattern matching with `function`
    ### Using `when` and `as` in patterns
    ## List operations
    ### List length and random access
    #### Appending (concatenating) lists
    ### Map
    ### Folding
    ### Predicates `exists` and `for_all`
    ## Example: Implementing a set from a list
    ## Example: pretty-printing a table
    #### Computing the widths
    #### Rendering the rows
    ## List performance
    ### Tail-recursion
    ## Heterogenous values
    ## Options and NULL values

The structure is hard for me to follow, and the section/chapter
headings seem a little disorganized

)_

_(yminsky: I wonder whether the title should be "Lists, Tuples,
Options and Patterns"...)_

_(yminsky: Add some coverage of irrefutable patterns?)_

## Lists

As with any programming language, we need a way to represent _data_, things like
numbers, words, images, etc., and we need a way to define _aggregates_ that
bring together related values that represent some concept.

Lists are one of the most common ways to aggregate data in OCaml.  You can
construct a list of values by enclosing them in square brackets, separating
the elements with semicolons.  The list elements must all have the same type.

```ocaml
# let cities = ["Chicago"; "Paris"; "Tokyo"];;
val cities : string list = ["Chicago"; "Paris"; "Tokyo"]
# List.nth cities 1;;
- : string option = Some "Paris"
# List.nth cities 2;;
- : string option = Some "Tokyo"
```

The square bracket syntax is really just a shorthand.  There are really just two
ways to construct a list value.

* [] is the _empty_ list.

* If `x` is a value and `l` is a list, then the expression `x :: l` constructs a
  new list where the first element is `x`, and the rest is `l`.  The value
  corresponding to `x :: l` is commonly called a _cons_-cell (the term comes
  from the Lisp programming language, where "cons" is short for "constructor").

The bracket syntax `["Chicago"; "Paris"; "Tokyo"]` is just another way to write
a list with 3 cons-cells, `"Chicago" :: "Paris" :: "Tokyo" :: []`.  Each cell
has two parts: a value, and a pointer to the rest of the list.  The final
pointer refers to the special value `[]` representing the empty list.

```ocaml
# let cities2 = "Chicago" :: "Paris" :: "Tokyo" :: [];;
val cities2 : string list = ["Chicago"; "Paris"; "Tokyo"]
# List.hd cities2;;
- : string option = Some "Chicago"
# List.tl cities2;;
- : string list option = Some ["Paris"; "Tokyo"]
```

The following diagram shows the memory layout for the list `["Chicago"; "Paris";
"Tokyo"]`.  There are 3 cons-cells, each having two fields: a head, and a tail.
The head of the first cell points to the string `"Chicago"`, and the tail points
to the second cons cell.  The head of the second cons cell points to the string
`"Paris"`, and the tail points to the third cons cell.  The head of the third
(and last) cons cell points to the string `"Tokyo"` and the tail refers to the
empty list `[]`, which serves as a list terminator.

TODO: IMAGE figures/04-list-01.svg

## Pattern matching

Constructing a list is really only half the story -- it would be pretty useless
to construct lists unless we can also pull them apart.  For this we use _pattern
matching_.

For a list, there are two possible shapes: the empty list `[]` or a cons-cell `hd
:: tl`.  We can use a `match` expression to perform the pattern matching.  In the
case of a cons-cell, the variables `hd` and `tl` in the pattern are bound to the
corresponding values in the list when the match is performed.

For example, suppose we want to define a function to add 1 to each element of a
list.  We have to consider two cases, where the list is empty, or where it is a
cons-cell `hd :: tl`.  In the latter case, we add one to `hd`, and then recursively
compute over the tail of the list `tl`.

```ocaml
# let rec add1 l =
    match l with
    | [] -> []
    | hd :: tl -> (hd + 1) :: (add1 tl);;
val add1 : int list -> int list = <fun>
# add1 [5; 3; 7];;
- : int list = [6; 4; 8]
```

_(yminsky: Maybe resent this at first with more parens, ot make it
clear that nesting is in fact going on?  And then later, explain that
due to the associativity rules, the parens are unnecessary.   _i.e._,
`_ :: (_ :: (x :: _))` might be clearer. )_

Patterns are not limited to just one constructor.  They can be aribitrarily
nested.  For example, if we want to extract the third element of a list, we can
use a pattern `_ :: (_ :: (x :: _))`.  The underscore `_` is a special pattern that
matches (and ignores) anything.  The first two underscores match the first two
elements of the list, the `x` matches the third element, and the final
underscore matches the rest of the list.

```ocaml
# let third l =
    match l with
    | _ :: (_ :: (x :: _)) -> Some x
    | _ -> None;;
val third : 'a list -> 'a option = <fun>
# third ["A"; "B"; "C"; "D"];;
- : string option = Some "C"
# third ["A"; "B"];;
- : string option = None
```

We have been explicit about grouping by using pranetheses here, but in fact the cons operator `::` is right associative, so the parentheses are unnecessary.  It is equivalent to use the pattern `_ :: _ :: x :: _`.

```ocaml
# let third l =
    match l with
    | _ :: _ :: x :: _ -> Some x
    | _ -> None;;
val third : 'a list -> 'a option = <fun>
# third ["A"; "B"; "C"; "D"];;
- : string option = Some "C"
# third ["A"; "B"];;
- : string option = None
```

The pattern `_ :: _ :: x :: _` is not exhaustive, because it doesn't match lists
with fewer than three elements, so we have included a second "wildcard" pattern,
a single underscore `_`, that matches anything else.  We're using the `option`
type for the return value, where `Some x` means that the third element of
the list was `x`, and `None` means that the list had fewer than three elements.
We'll discuss options more in the next section.

Patterns are matched in left-to-right order (or top-to-bottom, in this case), so
the result is determined by the first pattern to match.  If we had listed the
patterns in opposite order, the wildcard pattern would match everything, and the
second pattern would be ignored.

```ocaml
# let broken_third l =
    match l with
    | _ -> None
    | _ :: _ :: x :: _ -> Some x;;
Characters 47-63:
    | _ :: _ :: x :: _ -> Some x;;
      ^^^^^^^^^^^^^^^^
Warning 11: this match case is unused.
val broken_third : 'a list -> 'a option = <fun>
# broken_third [17; 21; 7; 34];;
- : int option = None
```

Pattern ordering can be an issue, but a more important concern is that pure
wildcard patterns match anything, making it easier to forget important cases.
When a pattern matching is compiled, OCaml performs an exhaustiveness check,
printing a warning if it is not exhaustive, along with an example of a missing
pattern.

_(yminsky: I wonder if somewhere it would make sense to list in
bulleted form, what the three static checks you get with match
statements: inexhaustive matches, impossible cases, useless cases.
Seems like a nice trio to describe in one unit.  That said, not sure
how to fit it into the present flow.

That's a good point, I'll see what I can do.)_

```ocaml
# let inexhaustive_third l =
    match l with
    | _ :: _ :: x :: _ -> Some x;;
    Characters 24-68:
  ...match l with
     | _ :: _ :: x :: _ -> Some x..
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val inexhaustive_third : 'a list -> 'a option = <fun>
# inexhaustive_third [1; 2; 3; 4];;
- : int option = Some 3
# inexhaustive_third [1; 2];;
Exception: (Match_failure //toplevel// 14 3).
```

This exhaustiveness checking can be tremendously useful in ensuring that the
appropriate cases are all considered.  Generally speaking, it is usually better
to be as specific as possible when writing patterns.  This helps exhaustiveness
checking point out missing cases if they exist, and also means that pattern
ordering is not as important.

```ocaml
# let third l =
    match l with
    | [] | [_] | [_; _] -> None
    | _ :: _ :: x :: _ -> Some x;;
val third : 'a list -> 'a option = <fun>
```

## Options

Note that the `List` functions like `List.hd` (for "head" of the list),
`List.tl` (for "tail" of the list), and `List.nth` return values of `option`
type.  That's because, in some cases, there is no value to return.  For example,
the empty list has no head or tail.

```ocaml
- : string list option = Some ["Paris"; "Tokyo"]
# List.nth ["Chicago"; "Paris"; "Tokyo"] 2;;
- : string option = Some "Tokyo"
# List.nth ["Chicago"; "Paris"; "Tokyo"] 4;;
- : string option = None
```

A value of `option` type is optional"-- it is either `None`, meaning no value"
or `Some v` for some value `v`.  For example, `List.nth ["Chicago"; "Paris";
"Tokyo"] 2` is `Some "Tokyo"` because the third element of the list is `"Tokyo"`
(indexing starts from 0), but `List.nth ["Chicago"; "Paris"; "Tokyo"] 4` is
`None` because there is no fifth element.

The type definition for `option` has the following form.

```ocaml
type 'a option =
  | Some of 'a
  | None
```

For example, let's implement a function `last` that returns the last element of
a list, or `None` if the list is empty.  There are three cases: the list can be empty, so we return `None`; it can have one element `x`, and we return `Some x`; or it can have more than one element, in which case we call `last` recursively.

```ocaml
# let rec last l =
    match l with
    | [] -> None
    | [x] -> Some x
    | _ :: tl -> last tl;;
val last : 'a list -> 'a option = <fun>
# last [];;
- : 'a option = None
# last [5; 7; 9];;
- : int option = Some 9
```

As with lists, the way to take apart an `option` value is to use pattern
matching, with patterns for the two cases.  The following function
`option_value` performs a case analysis on an option value `opt`, returning `x`
if `opt` is `Some x`, or `default` if `opt` is `None`.

```ocaml
# let option_value ~default opt =
    match opt with
    | Some x -> x
    | None -> default;;
val option_value : default:'a -> 'a option -> 'a = <fun>
# option_value ~default:10 (Some 17);;
- : int = 17
# option_value ~default:10 None;;
- : int = 10
```

_(yminsky: I wonder if we should largely skip the monad thing in this
chapter, and instead put in a forward reference to the error handling
chapter, which discusses these idioms in some detail.

I have mixed opinion about it.  Yes, it does belong in error handling, but OTOH
it is important to answer why these functions don't compose with function
composition.  That's different from the OCaml standard library, and it's
different from Lisp, too, so it seems quite reasonable to ask why anyone would
do it this way.  The answer is that we just define a different composition
operator.)_

One annoying thing about the use of options is that functions do not
compose directly.  For example, if we wanted to compute the third
element of a list using the `List.hd` and `List.tl` functions, we
might try to compose them like this.

```ocaml
# let third l = List.hd (List.tl (List.tl l));;
Characters 32-41:
  let third l = List.hd (List.tl (List.tl l));;
                                  ^^^^^^^^^
Error: This expression has type 'a list option
       but an expression was expected of type 'b list
```

Unfortunately, this doesn't work because the functions return options,
not lists.  We can use pattern matching to perform the composition,
but the code is pretty tedious.

```ocaml
# let tedious_third l1 =
    match List.tl l1 with
    | None -> None
    | Some l2 ->
      match List.tl l2 with
      | None -> None
      | Some l3 ->
        List.hd l3;;
val tedious_third : 'a list -> 'a option = <fun>
# tedious_third [7; 21; 12; 19];;
- : int option = Some 12
# tedious_third [1; 2];;
- : int option = None
```

One solution for abbreviating the code is to define a composition operator `x
>>= f` that takes an option value `x`, performs the pattern match, and passes
the value to function `f` if there is one.

```ocaml
# let (>>=) x f =
    match x with
    | None -> None
    | Some y -> f y;;
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option = <fun>
# let third l = Some l >>= List.tl >>= List.tl >>= List.hd;;
val third : 'a list -> 'a option = <fun>
# third [7; 21; 12; 19];;
- : int option = Some 12
# third [7; 21];;
- : int option = None
```

The operator `>>=` has infix syntax, and the expression `Some l >>= List.tl >>=
List.tl >>= List.hd` should be read left-to-right: pass `Some l` to the
`List.tl` function, then to another `List.tl`, then to `List.hd`.  Since `>>=`
is an infix operator, the function definition uses parenthesis `(>>=)` to define
the function in prefix form.

All we have done here is to hoist the pattern matching into a separate function
`>>=`, but the result has a convenient left-to-right sequential reading.  This
is a common pattern in Core, captured by the `Monad.S` module signature.  In
fact, The `Core.Std.Option` module already implements the `Monad.S` signature.
A more conventional way to write the function would be to use function
`Option.(>>=)` directly.

```ocaml
# let third l =
    let (>>=) = Option.(>>=) in
    Some l >>= List.tl >>= List.tl >>= List.hd;;
val third : 'a list -> 'a option = <fun>
```

For a more complete discussion of programming with monads, see the error
handling chapter.

## Pattern matching with `function`

Pattern matching can be used any place where a regular binding is
performed.  For a trivial example, we can use an underscore to ignore
a value.  This is often useful when defining functions that ignore one
or more of their arguments.

```ocaml
# let f _ = 5;;
val f : 'a -> int = <fun>
# f 3;;
- : int = 5
# f "three";;
- : int = 5
```

Tuple patterns can be used to break apart of components of a tuple.  For
example, we can bind the parts of a triple to separate variables, or define
projection functions that can be used to return components of the tuple.

```ocaml
# let x, y, z = 1, 2, 3;;
val x : int = 1
val y : int = 2
val z : int = 3
# let first_of_three (x, _, _) = x;;
val first_of_three : 'a * 'b * 'c -> 'a = <fun>
# first_of_three ("a", "b", "c");;
- : string = "a"
```

_(yminsky: I think you mean: "because a single pattern won't be
exhaustive", or something to that effect.  There's some rough coverage
of this issue in the Records chapter in the subsection on "Patterns
and exhaustiveness".  The coverage here and there should probably be
harmonized)_

Lists are a little different when using normal patterns, because in general a
single pattern will not be exhaustive.  The compiler requires that patterns
match all possible values of a type, regardless of the actual value being
matched.

_(yminsky: `h` -> `hd`, `t` -> `tl`)_

```ocaml
# let hd :: tl = [1; 2];;
Characters 4-10:
  let hd :: tl = [1; 2];;
      ^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val hd : int = 1
val tl : int list = [2]
```

_(yminsky: I'm not sure `function` deserves material coverage here,
since it was covered in the previous chapter)_

When matching values with multiple different cases, like with lists or variants,
you will usually want to use a `match` to perform a case analysis.  For
functions, there is an alternative form `function p1 -> e1 | p2 -> e2 | ... | pN
-> eN`, where the cases are just like a `match`.  The following functions
`head1` and `head2` are equivalent; they return the first element of the list if
it exists.

```ocaml
# let head1 l =
    match l with
    | [] -> None
    | hd :: _ -> Some hd;;
val head1 : 'a list -> 'a option = <fun>
# head1 [1; 2; 3];;
- : int option = Some 1
# let head2 = function
    | [] -> None
    | hd :: _ -> Some hd;;
val head2 : 'a list -> 'a option = <fun>
# head2 [1; 2; 3];;
- : int option = Some 1
```

### Using `when` and `as` in patterns

_(yminsky: I wonder if or-patterns deserve an explicit mention.  The
fact that you can nest disjunctions anywhere nested inside a pattern
seems notable.)_

_(yminsky: Do you mean "the example in the introduction"?  Maybe
better to reference the explicit chapter or section using an xref.)_

Let's return to the example in the guided tour, where we defined a `destutter`
function that removes adjacent duplicate elements in a list.  Here is the code
we presented.

```ocaml
# let rec destutter list =
    match list with
    | [] -> []
    | [hd] -> [hd]
    | hd1 :: (hd2 :: tl) ->
      if hd1 = hd2 then destutter (hd2 :: tl)
      else hd1 :: destutter (hd2 :: tl)
  ;;
val destutter : 'a list -> 'a list = <fun>
# destutter ["hey";"hey";"hey";"man!"];;
- : string list = ["hey"; "man!"]
```

There are several ways we can make this code more concise and efficient.  The
first two cases can be combined into a single case where we just return `list`
without reconstructing it.  In the final case, we should also avoid
reconstructing the list `(hd2 :: tl)` when performing the recursive call.  Here
is an updated, simplified, version of the code,.

```ocaml
# let rec destutter = function
   | ([] | [_]) as l -> l
   | hd1 :: ((hd2 :: _) as tl) ->
     if hd1 = hd2 then destutter tl
     else  i1 :: destutter tl;;
val destutter : 'a list -> 'a list = <fun>
# destutter [1; 3; 3; 3; 2];;
- : int list = [1; 3; 2]
```

In this revised code, we're using `as` patterns, which have the form "_pattern_
`as` _variable_".  If the _pattern_ matches, the value is also bound to the
variable.  For the first case, the pattern `([] | [_]) as l` matches lists that
are empty or have one element, and the list is bound to the variable `l`.

The second pattern is somewhat more interesting, `hd1 :: ((hd2 :: _) as tl)` binds
`tl` to the tail of the list.  This would be harder (or at least more verbose) if
we did not have `as` patterns.

OCaml also supports _guarded_ patterns using _pattern_ `when`
_expression_, where the _expression_ is a predicate.  The predicate is
allowed to use variables bound by the pattern, and the pattern matches
only of _(yminsky: "only if", I think)_ the expression evaluates to
`true`.  Another way to write the `uniq` function is to split the
equality case using a `when` pattern.

```ocaml
# let rec broken_destutter = function
   | ([] | [_]) as l -> l
   | hd1 :: ((hd2 :: _) as tl) when hd1 = hd2 ->
     broken_destutter tl
   | hd1 :: ((hd2 :: _) as tl) when hd1 <> hd2 ->
     hd1 :: broken_destutter tl;;
...
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
_::_::_
(However, some guarded clause may match this value.)
val broken_destutter : 'a list -> 'a list = <fun>
```

Unfortunately, there is a problem here, because the compiler complains about an
inexhaustive match.  The problem is that guarded patterns (patterns with `when`
clauses) are not included in the exhaustiveness analysis because, in general,
the compiler can't infer when the guards consider all values.  Of course, in
this particular case, we can remove the second guard and rely on pattern
ordering, but the result is not as obvious as the original form without guards.
You should generally avoid `when` clauses except when they really make things
simpler, typically by reducing the number of cases in your match.  Othwerise,
they're usually more trouble than their worth.

```ocaml
# let rec subtle_destutter = function
    | ([] | [_]) as l -> l
    | hd1 :: ((hd2 :: _) as tl) when hd1 = hd2 -> subtle_destutter tl
    | hd1 :: tl -> hd1 :: subtle_uniq tl;;
val subtle_destutter : 'a list -> 'a list = <fun>
# subtle_destutter [1; 3; 3; 3; 4];;
- : int list = [1; 3; 4]
```

## List operations

Abstractly, a list is an ordered collection of elements, where access is
sequential, and the list length is not fixed.  There are other kinds of standard
collections in OCaml, including sets, implemented by the `Set` module;
dictionaries, implemented by the `Map` module; arrays, implemented by the
`Array` module; and many others.  The different collections have different
properties, for example sets support efficient membership testing, but do not
preserve ordering, and arrays support efficient random access but have fixed
size.

However, all collections support some standard operations for computing over the
elements in the collection.  Let's look at some of the operations for lists,
including some possible implementations.

### List length and random access

Lists do not have a constant-time operation that returns the number of elements
in the list.  The function `List.length` calculates the length by iterating
through the elements of the list.  The `List.nth` function is similar, used for
random access into the list.

```ocaml
# List.length ["a"; "b"; "c"];;
- : int = 3
# List.nth ["a"; "b"; "c"; "d"] 2;;
- : string option = Some "c"
```

Since lists support only sequential acccess, the implementations of these
functions iterate through the elements of the list one at a time.

```ocaml
# let my_length = function
    | [] -> 0
    | _ :: tl -> my_length tl + 1;;
val my_length : 'a list -> int = <fun>
# my_length ["a"; "b"; "c"; "d"];;
- : int = 4
```

The `nth` function is similar.  The computation iterates through the list
sequentially, stopping when the index reaches zero.

```ocaml
# let rec my_nth l i =
    match l with
    | [] -> None
    | h :: t -> if i = 0 then Some h else my_nth t (i - 1);;
val my_nth : 'a list -> int -> 'a option = <fun>
# my_nth ["a"; "b"; "c"; "d"] 2;;
- : string option = Some "c"
```
#### Appending (concatenating) lists

Another basic list operation is to concatenate two lists, forming a new list
from the results.  This can be written explicitly using the `List.append`
function, or with the infix operator `@`; the two ways are equivalent.

```ocaml
# List.append [1; 2; 3] [12; 13];;
- : int list = [1; 2; 3; 12; 13]
# [1; 2; 3] @ [21; 32];;
- : int list = [1; 2; 3; 21; 32]
```

The implementation of the `append` function requires iterating through the first
list to find the last element, then replacing it with the second list.

```ocaml
# let rec my_append l1 l2 =
   match l1 with
    | [] -> l2
    | h :: t -> h :: my_append t l2;;
val my_append : 'a list -> 'a list -> 'a list = <fun>
# my_append ["a"; "b"] ["x"; "y"; "z"];;
- : string list = ["a"; "b"; "x"; "y"; "z"]
```

The `Core.Std.List` module contains a somewhat more efficient tail-recursive
implementation of the append function (tail-recursion is discussed later in this
chapter).  However, one thing to note is that the associativity of append has an
important effect on performance.  If we append three or more lists, it is most
efficient to associate to the right.  The expressions `List.append l1
(List.append l2 l3)` and `List.append (List.append l1 l2) l3` produce equal
results, but the first is more efficient in general.  The `@` operator is right
associative, so lists will be appended in the correct order by default;
`l1 @ l2 @ l3` is equivalent to `l1 @ (l2 @ l3)`.

### Map

The `List.map` function creates a new list by mapping a function over
the elements of one list, forming a new list from the results.  Given
a function `f`, and a list `l = [a1; a2; ...; aN]`, the function
`List.map ~f l` returns the new list `[f a1; f a2; ...; f aN]`.

```ocaml
# List.map;;
- : 'a list -> f:('a -> 'b) -> 'b list = <fun>
# List.map ~f:(fun i -> string_of_int (i + 1)) [10; 21; 12];;
- : string list = ["11"; "22"; "13"]
```

A simple way to compute the map is simply to use pattern matching to iterate
over the elements of the list (the `Core.Std.List` module uses a more efficient
tail-recursive implementation, discussed later in this chapter).

```ocaml
# let rec my_map ~f = function
   | [] -> []
   | h :: t -> f h :: map ~f t;;
val my_map : f:('a -> 'b) -> 'a list -> 'b list = <fun>
# my_map ~f:(fun i -> string_of_int (i * 10)) [1; 2; 3];;
- : string list = ["10"; "20"; "30"]
```

_(yminsky: If we're going to go over useful list operations, I would
put my vote in for, in order: `filter` and `filter_map`, `zip` and
`unzip`, `partition_tf`, `partition_map`.  `filter_map` in particular
is useful and not as well known as it deserves to be.)_


### Folding

_(yminsky: In Core one typically just calls it `List.fold`.  If you
read the API, `fold_left` is only for fairly special cases.

jyh: Actually, if you read the List API, you will find that neither function
is documented.)_

The `List.fold` function has a form similar to `map`, but it composes a
function over the elements of the list.  The expression `List.fold ~init ~f
[a1; a2; a3]` applies `f` to each element of the list, composing the results as
`f (f (f init a1) a2) a3`.

For example, if we want to sum up the elements in a list, we can fold the `+`
function over the elements.

```ocaml
# List.fold;;
- : 'a list -> init:'b -> f:('b -> 'a -> 'b) -> 'b = <fun>
# List.fold ~init:0 ~f:(+) [14; 21; 1];;
- : int = 36
```

The implementation is straightforward, applying the function `f` to each element
of the list from left to right, accumulating the results.

```ocaml
# let rec my_fold ~init ~f = function
    | [] -> init
    | h :: t -> my_fold ~init:(f init h) ~f t;;
val my_fold : init:'a -> f:('a -> 'b -> 'a) -> 'b list -> 'a = <fun>
# my_fold ~init:["d"] ~f:(fun t h -> h :: t) ["a"; "b"; "c"];;
- : string list = ["c"; "b"; "a"; "d"]
```

Most containers (including lists, sets, arrays, and others) have a folding
operation.  In fact, folding is the most general operation of the operations --
we can implement the functions like `length`, `nth`, `map`, `filter_map`,_etc._
in terms of fold.  This generality can also be confusing.  There are times when
you need the generality, but you should prefer other, less powerful idioms like
`map`, `filter_map` etc, where they apply.

### Predicates `exists` and `for_all`

Given a predicate function `f` on the element of a list, the function
`List.exists` tests whether `f` is true on any element of the list, and the
function `List.for_all` tests whether `f` is true on all elements of the list.

```ocaml
# List.exists ~f:(fun i -> i > 5) [1; 7; 3];;
- : bool = true
# List.for_all ~f:(fun i -> i > 5) [1; 7; 3];;
- : bool = false
```

## Example: Implementing a set from a list

To pull all this together, let's implement a set data structure using a list.
This is not necessarily an efficient implementation (unless the set is small),
because testing for set membership will take time linear in the size of the set.
However, it gives a pretty good illustration of list computations.  First, let's
give the signature of the module we are going to implement.

```ocaml
  type 'a t

  val empty : 'a t
  val of_list : 'a list -> 'a t
  val insert : 'a -> 'a t -> 'a t
  val member : 'a -> 'a t -> bool
  val union : 'a t -> 'a t -> 'a t
  val isect : 'a t -> 'a t -> 'a t
```

For the implementation, we'll order the elements in the set to be in ascending
order, which will improve performance, especially for the intersection `isect`
function.  Given this, the implementation of many of the funcitons is
straightforward.  The empty set is implemented as the empty list.  For the
`of_list` function, we sort the set with the `List.sort` function.  For sorting,
we use the builtin comparison function `Pervasives.compare`, which can compare
most OCaml values except functions and externally allocated values like C
values.  The `union` function is similar, we can just use the `List.merge` function.

```ocaml
  let empty = []
  let of_list l = List.sort ~cmp:Pervasives.compare l
  let member x l = List.exists ~f:((=) x) l
  let union = List.merge ~cmp:Pervasives.compare
```

We'll implement the `insert` function directly, advancing through the list until
the insertion point is found (a version of bubble sort).

```ocaml
  let rec insert x = function
    | [] -> [x]
    | (h :: t) as l ->
      if h < x then
        h :: insert x t
      else if h > x then
        x :: l
      else (* x = h *)
        l
```

For the intersection, the implementation is like a merging of the two lists, but
we keep only the elements that are in both lists.  For the pattern matching, we
match on both lists simultaneously by matching against a list pair.

```ocaml
  let rec isect l1 l2 =
    match l1, l2 with
    | i1 :: t1, i2 :: t2 ->
      if i1 < i2 then
        isect t1 l2
      else if i1 > i2 then
        isect l1 t2
      else (* i1 = i2 *)
        i1 :: isect t1 t2
    | [], _
    | _, [] -> []
```

There are two important cases.  The pattern ([], _ | _, []) matches the case
where one of the lists is empty, and the pattern `i1 :: t1, i2 :: t2` matches
the case where both lists are nonempty.  We keep the first element if `i1` and
`i2` are the same; otherwise, we discard the smaller value and continue
recursively.

## Example: pretty-printing a table

One common programming task is displaying tabular data.  In this
example, we will go over the design of a simple library to do just that.

_(yminsky: This is the first appearance of an mli file.  If we're
going to introduce it here, we need to do a little more explanation.

jyh: I agree, let's discuss.

yminsky: My inclination is to think that we can just get away without
using an mli here; just do this stuff directly in the toplevel.
)_

We'll start with the interface.  The code will go in a new
module called `Text_table` whose `.mli` contains just the following
function:

```ocaml
(* [render headers rows] returns a string containing a formatted
   text table, using Unix-style newlines as separators *)
val render
   :  string list         (* header *)
   -> string list list    (* data *)
   -> string
```

If you invoke `render` as follows:

```ocaml
let () =
  print_string (Text_table.render
     ["language";"architect";"first release"]
     [ ["Lisp" ;"John McCarthy" ;"1958"] ;
       ["C"    ;"Dennis Ritchie";"1969"] ;
       ["ML"   ;"Robin Milner"  ;"1973"] ;
       ["OCaml";"Xavier Leroy"  ;"1996"] ;
     ])
```

you'll get the following output:

```
| language | architect      | first release |
|----------+----------------+---------------|
| Lisp     | John McCarthy  | 1958          |
| C        | Dennis Ritchie | 1969          |
| ML       | Robin Milner   | 1973          |
| OCaml    | Xavier Leroy   | 1996          |
```

Now that we know what `render` is supposed to do, let's dive into the
implementation.

#### Computing the widths

To render the rows of the table, we'll first need the width of the
widest entry in each column.  The following function does just that.

```ocaml
let max_widths header rows =
  let to_lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(to_lengths header)
    ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (to_lengths row))
```

In the above we define a helper function, `to_lengths` which uses
`List.map` and `String.length` to convert a list of strings to a list
of string lengths.  Then, starting with the lengths of the headers, we
use `List.fold` to join in the lengths of the elements of each row by
`max`'ing them together element-wise.

_(yminsky: Should we explain what map2_exn does a little better?)_

Note that this code will throw an exception if any of the rows has a
different number of entries than the header.  In particular,
`List.map2_exn` throws an exception when its arguments have mismatched
lengths.

#### Rendering the rows

Now we need to write the code to render a single row.  There are
really two different kinds of rows that need to be rendered; an
ordinary row:

```
| Lisp     | John McCarthy  | 1962          |
```

and a separator row:

```
|----------+----------------+---------------|
```

Let's start with the separator row, which we can generate as follows:

```ocaml
let render_separator widths =
  let pieces = List.map widths
    ~f:(fun w -> String.make (w + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"
```

We need the extra two-characters for each entry to account for the one
character of padding on each side of a string in the table.

<note>
<title>Performance of `String.concat` and `^`</title>

In the above, we're using two different ways of concatenating
strings, `String.concat`, which operates on lists of strings, and
`^`, which is a pairwise operator.  You should avoid `^` for joining
long numbers of strings, since, it allocates a new string every time
it runs.  Thus, the following code:

```ocaml
let s = "." ^ "."  ^ "."  ^ "."  ^ "."  ^ "."  ^ "."
```

will allocate a string of length 2, 3, 4, 5, 6 and 7, whereas this
code:

```ocaml
let s = String.concat [".";".";".";".";".";".";"."]
```

allocates one string of size 7, as well as a list of length 7.  At
these small sizes, the differences don't amount to much, but for
assembling of large strings, it can be a serious performance issue.

</note>

We can write a very similar piece of code for rendering the data in
an ordinary row.

```ocaml
let pad s length =
  if String.length s >= length then s
  else s ^ String.make (length - String.length s) ' '

let render_row row widths =
  let pieces = List.map2 row widths
    ~f:(fun s width -> " " ^ pad s width ^ " ")
  in
  "|" ^ String.concat ~sep:"|" pieces ^ "|"
```

You might note that `render_row` and `render_separator` share a bit of
structure.  We can improve the code a bit by factoring that repeated
structure out:

```ocaml
let decorate_row ~sep row = "|" ^ String.concat ~sep row ^ "|"

let render_row widths row =
  decorate_row ~sep:"|"
    (List.map2_exn row widths ~f:(fun s w -> " " ^ pad s w ^ " "))

let render_separator widths =
  decorate_row ~sep:"+"
    (List.map widths ~f:(fun width -> String.make (width + 2) '-'))
```

And now we can write the function for rendering a full table.

```ocaml
let render header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row widths header
     :: render_separator widths
     :: List.map rows ~f:(fun row -> render_row widths row)
    )
```

## Tail recursion

_(yminsky: I think it's important not to go too negative on the
performance of lists.  Lists are after all quite efficient for lots of
applications, as long as you're thoughtful in how you use them, and
you don't use them in cases where the space overhead is material.  But
transformations of large lists can actually be quite fast, in part
because allocation in OCaml is so very fast.  Indeed, I think
programmers from other languages often overestimate the cost of using
lists, to the detriment of their code.)_

_(yminsky: Indeed, maybe this section should explicitly just be about
tail recursion, rather than being about list performance writ large?
It doesn't really talk about that in any detail, and it's kind of too
early in the book to start counting the number of words in a cons
cell...)_

Lists are ubiquitous in OCaml programs.  They are easy to use and
reasonably efficient, but large lists can sometimes have
performance problems.  The issue is that lists are formed
from separately allocated cons-cells.  This has space overhead because
each value in the list is paired with a pointer to the rest of the
list.  The separate allocation also reduces locality, so it can result
in poor cache behavior.

Perhaps more important than those concerns is that naive list
traversal takes time linear in the length of the list.  For example,
the following `length` function takes linear time to count the number
of elements in the list.

```ocaml
let rec length = function
  | [] -> 0
  | _ :: tl -> length tl + 1;;
```

In fact, this implementation of the function `length` has a worse problem.  When
the function runs, each recursive call is active at the same time as the caller.
The runtime needs to allocate an activation record to store the state of each
active call, so this function also takes linear space.  The OCaml runtime stores
activation records (also called _stack frames_) in the runtime stack, so for
large lists this can result in stack overflow.

### Tail-recursion

We can't do anything about `length` taking linear time --
singly-linked lists of this kind don't have an efficient `length`
operation.  However, we can address the space problem using _tail
recursion_.

_(yminsky: Probably some explanation is required for the word "stack
frame", which I think we can't assume people will just know.

jyh: I changed the name, see what you think.  I don't think we want to go into
too much detail here.)_

Tail recursion occurs whenever the result of the recursive call is returned
immediately by the calling function.  In this case, the compiler optimizes the
call by skipping the allocation of a new activation record, instead reusing the
activation record of the caller and branching directly to the called procedure.

In the definition of `length` above, the expression containing the
recursive call `(length t) + 1` is _not_ tail recursive because 1 is
added to the result.  However, it is easy to transform the function so
that it is properly tail recursive.

```ocaml
let length l =
  let rec tail_recursive_length len = function
    | [] -> len
    | _ :: tl -> tail_recursive_length (len + 1) tl
  in tail_recursive_length 0 l;;
```

To preserve the type of the `length` function, we hide the
tail-recursive implementation by nesting it.  The tail-recursive
implementation performs the addition _before_ the recursive call,
instead of afterwards.  Since the result of the recursive call is
returned without modification, the compiler branches directly to the
called procedure rather than allocating a new stack frame.

In other cases, it can be more problematic to use tail-recursion.  For
example, consider the non tail-recursive implemenation of `map`
function, listed above.  The code is simple, but not efficient.

```ocaml
let rec map ~f = function
 | [] -> []
 | h :: t -> f h :: map ~f t;;
```

If we use the same trick as we used for the `length` method, we need
to accumulate the result _before_ the recursive call, but this
collects the result in reverse order.  One way to address it is to
construct the reversed result, then explicitly correct it before
returning.

```ocaml
let rev l =
  let rec tail_recursive_rev result = function
   | [] -> result
   | h :: t -> tail_recursive_rev (h :: result) t
  in tail_recursive_rev [] l;;

let rev_map l ~f =
  let rec rmap accu = function
   | [] -> accu
   | h :: t -> rmap (f h :: accu) l
  in rmap [] l;;

let map l ~f = rev (rev_map l ~f);;
```

The functions `tail_recursive_rev` and `rev_map` are both
tail-recursive, which means that the function `map` is tail-recursive also.
The cost of doing so is that we construct an intermediate
reversed list that is immediately discarded.  One way to think of it
is that instead of allocating a linear number of stack frames, we
allocate a linear number of cons-cells.

Allocation of short-lived data in OCaml is quite cheap, so the
intermediate list is not very expensive.  The performance of the two
implementations is not significantly different, with one exception:
the tail-recursive implementation will not cause a stack overflow for
large lists, while the simple non-tail-recursive implementation will
have problems with large lists.

## Heterogenous values

_(yminsky: I'm not sure that "heterogenous values" is really the right
name for this section.  Indeed, I'm not totally sure how to think
about this as a general lesson about lists.  Indeed, it mostly seems
like it's more about good use of the type system and parametric
polymorphism than it is about lists per se.

I think I would just think of this as an extension of the example, and
not a particularized lesson about lists.)_

Lists are fairly general, but there are several reasons why you might
not want to use them.

* Large lists often have poor performance.
* The list length is variable, not fixed.
* The data in a list must have the same type.

In the tabulaton example above, the `List` is not a good choice for
each entry in the table.  Now, let's think about how you might
actually use this interface in practice.  Usually, when you have data
to render in a table, the data entries are described more precisely by
a record.  So, imagine that you start off with a record type for
representing information about a given programming language:

```ocaml
type style =
    Object_oriented | Functional | Imperative | Logic

type prog_lang = { name: string;
                   architect: string;
                   year_released: int;
                   style: style list;
                 }
```

If we then wanted to render a table from a list of languages, we might
write something like this:

```ocaml
let print_langs langs =
   let headers = ["name";"architect";"year released"] in
   let to_row lang =
     [lang.name; lang.architect; Int.to_string lang.year_released ]
   in
   print_string (Text_table.render headers (List.map ~f:to_row langs))
```

This is OK, but as you consider more complicated tables with more
columns, it becomes easier to make the mistake of having a mismatch in
between `headers` and `to_row`.  Also, adding, removing and reordering
columns becomes awkward, because changes need to be made in two
places.

We can improve the table API by adding a type that is a first-class
representative for a column.  We'd add the following to the interface
of `Text_table`:

```ocaml
(** An ['a column] is a specification of a column for rending a table
    of values of type ['a] *)
type 'a column

(** [column header to_entry] returns a new column given a header and a
    function for extracting the text entry from the data associated
    with a row *)
val column : string -> ('a -> string) -> 'a column

(** [column_render columns rows] Renders a table with the specified
    columns and rows *)
val column_render :
  'a column list -> 'a list -> string
```

Thus, the `column` functions creates a `column` from a header string
and a function for extracting the text for that column associated with
a given row.  Implementing this interface is quite simple:

```ocaml
type 'a column = string * ('a -> string)
let column header to_string = (header,to_string)

let column_render columns rows =
  let header = List.map columns ~f:fst in
  let rows = List.map rows ~f:(fun row ->
    List.map columns ~f:(fun (_,to_string) -> to_string row))
  in
  render header rows
```

And we can rewrite `print_langs` to use this new interface as follows.

```ocaml
let columns =
  [ Text_table.column "Name"      (fun x -> x.name);
    Text_table.column "Architect" (fun x -> x.architect);
    Text_table.column "Year Released"
       (fun x -> Int.to_string x.year_released);
  ]

let print_langs langs =
  print_string (Text_table.column_render columns langs)
```

The code is a bit longer, but it's also less error prone.  In
particular, several errors that might be made by the user are now
ruled out by the type system.  For example, it's no longer possible
for the length of the header and the lengths of the rows to be
mismatched.

The simple column-based interface described here is also a good
starting for building a richer API.  You could for example build
specialized columns with different formatting and alignment rules,
which is easier to do with this interface than with the original one
based on passing in lists-of-lists.

_(yminsky: Isn't this awkwardly placed?  It feels like this should go
way earlier in the chapter.  Indeed, some of the material below shows
up earlier as it is, including the explicit definition of the option type.)_

## Options and NULL values

OCaml has no "NULL" or "nil" values.  Programmers coming from other
languages are often surprised and annoyed by this -- it seems really
convenient to have a special `NULL` value that represents concepts
like "end of list" or "leaf node in a tree."  The possible benefit is
that _every_ pointer type has a extra NULL value; the problem is that
using the NULL value as if it were a real value has weak or undefined
semantics.

How do we get similar semantics in OCaml?  The ubiquitous technique is
to use the `option` type, which has the following definition.

```ocaml
type 'a option = None | Some of 'a;;
```

That is, a value of type `'a option` is either `None`, which means "no
value;" or it is `Some v`, which represents a value `v`.  There is
nothing special about the `option` type -- it is a variant type just
like any other.  What it means is that checking for `None` is
_explicit_, it is not possible to use `None` in a place where `Some x`
is expected.

In the most direct form, we can use an `option` wherever some value is
"optional," with the usual meaning.  For example, if the architect of
a programming language is not always known, we could use a special
string like `"unknown"` to represent the architect's name, but we
might accidentally confuse it with the name of a person.  The more
explicit alternative is to use an option.

```ocaml
type prog_lang = { name: string;
                   architect: string option;
                   year_released: int;
                   style: style list;
                 }

let x86 = { name = "x86 assembly";
            architect = None;
            year_released = 1980;
            style = [Imperative]
          };;
```

We can also represent data structures with NULL-pointers using the `option`
type.  For example, if we're defining a type of binary trees, one choice is to
use `option` for the child node references.  In a binary search tree, each node
in the tree is labeled with a value and it has up to two children.  The nodes in
the tree follow _infix_ order, meaning that the label of the left child is
smaller than the label of its parent, and the label of the right child is larger
than the label of the parent.

_(yminsky: Do we really want a binary tree example here?  It feels a
little overkill for so early in the book, and feels off-topic from the
lists/options/pattern-matching direction of the chapter.)_

_(yminsky: I think we should use label punning below, to make it a bit
easier to read.  We already introduced label punning in the getting
started section.)_

```ocaml
type 'a node = { label : 'a; left : 'a binary_tree; right : 'a binary_tree }
and 'a binary_tree = 'a node option;;

let new_binary_tree () : 'a binary_tree = None;;

let rec insert x = function
 | Some { label = label; left = left; right = right } as tree ->
   if x < label then
     Some { label = label; left = insert x left; right = right }
   else if x > label then
     Some { label = label; left = left; right = insert x right }
   else
     tree
 | None -> Some { label = x; left = None; right = None };;
```

This representation is perfectly adequate, but many OCaml programmers
would prefer a representation where the `option` is "hoisted" to the
`node` type, meaning that we have two kinds of nodes.  In this case,
the code is somewhat more succinct.  In the end, of course, the two
versions are isomorphic.

```ocaml
type 'a binary_tree =
 | Leaf
 | Interior of 'a * 'a binary_tree * 'a binary_tree;;

let new_binary_tree () : 'a binary_tree = Leaf;;

let rec insert x = function
 | Interior (label, left, right) as tree ->
   if x < label then Interior (label, insert x left, right)
   else if x > label then Interior (label, left, insert x right)
   else tree
 | Leaf -> Interior (x, Leaf, Leaf);;
```
