# Lists and Patterns

This chapter will focus on two common elements of programming in
OCaml: lists and pattern matching.  Both of these were discussed in
[xref](#a-guided-tour), but we'll go into more depth here, presenting
the two topics together, and using one to help illustrate the other.

## List Basics

We'll start with lists.  An OCaml list is an immutable, finite
sequence of elements of the same type.  As we've seen, OCaml lists can
be generated using a bracket-and-semicolon notation:

```ocaml
# [1;2;3];;
- : int list = [1; 2; 3]
```

And they can also be generated using the equivalent `::` notation.

```ocaml
# 1 :: (2 :: (3 :: [])) ;;
- : int list = [1; 2; 3]
# 1 :: 2 :: 3 :: [] ;;
- : int list = [1; 2; 3]
```

Note that `[]`, the empty list, is used as a terminator for the list.

The `::` operator is considered the more fundamental of the two, and
it conveys something important about the nature of lists, which is
that they are implemented as singly-linked lists.  The following is a
rough graphical representation of how the list `1 :: 2 :: 3 :: []` is
laid out as a data-structure.


```
+---+---+   +---+---+   +---+---+
| 1 | *---->| 2 | *---->| 3 | *---->||
+---+---+   +---+---+   +---+---+
```

In the above, the final arrow (from the box containing `3`)
points to the empty list.

Every time you see the `::` operator used to extend a list, you should
think of that as allocating a new list element that contains two
things: a reference to the data in that list element, and a reference
to the remainder of the list.

## Using patterns to extract data from a list

We can read data out of a list using the match statement.  Here's a
simple example of a recursive function that computes the sum of all
elements of a list.

```ocaml
# let rec sum l =
    match l with
    | [] -> 0
    | hd :: tl -> hd + sum tl
  ;;
val sum : int list -> int = <fun>
# sum [1;2;3];;
- : int = 6
# sum [];;
- : int = 0
```

This code follows the convention of using `hd` to represent the first
element (or head) of the list, and `tl` to represent the remainder (or
tail).

The match statement in `sum` is really doing two things: first, it's
acting as a case-analysis tool, breaking down the possibilities into a
concrete list of cases indexed by patterns.  Second, it lets you name
sub-structures within the data-structure being matched.  In this case,
the variables `hd` and `tl` are bound by the pattern that defines the
first case of the match statment, and are then used in the expressions
to the right of the arrow.

The fact that match statements can be used to bind new variables can
be a little surprising.  To see how, imagine we wanted to write a
function that filtered out from a list all elements equal to a
particular value.  You might be tempted to write that code as follows.

```ocaml
# let rec drop_value l to_drop =
    match l with
    | [] -> []
    | to_drop :: tl -> drop_value tl to_drop
    | hd :: tl -> hd :: drop_value tl to_drop
  ;;
```

But when we type this in, the compiler will immediately warn us that
something is wrong, and if we try to use the resulting function, we'll
see it doesn't do the right thing.

```
Characters 114-122:
      | hd :: tl -> hd :: drop_value tl to_drop
        ^^^^^^^^
Warning 11: this match case is unused.
val drop_value : 'a list -> 'a -> 'a list = <fun>
# drop_value [1;2;3] 2;;
- : int list = []
```

So, what's going on?

The key observation is that the appearance of `to_drop` in the second
case doesn't imply a check that the first element is equal to the
value `to_drop` passed in as an argument to the function.  Instead, it
just causes a new variable `to_drop` to be bound to whatever happens
to be in the first element of the list, shadowing the earlier
definition of `to_drop`.  And that's why the final case is unused: it
is essentially the same pattern, just with a different variable name.

A better way to write this code is not to use pattern matching for
determining whether the first element is equal to `to_drop`, but to
instead just use an ordinary boolean condition and an if-statement.

```ocaml
# let rec drop_value l to_drop =
    match l with
    | [] -> []
    | hd :: tl ->
      let new_tl = drop_value tl to_drop in
      if hd = to_drop then new_tl else hd :: new_tl
  ;;
val drop_value : 'a list -> 'a -> 'a list = <fun>
# drop_value [1;2;3] 1;;
- : int list = [1; 3]
```

Note that if we wanted to drop a particular literal value (rather than
a value that was passed in), we could do this using something like the
our original implementation of `drop_value`.

```ocaml
# let rec drop_zero l =
    match l with
    | [] -> []
    | 0  :: tl -> drop_zero tl
    | hd :: tl -> hd :: drop_zero tl
  ;;
val drop_zero : int list -> int list = <fun>
# drop_zero [1;2;0;3];;
- : int list = [1; 2; 3]
```

## Limitations of patterns, and their benefits

The above example highlights an important fact about patterns, which
is that they aren't able to capture arbitrary conditions.  Instead,
patterns are limited to cases that are characterized by the layout of
a data-structure, and the details of what are checked need to be known
at compile time.  The latter point explains why we can write a pattern
that checks for a literal value `0` but not for a value passed in at
run-time.

You can think of the set of available patterns as a specialized
sub-language that can express a limited (though still quite rich) set
of conditions.  The fact that the pattern language is limited turns
out to be a very good thing, because it enables the key features of
match statements that make them so useful.  In particular, both the
efficiency of match statements and the ability of the compiler to
detect errors in matches depends on the constrained nature of
patterns.

Let's consider efficiency first.  Naively, you might think that it
would be necessary to evaluate each case in a `match` in sequence to
figure out which one fires.  If the cases of a match were guarded by
arbitrary code, that would be the case.  But in fact, OCaml is often
able to generate code that jumps directly to the matched case based on
an efficiently chosen set of checks, and so can evaluate even quite
large match statements very efficiently.

The error-detecting capabilities of match statements are if anything
more important than their perforamance.  We've already seen one
example of OCaml's ability to catch errors: in our broken
implementation of `drop_value`, OCaml warned us that the final case
was redundant.  Checking redundancy of arbitrary conditions is an
intractable problem, but the language of patterns is constrainted
enough that it can be done reliably.

Another similar check done by the compiler is a check for
exhaustiveness.  If we modify the `drop_zero` function described above
so that we forget one of the cases, the compiler will notice, and will
even identify an example of an unmatched pattern.

```ocaml
# let rec drop_zero l =
    match l with
    | [] -> []
    | 0  :: tl -> drop_zero tl
  ;;
Characters 26-84:
  ....match l with
      | [] -> []
      | 0  :: tl -> drop_zero tl
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
1::_
val drop_zero : int list -> 'a list = <fun>
```

For simple examples like this, exhaustivenss checks are a useful
bug-catching tool, but their impact is not earth shattering.  But as
we'll see in [xref](#variants), as you get to more complicated
examples involving user-defined types, the fact that pattern matches
are checked for exhaustiveness becomes an invaluable refactoring tool,
helping you find places in your code where you you need to adapt your
code to deal with type changes elsewhere in your codebase.

