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

We can read data out of a list using a match statement.  Here's a
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
pattern-indexed list of cases.  Second, it lets you name
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
something is wrong.  Moreover, the function clearly does the wrong
thing, filtering out all elements of the list rather than just those
equal to the provided value.

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
value `to_drop` passed in as an argument to `drop_value`.  Instead, it
just causes a new variable `to_drop` to be bound to whatever happens
to be in the first element of the list, shadowing the earlier
definition of `to_drop`.  The third case is unused because it is
essentially the same pattern as was used in the second case.

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
a value that was passed in), we could do this using something like our
original implementation of `drop_value`.

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

## Limitations (and blessings) of pattern matching

The above example highlights an important fact about patterns, which
is that they can't be used to express arbitrary conditions.  Patterns
can characterize the layout of a data-structure, and can even include
literals as in the `drop_zero` example, but that's where they stop.  A
pattern can check if a list has two elements, but it can't check if
the first two elements are equal to each other.

You can think of patterns as a specialized sub-language that can
express a limited (though still quite rich) set of conditions.  The
fact that the pattern language is limited turns out to be a very good
thing, making it possible to build better support for patterns in the
compiler.  In particular, both the efficiency of match statements and
the ability of the compiler to detect errors in matches depend on the
constrained nature of patterns.

### Performance

Naively, you might think that it would be necessary to check each case
in a `match` in sequence to figure out which one fires.  If the cases
of a match were guarded by arbitrary OCaml code, that would be the
case.  But OCaml is often able to generate code that jumps directly to
the matched case based on an efficiently chosen set of run-time
checks.  This means that match statements that would be slow when
implemented as a sequence of checks can run quickly as a match.

As an example, consider the following rather silly functions for
incrementing an integer by one.  The first is implemented with a
match statement, and the second, with a sequence of if statements.

```ocaml
let plus_one x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | _ -> x + 1

let plus_one_slow x =
  if      x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else x + 1
```

If you benchmark these, you'll see that `plus_one_slow` is about 35%
slower than `plus_one`.  And the advantage gets larger as the number
of cases increases.  As another example, we can rewrite the `sum`
function we described earlier in the chapter using an `if` statement
rather than a match.  Here, we're using the functions `is_empty`,
`hd_exn` and `tl_exn` from the `List` module to deconstruct the list,
rather than using pattern matching.

```
let rec sum_slow l =
  if List.is_empty l then 0
  else List.hd_exn l + sum_slow (List.tl_exn l)
;;
```

In this case, the match-based implementation is 70% faster than the
one using if.  The difference comes because we need to effectively do
the same check multiple times, since each function we call has to
re-examine the first element of the list to determine whether or not
it's the empty cell, whereas with the match statement this happens
exactly once per list element.

Overall, pattern matching is very efficient; typically more efficient
than the alternatives one might code by hand.  One notable exception
is matches over strings, which are in fact tested sequentially.  But
most of the time, using pattern matching is a performance win.

### Detecting errors

The error-detecting capabilities of match statements are if anything
more important than their performance.  We've already seen one example
of OCaml's ability to find problems in a pattern match: in our broken
implementation of `drop_value`, OCaml warned us that the final case
was redundant.  There are no algorithms for determining if a predicate
written in a general-purpose language is redundant, but it can be
solved reliably in the context of patterns.

OCaml also checks match statements for exhaustiveness.  Consider what
happens if we modify `drop_zero` by deleting the handler for one of
the cases.

```ocaml
# let rec drop_zero l =
    match l with
    | [] -> []
    | 0  :: tl -> drop_zero tl
  ;;
```

The compiler will produce a warning that we've missed a case, along
with an example of an unmatched pattern.

```
Characters 26-84:
  ....match l with
      | [] -> []
      | 0  :: tl -> drop_zero tl
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
1::_
val drop_zero : int list -> 'a list = <fun>
```

For simple examples like this, exhaustiveness checks are useful
enough.  But as we'll see in [xref](#variants), as you get to more
complicated examples, especially those involving user-defined types,
the fact that pattern matches are checked for exhaustiveness become
invaluable, both as a way of catching outright errors and as a
refactoring tool, with such compiler errors guiding you to the
locations where you need to adapt your code to deal with changing type
definitions.

## Using the `List` module effectively

In this chapter and before, we've written a fair amount of direct
list-munging code using pattern matching and recursive functions.  In
real-world programming, you need to do this comparatively rarely.
Most of the time, you're better off using functions from the `List`
module which is full of reusable functions that abstract out common
patterns for computing with lists.

To see this, let's work through a concrete example.  In particular,
we'll write code to render a table of data.  In particular, we'll
write a function `render_table` that, given a list of column headers
and a list of rows, prints them out in a well formatted text table.
So, if you were to write:

```ocaml
# printf "%s\n"
   (render_table
     ["language";"architect";"first release"]
     [ ["Lisp" ;"John McCarthy" ;"1958"] ;
       ["C"    ;"Dennis Ritchie";"1969"] ;
       ["ML"   ;"Robin Milner"  ;"1973"] ;
       ["OCaml";"Xavier Leroy"  ;"1996"] ;
     ]);;
```

it would generate the following output.

```
| language | architect      | first release |
|----------+----------------+---------------|
| Lisp     | John McCarthy  | 1958          |
| C        | Dennis Ritchie | 1969          |
| ML       | Robin Milner   | 1973          |
| OCaml    | Xavier Leroy   | 1996          |
```

Let's now dive into the implementation.  The first thing we need to do
is to compute the maximum widths of each column of data.  We can do
this by converting the header and each row into a list of integer
lengths, and the taking the element-wise max of those lengths.
Writing the code for all of this directly can be a bit of a chore, but
we can it quite concisely if we make use of some built in functions
from the list module.  We'll use three in particular, `List.map`,
`List.map2_exn`, and `List.fold`.

`List.map` is the simplest to explain.  It takes a list and a function
for transforming elements of that list, and returns a new list with
the transformed elements.  Thus, we can write:

```ocaml
# List.map ~f:String.length ["Hello"; "World!"];;
- : int list = [5; 6]
```

`List.map2_exn` is similar to `List.map`, except that it takes two
lists and a function for combining them.  Thus, we might write:

```ocaml
# List.map2_exn ~f:Int.max [1;2;3] [3;2;1];;
- : int list = [3; 2; 3]
```

The `_exn` is there because the function throws an exception if the
lists are of mismatched length.

```ocaml
# List.map2_exn ~f:Int.max [1;2;3] [3;2;1;0];;
Exception: (Invalid_argument "length mismatch in rev_map2_exn: 3 <> 4 ").
```

The final, and most complicated function is `List.fold`.  With
`List.fold`, you provide an initial value, and a function for updating
that initial value with the information from a list element.
`List.fold` then walks over the list elements from left to right,
carrying forward the updated initial value as an accumulator.  It's
worth looking at the type for `List.fold` and puzzling through it:

```ocaml
# List.fold;;
- : 'a list -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum = <fun>
```

We can use `List.fold` for something as simple as summing up a list:

```ocaml
# List.fold ~init:0 ~f:(+) [1;2;3;4];;
- : int = 10
```

But we can also use it for more complicated calculations where the
accumulator does not necessarily have the same type as the elements of
the list.  Here, for example, is how you can use `fold` to reverse a
list.

```ocaml
# List.fold ~init:[] ~f:(fun list x -> x :: list) [1;2;3;4];;
- : int list = [4; 3; 2; 1]
```

Let's bring this all together to write a function to compute the
maximum widths for the table.

```ocaml
# let max_widths header rows =
    let to_lengths l = List.map ~f:String.length l in
    List.fold rows
      ~init:(to_lengths header)
      ~f:(fun acc row ->
        List.map2_exn ~f:Int.max acc (to_lengths row))
  ;;
val max_widths : string list -> string list list -> int list = <fun>
```

Here, we define the function `to_lengths` which converts a list of
strings to a list of integer lengths.  Then `fold`, using the lengths
of the header row as the initial accumulator, uses `map2_exn` to take
the max of the accumulator with the lengths of the strings in each row
of the table.

The next thing we'll need to do is to compute the separator row, based
on the widths.  Here's a simple function for doing that, which uses
`List.map` to convert a width to an appropriately sized string of
dashes.  For generating the strings themselves we use `String.concat`,
which concatenates a list of strings with an optional separator
string, and `^` concatenating a pair of strings.

```ocaml
# let render_separator widths =
    let pieces = List.map widths
      ~f:(fun w -> String.make (w + 2) '-')
    in
    "|" ^ String.concat ~sep:"+" pieces ^ "|"
  ;;
val render_separator : int list -> string = <fun>
# render_separator [3;6;2];;
- : string = "|-----+--------+----|"
```

Note that we make the line of dashes two larger than the provided
width to provide some whitespace around each entry in the table.

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

Now we need code for rendering a row with data in it.  We'll first
write a function `pad` for padding out a string to a specified length:

```ocaml
# let pad s length =
    if String.length s >= length then s
    else s ^ String.make (length - String.length s) ' '
  ;;
val pad : string -> int -> string = <fun>
# pad "hello" 10;;
- : string = "hello     "
```

And then we'll write a function for rendering a whole row by merging
together the padded out strings.  Again, we'll use `List.map2_exn` for
combining the list of data in the row with the list of widths.

```ocaml
# let render_row row widths =
    let pieces = List.map2_exn row widths
      ~f:(fun s width -> " " ^ pad s width ^ " ")
    in
    "|" ^ String.concat ~sep:"|" pieces ^ "|"
  ;;
val render_row : string list -> int list -> string = <fun>
# render_row ["Hello";"World"] [10;15];;
- : string = "| Hello      | World           |"
```

Now we can bring this all together in a single function that renders
the table.

```ocaml
# let render_table header rows =
    let widths = max_widths header rows in
    String.concat ~sep:"\n"
      (render_row header widths
       :: render_separator widths
       :: List.map rows ~f:(fun row -> render_row row widths)
      )
  ;;
val render_table : string list -> string list list -> string = <fun>
```

## Tail recursion

## Writing efficient list-handling code

_(Cover as-patterns here)_

## TODO

- Underscores in pattern matches
