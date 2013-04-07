# Lists and Patterns

This chapter will focus on two common elements of programming in
OCaml: lists and pattern matching.  Both of these were discussed in
[xref](#a-guided-tour), but we'll go into more depth here, presenting
the two topics together, and using one to help illustrate the other.

We'll start with lists.  In OCaml, a list is an immutable, finite
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

We can also deconstruct lists, using the `match` statement.  Here's a
simple example of a function that drops the first element of a list,
if there is one.

```ocaml
# let drop_first l =
    match l with
    | hd :: tl -> tl
    | [] -> []
  ;;
val drop_first : 'a list -> 'a list = <fun>
# drop_first [1;2;3];;
- : int list = [2; 3]
# drop_first [];;
- : 'a list = []
```

In the above we follow the convention of using `hd` to represent the
first element (or head) of the list, and `tl` to represent the
remainder of the list.

The `match` statement above is doing two things: first, it's acting as
a case-analysis tool, breaking down the possibilities into a concrete
list of patterns that might be matched.  And second, it's acting as a
way of naming sub-structures within the data-structures its matching.
In particular, the variables `hd` and `tl` are bound by the first case
of the match statment.



