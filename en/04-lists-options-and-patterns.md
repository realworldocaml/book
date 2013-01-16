# Lists, Options and Patterns

_(Note, this chapter is incomplete.  jyh is working on it.)_

## Lists

As with any programming language, we need a way to represent _data_,
things like numbers, words, images, etc., and we need a way to define
_aggregates_ that bring together related values that represent some
concept.

Lists are one of the most common ways to aggregate data in OCaml; they
are simple, and they are extensively supported by the standard
library. 

_(yminsky: I would point out that they are the most commonly used
aggregate, rather than just that they're supported by the standard
library.  Also, that way we avoid confusion in the sentence. between
the standard library and Core.)_

### Example: pretty-printing a table

_(yminsky: I wonder if the whole example should go later in the
section, after we've done more of the basis of lists in this
chapter.)_

One common programming task is displaying tabular data.  In this
example, we will go over the design of a simple library to do just that.

_(yminsky: This is the first appearance of an mli file.  If we're
going to introduce it here, we need to do a little more explanation.)_

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

## List basics

_(yminsky: I wonder if this should come before the example.  The
example kind of assumes a lot of information, some of which was given
in the guided tour, and some of which wasn't.)_

In the example, we see calls to `List` functions in the standard
library, in particular `List.map`.  How does this all work?  To
understand, we need to consider first how lists are _represented_
internally, which follows from the type definition and the way lists
are constructed.  Let's look at the constructors first.

We have seen how square brackets can be used to construct a list of
values, but there are really just two ways to construct a list value.

* [] is the _empty_ list.
* If `x` is a value and `l` is a list, then the expression `x :: l`
  constructs a new list where the first element is `x`, and the rest
  is `l`.  The value corresponding to `x :: l` is commonly called a
  _cons_-cell (the term comes from Lisp, where _cons_ is short for
  "constructor").

The bracket syntax `[5; 3; 7]` is syntactic sugar for a list with 3
cons-cells, `5 :: 3 :: 7 :: []`.  Each cell has two parts: 1) a value,
and 2) a pointer to the rest of the list.  The final pointer refers to
the special value `[]` representing the empty list.

TODO: IMAGE figures/04-list-01.svg

## Pattern matching

Constructing a list is really only half the story -- it would be
pretty useless to construct lists unless we can also pull them apart.
We need _destructors_, and for this we use _pattern matching_, like we
saw in the previous chapter.

For a list, there are two possible shapes: the empty list `[]` or a
cons-cell `h :: t`.  We can use a `match` expression to perform the
pattern matching.  In the case of a cons-cell, the variables `h` and
`t` in the pattern are bound to the corresponding values in the list
when the match is performed.

For example, suppose we want to define a function to add 1 to each
element of a list.  We have to consider both cases, 1) where the list
is empty, or 2) where it is a cons-cell.

```ocaml
# let rec add1 l =
    match l with
     | [] -> []
	 | h :: t -> (h + 1) :: (add1 t);;
val add1 : int list -> int list = <fun>
# add1 [5; 3; 7];;
- : int list = [6; 4; 8]
```

The functions in the standard library can implemented in similar ways.
A straightforward, but inefficient, version of the `List.map` function
is as follows.

```ocaml
# let rec map l ~f =
    match l with
     | [] -> []
     | h :: t -> f h :: map ~f t;;
val map : 'a list -> f:('a -> 'b) -> 'b list = <fun>
# map ~f:string_of_int [5; 3; 7];;
- : string list = ["5"; "3"; "7"]
```

## List performance

Lists are ubiquitous in OCaml programs.  They are easy to use and
reasonably efficient for small lists, but large lists can have
significant performance problems.  The issue is that lists are formed
from separately allocated cons-cells.  This has space overhead because
each value in the list is paired with a pointer to the rest of the
list.  The separate allocation also reduces locality, so it can result
in poor cache behavior.

Perhaps more important than those concerns is that naive list
traversal takes time linear in the length of the list.  For example,
the following `length` function takes linear time to count the number
of elements in the list.

```ocaml
let rec length = function [] -> 0 | _ :: t -> (length t) + 1;;
```

In fact, this implementation of the function `length` is worse than
that, because the function is recursive.  In this implementation of
the function, the recursive call to `length t` is active at the same
time as the outer call, with the result that the runtime needs to
allocate stack frames for each recursive call, so this function also
takes linear space.  For large lists, this is not only inefficient,
it can also result in stack overflow.

### Tail-recursion

We can't do anything about `length` taking linear time --
singly-linked lists of this kind don't have an efficient `length`
operation.  However, we can address the space problem using _tail
recursion_.

Tail recursion occurs whenever the result of the recursive call is
returned immediately by the calling function.  In this case, the
compiler optimizes the call by skipping the allocation of a new stack
frame, instead branching directly to the called procedure.

In the definition of `length` above, the expression containing the
recursive call `(length t) + 1` is _not_ tail recursive because 1 is
added to the result.  However, it is easy to transform the function so
that it is properly tail recursive.

```ocaml
let length l =
  let rec tail_recursive_length len = function
    | [] -> len
    | _ :: t -> tail_recursive_length (len + 1) t
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
let rec map f = function
 | [] -> []
 | h :: t -> f h :: map f t;;
```

If we use the same trick as we used for the `length` method, we need
to accumulate the result _before_ the recursive call, but this
collects the result in reverse order.  One way to address it is to
construct the reserved result, then explicitly correct it before
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

### Hybrid recursion

In general, the choice of whether to use regular recursion vs. tail
recursion is not immediately obvious.  Regular recursion is often
better for small lists (and other data structures), but it is better
to use tail recursion for very large lists -- especially because stack
sizes limit the number of recursive calls.

Core takes a hybrid approach that can be illustrated with the
implementation of the function `Core_list.map`.

```ocaml
let map_slow l ~f = rev (rev_map l ~f);;

let rec count_map ~f l ctr =
  match l with
  | [] -> []
  | [x1] -> let f1 = f x1 in [f1]
  | [x1; x2] -> let f1 = f x1 in let f2 = f x2 in [f1; f2]
  | [x1; x2; x3] ->
    let f1 = f x1 in
	let f2 = f x2 in
	let f3 = f x3 in
	[f1; f2; f3]
  | [x1; x2; x3; x4] ->
    let f1 = f x1 in
	let f2 = f x2 in
	let f3 = f x3 in
	let f4 = f x4 in
	[f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    let f1 = f x1 in 
	let f2 = f x2 in
	let f3 = f x3 in
	let f4 = f x4 in
	let f5 = f x5 in
	f1 :: f2 :: f3 :: f4 :: f5 ::
      (if ctr > 1000 then map_slow ~f tl else count_map ~f tl (ctr + 1));;

let map l ~f = count_map ~f l 0;;
```

For performance, there are separate patterns for small lists with up
to 4 elements, then a recursive case for lists with five or more
elements.  The `ctr` value limits the recursion -- regular recursion
is used for up to 1000 recursive calls (which includes lists with up
to 4000 elements), then the tail-recursive function `map_slow` is used
for any remainder.

As an aside, you might wonder why this implementation uses explicit
let-definitions for the result values `f1`, `f2`, etc.  The reason is
to force the order of evaluation, so that the the function `f` is
always applied to the list values left-to-right (starting with the
first element in the list).  In an expression like
`[f x1; f x2; f x3]` the order of evaluation is not specified by the
language, any of the subexpressions might be evaluated first (though
we would often expect evaluation order to be either left-to-right or
right-to-left).  For functions that perform I/O, or have other
side-effects, left-to-right evaluation order is important (and
required).

## Heterogenous values

Lists are fairly general, but there are several reasons why you might
not want to use them.

* Large lists often have poor performance.
* The list length is variable, not fixed.
* The data in a list must have the same type.

In the tabulaton example that we used to start this chapter, the
`List` is not a good choice for each entry in the table.  Now, let's
think about how you might actually use this interface in practice.
Usually, when you have data to render in a table, the data entries are
described more precisely by a record.  So, imagine that you start off
with a record type for representing information about a given
programming language:

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

## Options

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
			style = Imperative
		  };;
```

We can also represent a data structure with NULL-pointers using the
`option` type.  For example, let's build an imperative singly-linked
list, where new values are added to the _end_ of the list.  In a
standard imperative language (like in the C++ Standard Template
Library), NULL is used to represent "end of list."  We'll use the
`option` type instead.

```ocaml
type 'a slist = { mutable head : 'a elem option; mutable tail : 'a elem option }
and 'a elem = { value : 'a; mutable next : 'a elem option };;

let new_slist () = { head = None; tail = None };;

let push_back l x =
  let elem = { value = x; next = None } in
  match l.tail with
   | None -> l.head <- Some elem; l.tail <- Some elem
   | Some last -> last.next <- Some elem;;
```

Similarly, if we're defining a type of binary trees, one choice is to
use `option` for the child node references.  In a binary search tree,
each node in the tree is labeled with a value and it has up to two
children.  The nodes in the tree follow _prefix_ order, meaning that
the label of the left child is smaller than the label of its parent,
and the label of the right child is larger than the label of the
parent.

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
