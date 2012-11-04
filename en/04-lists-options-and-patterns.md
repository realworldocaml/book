# Lists, Options and Patterns

_(Note, this chapter is highly incomplete.  jyh is working on it.)_

## Lists

As with any programming language, we need a way to represent _data_,
things like numbers, words, images, etc., and we need a way to define
_aggregates_ that bring together related values that represent some
concept.

In a strongly-typed language like OCaml, the structure of data is
closely related to the data _type_.  The type defines the ``shape'' of
the data, and it also specifies exactly what operations are used to
_construct_ and _destruct_ values of that type.  This relationship is
essential -- we will always discuss data in relation to its type, its
constructors, and its destructors.

Let's start with an example using _lists_.  Lists are one of the most
common ways to aggregate data in OCaml; they are simple, and they are
extensively supported by the standard library.

### Example: pretty-printing a table

One common programming task is displaying tabular data.  In this
example, we will go over the design of a simple library to do just that.

We'll start with the interface.  The code will go in a new module
called `Text_table` whose `.mli` contains just the following function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* [render headers rows] returns a string containing a formatted
   text table, using Unix-style newlines as separators *)
val render
   :  string list         (* header *)
   -> string list list    (* data *)
   -> string
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you invoke `render` as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let () =
  print_string (Text_table.render
     ["language";"architect";"first release"]
     [ ["Lisp" ;"John McCarthy" ;"1958"] ;
       ["C"    ;"Dennis Ritchie";"1969"] ;
       ["ML"   ;"Robin Milner"  ;"1973"] ;
       ["OCaml";"Xavier Leroy"  ;"1996"] ;
     ])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

you'll get the following output:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| language | architect      | first release |
|----------+----------------+---------------|
| Lisp     | John McCarthy  | 1958          |
| C        | Dennis Ritchie | 1969          |
| ML       | Robin Milner   | 1973          |
| OCaml    | Xavier Leroy   | 1996          |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now that we know what `render` is supposed to do, let's dive into the
implementation.

#### Computing the widths

To render the rows of the table, we'll first need the width of the
widest entry in each column.  The following function does just that.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let max_widths header rows =
  let to_lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(to_lengths header)
    ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (to_lengths row))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| Lisp     | John McCarthy  | 1962          |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and a separator row:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
|----------+----------------+---------------|
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's start with the separator row, which we can generate as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let render_separator widths =
  let pieces = List.map widths
    ~f:(fun w -> String.make (w + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We need the extra two-characters for each entry to account for the one
character of padding on each side of a string in the table.

<sidebar>
<title>Performance of `String.concat` and `^`</title>

In the above, we're using two different ways of concatenating
strings, `String.concat`, which operates on lists of strings, and
`^`, which is a pairwise operator.  You should avoid `^` for joining
long numbers of strings, since, it allocates a new string every time
it runs.  Thus, the following code:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let s = "." ^ "."  ^ "."  ^ "."  ^ "."  ^ "."  ^ "."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

will allocate a string of length 2, 3, 4, 5, 6 and 7, whereas this
code:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let s = String.concat [".";".";".";".";".";".";"."]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allocates one string of size 7, as well as a list of length 7.  At
these small sizes, the differences don't amount to much, but for
assembling of large strings, it can be a serious performance issue.

</sidebar>

We can write a very similar piece of code for rendering the data in
an ordinary row.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let pad s length =
  if String.length s >= length then s
  else s ^ String.make (length - String.length s) ' '

let render_row row widths =
  let pieces = List.map2 row widths
    ~f:(fun s width -> " " ^ pad s width ^ " ")
  in
  "|" ^ String.concat ~sep:"|" pieces ^ "|"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might note that `render_row` and `render_separator` share a bit of
structure.  We can improve the code a bit by factoring that repeated
structure out:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let decorate_row ~sep row = "|" ^ String.concat ~sep row ^ "|"

let render_row widths row =
  decorate_row ~sep:"|"
    (List.map2_exn row widths ~f:(fun s w -> " " ^ pad s w ^ " "))

let render_separator widths =
  decorate_row ~sep:"+"
    (List.map widths ~f:(fun width -> String.make (width + 2) '-'))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And now we can write the function for rendering a full table.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let render header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row widths header
     :: render_separator widths
     :: List.map rows ~f:(fun row -> render_row widths row)
    )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## List basics

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
  ``constructor'').

The bracket syntax `[5; 3; 7]` is syntactic sugar for a list with 3
cons-cells, `5 :: 3 :: 7 :: []`.  Each cell has two parts: 1) a value,
and 2) a pointer to the rest of the list.  The final pointer refers to
the special value `[]` representing the empty list.

![List](figures/04-list-01.svg)

## Pattern matching

Constructing a list is really only half the story -- it would be
pretty useless to construct lists unless we can also pull them apart.
We need _destructors_.  In OCaml, in general, destructors use _pattern
matching_, where we _match_ a value aginst the possible patterns that
describe its possible shapes.

For a list, there are two possible shapes: the empty list `[]` or a
cons-cell `h :: t`.  We can use a `match` expression to perform the
pattern matching.  In the case of a cons-cell, the variables `h` and
`t` in the pattern are bound to the corresponding values in the list
when the match is performed.

For example, suppose we want to define a function to add 1 to each
element of a list.  We have to consider both cases, 1) where the list
is empty, or 2) where it is a cons-cell.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# let rec add1 l =
    match l with
     | [] -> []
	 | h :: t -> (h + 1) :: (add1 t);;
val add1 : int list -> int list = <fun>
# add1 [5; 3; 7];;
- : int list = [6; 4; 8]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The functions in the standard library are implemented in similar ways.
The `List.map` function can be defined as follows (the `function`
syntax is equivalent to performing a `match`).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let rec map f = function
   | [] -> []
   | h :: t -> f h :: map f t;;
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
# map string_of_int [5; 3; 7];;
- : string list = ["5"; "3"; "7"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## List performance

Lists are ubiquitous in OCaml programs.  They are easy to use and
reasonably efficient for small lists, but large lists can have
significant performance problems.  The issue is that lists are formed
from separately allocated cons-cells.  This has space overhead because
each value in the list is paried with a pointer to the rest of the
list.  The separate allocation also reduces locality, so it can result
in poor cache behavior.

Perhaps more important than those concerns is that list traversal
takes linear time in the length of the list.  For example, the
`List.length` function counts the number of elements in the list,
taking linear time.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
let rec length = function [] -> 0 | _ :: t -> (length t) + 1;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In fact, this implementation of the function `length` is worse than
that, because the function is recursive.  In this implementation of
the function, the recursive call to `length t` is active at the same
time as the outer call, with the result that the runtime needs to
allocate stack frames for each recursive call, so this function also
takes linear space.  For large lists, this is inefficient, and it can
result in a stack overflow.

### Tail-recursion

We can't do anything about `length` taking linear time --
singly-linked lists of this kind don't have an efficient `length`
operation.  However, we can address the space problem using _tail
recursion_.

Tail recursion occurs whenever the result of the recursive call is
returned immediately by the calling function.  In this case, the
compiler optimizes the call by skipping the allocation of a new stack
frame, instead branching directly to the called procedure.

In the definition of `nth` above, the expression containing the
recursive call `(length t) + 1` is _not_ tail recursive because 1 is
added to the result.  However, it is easy to transform the function so
that it is properly tail recursive.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
let length l =
  let rec tail_recursive_length len = function
    | [] -> len
    | _ :: t -> tail_recursive_length (len + 1) t
  in tail_recursive_length 0 l;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To preserve the type of the `length` function, we hide the
tail-recursive implementation by nesting it.  The tail-recursive
implementation performs the addition _before_ the recursive call,
instead of afterwards.  Since the result of the recursive call is
returned without modification, the compiler branches directly to the
called procedure rather than allocating a new stack frame.

In other cases, it can be more problematic to use tail-recursion.  For
example, consider the `List.map` function, which has a simple
non-tail-recursive implementation.  The code is simple, but not
efficient.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
let rec map f = function
 | [] -> []
 | h :: t -> f h :: map f t;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we use the same trick as we used for the `length` method, we need
to accumulate the result _before_ the recursive call, but this
collects the result in reverse order.  One way to address it is to
construct the reserved result, then explicitly correct it before
returning.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
 let rev l =
   let rec tail_recursive_rev result = function
    | [] -> result
	| h :: t -> tail_recursive_rev (h :: result) t
   in tail_recursive_rev [] l;;

 let map f l =
   let rec rev_map result = function
    | [] -> result
	| h :: t -> rev_map (f h :: result) t
   in rev (rev_map [] l);;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The functions `tail_recursive_rev` and `rev_map` are both
tail-recursive, but at the cost of constructing an intermediate
reversed list that is immediately discarded.  One way to think of it
is that instead of allocating a linear number of stack frames, we
allocate a linear number of cons-cells.

Allocation of short-lived data in OCaml is quite cheap, so the
intermediate list is not very expensive.  The performance of the two
implementations is not significantly different, with one exception:
the tail-recursive implementation will not cause a stack overflow for
large lists, while the simple non-tail-recursive implementation will.



## Original stuff (placeholder -- do not review)


Now, let's think about how you might actually use this interface in
practice.  Usually, when you have data to render in a table, that data
comes in the form of a list of objects of some sort, where you need to
extract data from each record for each of the columns.  So, imagine
that you start off with a record type for representing information
about a given programming language:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type style =
    Object_oriented | Functional | Imperative | Logic

type prog_lang = { name: string;
                   architect: string;
                   year_released: int;
                   style: style list;
                 }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we then wanted to render a table from a list of languages, we might
write something like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let print_langs langs =
   let headers = ["name";"architect";"year released"] in
   let to_row lang =
     [lang.name; lang.architect; Int.to_string lang.year_released ]
   in
   print_string (Text_table.render headers (List.map ~f:to_row langs))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is OK, but as you consider more complicated tables with more
columns, it becomes easier to make the mistake of having a mismatch in
between `headers` and `to_row`.  Also, adding, removing and reordering
columns becomes awkward, because changes need to be made in two
places.

We can improve the table API by adding a type which is a first-class
representative for a column.  We'd add the following to the interface
of `Text_table`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Thus, the `column` functions creates a `column` from a header string
and a function for extracting the text for that column associated with
a given row.  Implementing this interface is quite simple:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type 'a column = string * ('a -> string)
let column header to_string = (header,to_string)

let column_render columns rows =
  let header = List.map columns ~f:fst in
  let rows = List.map rows ~f:(fun row ->
    List.map columns ~f:(fun (_,to_string) -> to_string row))
  in
  render header rows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can rewrite `print_langs` to use this new interface as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let columns =
  [ Text_table.column "Name"      (fun x -> x.name);
    Text_table.column "Architect" (fun x -> x.architect);
    Text_table.column "Year Released"
       (fun x -> Int.to_string x.year_released);
  ]

let print_langs langs =
  print_string (Text_table.column_render columns langs)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
