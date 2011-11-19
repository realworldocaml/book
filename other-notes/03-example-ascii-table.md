## Example: pretty-printing a table

One common programming task is displaying tabular data.  In this
example, will go over the design of a simple library to do just that.

The right way to design an OCaml library is to start with the
interface.  Here's a first cut for the interface to our new module,
`Text_table`, which contains a single function, `render`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(* [render headers rows] returns a string containing a formatted
   text table, using unix-style newlines as separators *)
val render
   :  string list         (* header *)
   -> string list list    (* data *)
   -> string
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you invoke `render` as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let () =
  print_string (Text_table.render
     ["language";"architect";"style"]
     [ ["simula";"who?";"object-oriented"]
       ["ocaml";"Xavier Leroy";"functional"];
       ["scala";"Martin Odersky";"OO/functional"];
      ])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You'll get string output that looks like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| language | architect      | first release |
|----------+----------------+---------------|
| Lisp     | John McCarthy  | 1962          |
| C        | Who?           | 1973          |
| ML       | Robin Milner   | 1976          |
| OCaml    | Xavier Leroy   | 1992          |
| Scala    | Martin Odersky | 1998          |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now that we know how it's supposed t work, let's dive into the
implementation.

### Computing the widths

In order to render a table, we first need to compute the width of the
widest entry in each column.  The following function does just that.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let compute_max_widths header rows =
  let to_lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(to_lengths header)
    ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (to_lengths row))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, we define a helper function, `to_lengths`, which uses
`List.map` and `String.length` to convert a list of strings to a list
of string lengths.  Then, starting with the lengths of the headers, we
use `List.fold` to join in the lengths of the elements of each row by
`max`'ing them together elementwise.


Note that this code will throw an exception if any of the rows has a
different number of entries than the header.  In particular,
`List.map2_exn` throws an exception when its arguments have mismatched
lengths.

### Rendering the rows

Now we need to write the code to render a single row.  There are
really two different kinds of rows that need to be rendered: an
ordinary row:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| Lisp     | John McCarthy  | 1962          |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and a separator row:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
|----------+----------------+---------------|
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's start with the separator row, which we can generate as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let render_separator widths =
  let pieces = List.map widths
    ~f:(fun width -> String.make (width + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We need the extra two-characters for each entry to account for the one
character of padding on each side of a string in the table.

#### Note: Performance of `String.concat` and `^`
_[yminsky: need some way of segmenting the following out nicely]_

In the above, we're using two different ways of concatenating strings,
`String.concat`, which operates on lists of strings, and `^`, which is
a pairwise operator.  Note that for long collections of strings to be
joined, you should avoid `^`, since, it allocates new strings every
time it's run.  Thus, the following code:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let s = "." ^ "."  ^ "."  ^ "."  ^ "."  ^ "."  ^ "."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

will allocate a string of length 2, 3, 4, 5, 6 and 7, whereas this
code:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let s = String.concat [".";".";".";".";".";".";"."]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allocates one string of size 7, as well as a list of length 7.  At
these small sizes, the differences don't amount ot much, but for
assembling of large strings, it can be a very serious issue.

_[yminsky: end the note here]_

We can write a very similar piece of code for rendering the data in a
an ordinary row.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let pad s length =
  if String.length s >= length then s
  else s ^ String.make (length - String.length s) ' '

let render_row row widths =
  let pieces = List.map2 row widths
    ~f:(fun s width -> " " ^ pad s width ^ " ")
  in
  "|" ^ String.concat ~sep:"|" pieces ^ "|"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

