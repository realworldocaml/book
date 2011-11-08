## Example: pretty-printing a table

As a simple example, let's consider the question of how to design a
library for printing out data in a tabular format.

The right way to design an OCaml library is to start with the
interface.  Here's a first cut for the interface to our new module,
`Text_table`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
val render_table
   :  string list         (* header *)
   -> string list list    (* data *)
   -> string
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where invoking the function like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let () =
  print_string (Ascii_table.render_table
     ["country";"year of founding";"national bird"]
     [ ["New Jersey";"1789";"Wood thrush"];
       ["New";"42";"6'3\""];
       ["Anil Madhavapeddy";"38";"7'3\""];
      ])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

would result in output of this form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| name              | age | height |
|-------------------+-----+--------|
| Yaron Minsky      | 40  | 5'11"  |
| Jason Hickey      | 42  | 6'3"   |
| Anil Madhavapeddy | 38  | 7'3"   |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic task here is simple: we need to render each row of the
table, and then combine them together.  To render the rows, though, we
first need to know the width of the widest thing in each column.  So
let's start by writing a function to compute that:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
let compute_max_widths header rows =
  let to_lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(to_lengths header)
    ~f:(fun acc row -> List.map2_exn ~f:Int.max acc (to_lengths row))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, we define a helper function, `to_lengths`, which takes list of
strings and return a list of the lengths of those strings.  Then,
staring with the lengths of the header, we fold over the rows, maxing
together the lengths of that row with the accumulated max-length so
far.

Note that this code will throw an exception if any of the rows has a
different number of entries than the header.  This is enforced by
`List.map2_exn`, which throws an exception when its arguments have
mismatched lengths.

Now we need to write the code to render a single row.  It's worth
noting that there are two visually different kinds of rows that need
to be rendered: a separator row, which looks like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
|-------------------+-----+--------|
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and a regular row, which looks like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| Yaron Minsky      | 40  | 5'11"  |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For one, the contents of the row are a padded string
