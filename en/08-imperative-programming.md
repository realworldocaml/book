# Imperative Programming

The OCaml programming language is _functional_, meaning that functions
are first-class values that can be passed around like any other.
However, this doesn't mean that OCaml programs are _pure_.  The
language includes assignment, mutable values like arrays and strings.
Evaluation order is strict and sequential.  In principle, you can port
many imperative programs directly to OCaml.  If you find yourself
doing this a lot, then OCaml may not be the right programming language
for your problem.  However, there are times when imperative
programming is both appropriate and efficient, and OCaml shines at
supporting programs with both functional and imperative aspects.

To illustrate imperative programming, let's start by implementing a hash table.
Hash tables are an efficient way to implement imperative _dictionaries_.  There
are full-featured implementations of hash tables in Core as well as in the OCaml
standard library.  For illustration, we'll construct just a basic dictionary
using _open hashing_, where there are an array of buckets, each of which contain
a linked list of elements.  We'll use regular (pure) OCaml lists, and an array
for the buckets.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module HashMap : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b
end = struct
  type ('a, 'b) t = ('a * 'b) list array

  let num_buckets = 17
  let hash_bucket key = (Hashtbl.hash key) mod num_buckets

  let create () = Array.create num_buckets []

  let add table ~key ~data =
    let index = hash_bucket key in
    table.(index) <- (key, data) :: table.(index)

  let find table ~key =
    let rec find = function
     | [] -> raise Not_found
     | (k, d) :: _ when k = key -> d
     | _ :: t -> find t
    in
    find table.(hash_bucket key)
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The signature for the `HashMap` declares the type of dictionaries `('a, 'b) t`,
with keys of type `'a` and associated values of type `'b`.  It also includes
three functions.  The `create` function constructs an empty dictionary.  The
`add` function adds a key/value association in the dictionary, by
_side-effect_.  That is, the hash table is modified _in place_, and any
references to the table will be able to observe the change.  Furthermore, the
`add` method doesn't return a useful value; the type `unit` contains just the
trivial value `()`, which is used by convention to represent "nothing."  The
`find` function returns the value associated with a key, raising the exception
`Not_found` if the table does not contain the key.

The hash table is implemented as an array of buckets (fixed-size, in this
example).  The OCaml runtime provides a builtin polymorphic hash function
`Hashtbl.hash` that works for almost any value in OCaml, excluding functions and
abstract values like C data.  The `create` function create a new array where all
buckets are empty.  The `add` function uses the hash function to determine the
appropriate bucket, then adds a new key/value association to the bucket through
an array _assignment_ `table.(index) <- (key, data) :: table.(index)`, which
_replaces_ the bucket with a new one where the new key/value pair is added to
the front of the list.  The `find` function performs a linear search through the
appropriate bucket to find the value associated with a key.

## Imperative operations

This example illustrates _one_ of the mutating operations in OCaml: array field
assignment.  There are just two others: the contents of a string can be mutated,
and so can record fields that have been declared as `mutable`.

* `array.(index) <- expr`: Array field assignment.  See also the `Array.blit`
  functions for mutating multiple fields at once.

* `string.[index] <- char`: String element assignment.  See also the
  `String.blit` functions for mutating substrings.

* `record.label <- expr`: Record field assignment.  The field `label` must
   be declared as `mutable`.

Note that _variables are not mutable_.  Variables can refer to mutable data, but
the binding of a variable cannot be changed.  For convenience, OCaml defines a
type of "reference cell," that is a like a "box" where the contents can be
mutated.

* `ref expr` constructs a reference cell containing the value defined by the
  expression `expr`.
* `! refcell` returns the contents of the reference cell.
* `refcell := expr` replaces the contents of the reference cell.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# let x = ref 1;; 
val x : int ref = {contents = 1}
# x := 7;;
- : unit = ()
# !x;;
- : int = 7
# x.contents <- 12;;
- : unit = ()
# x.contents;;
- : int = 12
# !x;;
- : int = 12
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As the example shows, reference cells are actually just a short form for record
operations.  The `ref` type is a record with a single mutable field `contents`,
with the following definition.  The expression `!x` is equivalent to
`x.contents`, and `x := e` is equivalent to `x.contents <- e`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
type 'a ref = { mutable contents : 'a };;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## TODO

0. Queues.
1. Splay trees.
2. Lazy.
3. Concurrent collections.
4. Iteration.
