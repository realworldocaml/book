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

## Doubly-linked lists

Another common imperative data structure is the doubly-linked list, which allows
traversal in both directions, as well as O(1) deletion of any element.
Doubly-linked lists are a _cyclic_ data structure, meaning that it is possible
to follow a nontrivial sequence of references from an element, through other
elements, back to itself.  Cyclic data structures can be constructed only
through side-effects, by constructing a set of data elements first, then using
assignment to set up the references.  (Some kinds of cyclic data structures can
also be constructed with `let rec`.)

For doubly-linked lists, we define an element `'a element` with a reference both
to the previous and next elements.  The elements at the ends have nothing to
refer to, so we use an option to allow the reference to be `None`.  The element
record fields are declared as `mutable` to allow them to be modified when the
list is mutated.

The list itself is either empty, or it refers to the first element of the list.
We use the type `type 'a dlist = 'a element option ref`; the `ref` allows the
list to be mutated, and the value is either `None` for the empty list, or `Some
first_element` if the list is non-empty.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
type 'a element =
  { mutable value : 'a;
    mutable next : 'a element option;
    mutable previous : 'a element option
  }

type 'a dlist = 'a element option ref

let create () = ref None

let is_empty l = (!l = None)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The function `create` creates an empty list.  The function `is_empty l`
dereferences the list using the `!` operator, returning true if the value is
`None`, or false otherwise.

Next, let's define the function that inserts a value onto the front of the list
as a new first element.  We define a new element `new_front`, link in the new
element, and set the list references.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
let push_front l ~data =
  let new_front = { value = data; next = None; previous = None } in
  begin match !l with
   | Some el ->
     el.previous <- Some new_front;
     new_front.next <- Some el
   | None ->
     ()
  end;
  l := Some new_front
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This example introduces the _sequencing_ operator `;`.  In the case where the
list is non-empty (the `Some el` case in the `match`), we first set
`el.previous` to refer to the `new_front` element, and next set `new_front.next`
to refer to `el`.

In general, when a sequence expression `expr1; expr2` is evaluated, `expr1` is
evaluated first, and then `expr2`.  The expression `expr1` must have type
`unit`, and the the value of `expr2` is returned as the value of the entire
sequence.  So, for example, the sequence `print_string "hello world"; 1 + 2`
first prints the string `"hello world"`, then returns the integer `3`.

There are a few more things to note.  First, semicolon `;` is a _separator_, not
a terminator, like it is in C or Java.  The compiler is somewhat relaxed about
parsing a terminating semicolon, so it may work for you, but you should not rely
on it.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# let i = print_string "Hello world\n"; 2; in i;;
Hello world
- : int = 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Also note, the precedence of a `match` expression is very low, so to separate it
from the following assignment `l := Some new_front`, we surround the match in a
`begin ... end` bracketing (we could also use parentheses).  If we did not, the
final assignment would become part of the `None -> ...` case, which is not what
we want.

To complete this initial part of the implementation, let's define function
`front` to return the first element, and `pop_front` to remove it.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
let front = function
 | { contents = Some { value = v } } -> v
 | { contents = None } -> raise (Invalid_argument "front")

let pop_front l =
  match !l with
   | Some { value = v; next = None } -> l := None; v
   | Some { value = v; next = (Some el) as next } ->
     l := next;
     el.previous <- None;
     v
   | None -> raise (Invalid_argument "pop_front")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For illustration the `front` function uses pattern matching on the reference
cell -- it would be equivalent to write an explicit dereference `let front l =
match !l with Some { value = v } -> v | None -> ...`.

The function `pop_front` performs the unlinking.  There are three cases, 1) the
list has one element, so it becomes empty, 2) the list has more than one
element, so the second element is relinked, or 3) the list is empty, which is an
error.  In the second case, the list `l` is set to point to the second element,
the `previous` field is set to `None`, and `v` is returned.

## Iteration

When defining containers like lists, dictionaries, trees, etc. it is
conventional to define some kind of iteration to allow the elements of the
collection to be enumerated.  When the containers are immutable, like `'a list`,
this is normaly done with functions like `iter`, `map`, and `fold`.  Each of
these iteration functions takes a function that will be applied to each of the
elements in order.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# List.iter (fun i -> Printf.printf "Element: %d\n" i) [1; 2; 3];;
Element: 1
Element: 2
Element: 3
- : unit = ()
# List.map ((+) 10) [1; 2; 3];;
- : int list = [11; 12; 13]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defining this for doubly-linked lists is simple enough.  The following function
iterates through the list, applying the function `f` to each element in turn.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
let iter f l =
  let rec loop = function
   | Some { value = v; next = next } -> f v; loop next
   | None -> ()
  in
  loop !l;;

# let l = create ();
  push_front l 1;
  push_front l 2;
  push_front l 3;
  DList.iter (Printf.printf "Item: %d\n") l;;
Item: 3
Item: 2
Item: 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This style of iteration is concise, and completely general.  However, with
imperative containers, we often want more control.  We may not want to iterate
through all elements, and we often want to mutate the container as we iterate.
One conventional way to do this is to define a generic `iterator` type that can
be used to enumerate and/or mutate the elements in a container.  This is a style
seen, for example, in Java (type `Iterator`) or the C++ Standard Template
Library.

Let's define a Java-style kind of generic iterator object that allows manual
enumeration and mutation of the container.  Here is the Java interface.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {Java}
public iterface Iterator {
  public boolean hasNext();
  public Object next();
  public void remove();
};
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At any time, a `Iterator` object refers (optionally) to some element of a
container.  The `hasNext()` method returns true if the iterator refers to an
element; the method `next()` returns the element, and advances to the next one;
and `remove()` removes the last element returned by the iterator.

When we define a similar iterator concept in OCaml, we need to choose how to
represent it.  We _could_ define a separate iterator type for each kind of
container, but this would be inconvenient, since iterators have similar behavior
for many different kinds of containers.  To define a _generic_ iterator, there
are two reasonable choices: we can use first-class modules, or objects.  The
first-class modules chapter explores this concept, so let's look at an
implementation using objects.

You can skip forward to the Objects chapter for more informatation about
objects, but we'll be using basic objects, which are just collections of
methods, similar to having a record of functions -- we could also implement the
iterator as a record of functions, but the code would be somewhat more verbose.

First, we need to define a generic iterator type.  For clarity, we'll use a more
verbose type where retreiving a value is separate from advacing to the next
element.  The object type is specified like a record type, but using angle
brackets `< ... >`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
type 'a iterator =
   < has_value : bool; value : 'a; next : unit; remove : unit >
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each of the labeled parts `has_value`, `value`, etc. are object _methods_.  This
object type corresponds to an _interface_ consisting of a set of methods.

Next, to define the iterator implementation, we implement each of the methods,
bracketed by `object ... end`, declaring each method with the `method` keyword.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
let iterator (list : 'a t) =
  let current = ref !list in
  object
    method has_value = !current <> None
    method value =
      match !current with
       | Some { value = v } -> v
       | None -> raise (Invalid_argument "next")
    method next =
      match !current with
       | Some { next = next } -> current := next
       | None -> raise (Invalid_argument "next")
    method remove =
      match !current with
       | Some { previous = previous; next = next } ->
            (match previous with
              | Some el -> el.next <- next
              | None -> list := next);
            (match next with
              | Some el -> el.previous <- previous;
              | None -> ());
            current := next
       | None -> raise (Invalid_argument "remove")
  end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The reference cell `current` holds the current position in the list.  The method
`has_value` returns true if `current` refers to an element, `value` returns the
element, and `next` advances the iterator.  The method `remove` unlinks the
`current` element by setting the previous element's `next` pointer, and the
next's elements `previous` pointer, then advancing `current` to the next
element.  The following example illustrates the semantics.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let l = create ();
  push_front l 1;
  push_front l 2;
  push_front l 3;
  DList.iter (Printf.printf "Item: %d\n") l;;
Item: 3
Item: 2
Item: 1
# let it = iterator l;;
val it : int iterator = <obj>
# it#value;;
- : int = 3
# it#next;;
- : unit = ()
# it#value;;
- : int = 2
# it#remove;;
- : unit = ()
# it#value;;
- : int = 1
# it#next;;
- : unit = ()
# it#has_value;;
- : bool = false
# iter (Printf.printf "%d\n") l;;            
3
1
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the doubly-linked list is a _cyclic_ data structure.  Most notably,
the builtin equality _does not work_ in general with cyclic values.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# let l2 = DList.create();;
val l2 : '_a DList.t = <abstr>
# push_front l2 1;;
- : unit = ()
# push_front l2 3;;
- : unit = ()
# l == l2;;
- : bool = false
# l = l2;;
Out of memory during evaluation.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## TODO

0. Queues.
1. Splay trees.
2. Lazy.
3. Concurrent collections.
4. Iteration.
