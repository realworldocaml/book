# Imperative Programming

Most of the code shown so far in this book, and indeed, most OCaml
code in general, is _pure_.  Pure code works without mutating the
program's internal state, performing I/O, reading the clock, or in any
other way interacting with changeable parts of the world.  Thus, a
pure function behaves like a mathematical function, always returning
the same results when given the same inputs.  _Imperative_ code, on
the other hand, operates by side-effects that modify a program's
internal state or interacts with the outside world, and so can have a
different effect, and return different results, every time they're
called.

Pure code is the default in OCaml, and for good reason --- it's
generally easier to reason about, less error prone and more
composeable.  But imperative code is of fundamental importance to any
practical programming language because real-world tasks require that
you interact with the outside world, whether by receiving a network
packet or by writing data to a file.  Imperative programming can also
be important for performance.  While pure code is quite efficient in
OCaml, there are many algorithms that can only be implemented
efficiently using imperative techniques.

OCaml offers a happy compromise here, making it easy and natural to
program in a pure style, but also providing great support for
imperative programming where you need it.  This chapter will walk you
through OCaml's imperative features, and help you use them to their
fullest.

## A simple dictionary

We'll walk through the implementation of a simple imperative
dictionary, _i.e._, a mutable mapping from keys to values.  Both Core
and OCaml's standard library provide multiple data structures for
implementing such dictionaries, and for most real world tasks, you
should use one of those.  But we'll walk through this nonetheless as a
way of seeing OCaml's imperative constructs in action.

We'll implement our dictionary as a hash table, based on an _open
hashing_ scheme, which is to say it will be structured as an array of
buckets, with each bucket containing a list of key/value pairs that
have been hashed into that bucket.  For simplicity's sake, we'll use a
fixed-length bucket array, though growing the bucket array as more
elements are added would be necessary for a practical implementation.

First, we'll write down the `mli` for our dictionary.

```ocaml
(* file: dictionary.mli *)
open Core.Std

type ('a, 'b) t

val create : unit -> ('a, 'b) t
val length : ('a, 'b) t -> int
val add    : ('a, 'b) t -> key:'a -> data:'b -> unit
val find   : ('a, 'b) t -> 'a -> 'b option
val iter   : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
val remove : ('a, 'b) t -> 'a -> unit
```

The type `('a, 'b) t` is used to represent a dictionary with keys of
type `'a` and data of type `'b`.  The `mli` also includes a collection
of helper functions.  Notice that a number of the functions, in
particular, ones like `add` that modify the dictionary, return unit.
This is typical of functions that act by side-effect.

The implementation is shown below.  We'll go through the code bit by
bit, explaining different imperative constructs as they show up.

First, we define the type of a dictionary as a record with two fields.
The first field, `length` is declared as mutable; records are
immutable by default, but individual fields are mutable when
explicitly marked as such.  The second field, `buckets`, is immutable,
but contains an array, which is itself a mutable data structure, as
we'll see.

```ocaml
(* file: dictionary.ml *)
open Core.Std

type ('a, 'b) t = { mutable length: int;
                    buckets: ('a * 'b) list array;
                  }
```

Next, we'll define the function for choosing a hash-bucket based on
the key.  We do this using `Hashtbl.hash` to compute a hash value for
a key.  `Hashtbl.hash` is a special polymorphic function (meaning it
can take an input of any type) provided by the OCaml runtime that can
compute a hash for almost any OCaml value.  (While it can be applied
to any type as far as the type-system is concerned, it won't always
succeed.  `Hashtbl.hash` will throw an exception if given a value it
can't handle, like a function or a value from a C libraries that lives
outside the OCaml heap.)

There's also code for `create`, which creates an empty dictionary,
`length`, which grabs the length from the corresponding record field,
and `find`, which looks for a matching key in the table using
`List.find_map` on the corresponding bucket.  In `find`, you'll notice
that we make use of the `array.(index)` syntax for looking up a value
in an array.

```ocaml
let num_buckets = 17
let hash_bucket key = (Hashtbl.hash key) mod num_buckets

let create () =
  { length = 0;
    buckets = Array.create ~len:num_buckets [];
  }

let length t = t.length

let find t key =
  List.find_map t.buckets.(hash_bucket key)
    ~f:(fun (key',data) -> if key' = key then Some data else None)
```

The `iter` function, shown below, is designed to walk over all the
entries in the dictionary.  In particular, `iter d ~f` will call `f`
for each key/value pair in dictionary `d`.  

The code for `iter` uses two forms of iteration: a `for` loop is used
to iterate over the array of buckets; and within that loop,
`List.iter` is used to walk over the list of values in a given bucket.
`for` loops are syntactically convenient, and are more familiar for
someone coming from an imperative language.  But they're not of
fundamental importance to the language: instead of using `for`, we
could have implemented the outer loop using the `Array.iter`, which in
turn could be implemented as a recursive function.

```ocaml
let iter t ~f =
  for i = 0 to Array.length t.buckets - 1 do
    List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
  done
```

It's worth noting that in such an iteration function, the function `f`
is expected to return unit, because it is supposed to operate by side
effect rather than by returning a value.

Next, we have the code for adding and removing mappings from the
dictionary.  The code is made more complicated by the fact that we
need to detect whether we are overwriting or removing an existing
binding, so we can decide whether `t.length` needs to be changed.  The
helper function `bucket_has_key` is used to answer this question.

This code uses the `<-` operator for updating elements of an array
(`array.(i) <- expr`) and for updating a record field (`record.field
<- expression`).  In addition, we see the use of the semicolon to
sequence the imperative operations that are the last two lines of the
function.

Note also that we do all of the side-effecting operations at the very
end of each function.  This is good practice because it minimizes the
chance that such operations will be interrupted with an exception,
leaving the data structure in an inconsistent state.

```ocaml
let bucket_has_key t i key =
  List.exists t.buckets.(i) ~f:(fun (key',_) -> key' = key)

let add t ~key ~data =
  let i = hash_bucket key in
  let replace = bucket_has_key t i key in
  let filtered_bucket =
    if replace then
      List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
    else
      t.buckets.(i)
  in
  t.buckets.(i) <- (key, data) :: filtered_bucket;
  if not replace then t.length <- t.length + 1

let remove t key =
  let i = hash_bucket key in
  if not (bucket_has_key t i key) then ()
  else (
    let filtered_bucket =
      List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
    in
    t.buckets.(i) <- filtered_bucket;
    t.length <- t.length - 1
  )
```

## Mutable data structures

We've already encountered two different forms of mutable data: records
with mutable fields, and arrays.  We'll discuss those, and a few other
forms of mutable data, in more detail now.

### Array-like data

OCaml supports a number of array-like data structures; _i.e._,
integer-indexed containers.  The `array` type is one example, and the
`Array` module comes with a variety of mutable operations on arrays,
including `Array.set`, which modifies an individual element, and
`Array.blit`, which efficiently copies a range of values in an array.

Arrays also come with a special syntax for getting an element from an
array: `array.(index)`; and for setting an element: `array.(index) <-
expr`.  Literal arrays can be declared using `[|` and `|]` as
delimiters.  Thus, `[| 1; 2; 3 |]` is an integer array.

Strings are essentially byte-arrays, with some extra useful functions
in the `String` module for dealing with textual data in this form.
The main reason to use a `String.t` rather than a `Char.t array` (a
`Char.t` is an 8-bit character) is that the former is considerably
more space efficient; an array uses one word --- 8 bytes on a 64-bit
machine --- to store a single entry, whereas strings use one byte per
character.

Strings also come with their own syntax for getting and setting
values: `string.[index]` and `string.[index] <- expr` respectively,
and string literals are bounded by quotes.

A bigarray is a handle to a block of memory stored outside of the
OCaml heap.  These are mostly useful for interacting with C or Fortran
libraries, and are discussed in
[xref](#managing-external-memory-with-bigarrays).  Bigarrays too have
their own getting and setting syntax: `bigarray.{index}` and
`bigarray.{index} <- expr`.

### Mutable record and object fields and ref cells

As we've seen, records are immutable by default, but individual record
fields can be declared as mutable.  These mutable fields can be set
using the `<-` operator, _i.e._, `record.field <- expr`.

As we'll see in [xref](#object-oriented-programming), fields of an
object can similarly be declared as mutable, and can then be modified
in much the same way as with records.

#### Ref Cells

Variables in OCaml are never mutable --- they can refer to mutable
data, but what the variable points to can't be changed.  Sometimes,
though, you want to do exactly what you would do with a mutable
variable in another language: define a single, mutable value. In OCaml
this is typically achieved using a `ref`, which is essentially a
container with a single, mutable value.

The standard library defines the following operators for working with
refs.

* `ref expr` constructs a reference cell containing the value defined by the
  expression `expr`.
* `! refcell` returns the contents of the reference cell.
* `refcell := expr` replaces the contents of the reference cell.

You can see these in action below.

```ocaml
# let x = ref 1;;
val x : int ref = {contents = 1}
# !x;;
- : int = 1
# x := !x + 1;;
- : unit = ()
# !x;;
- : int = 2
```

There's nothing magic about a `ref`: it's really just a record.  The
`ref` type and its associated operations are defined as follows.

```ocaml
type 'a ref = { mutable contents : 'a }

let ref x = { contents = x }
let (!) r = r.contents
let (:=) r x = r.contents <- x
```

### Foreign functions

Another source of imperative operations in OCaml is resources that
come from interfacing with some external library through OCaml's
foreign function interface (FFI).  The FFI opens OCaml up to any
imperative construct that is exported by a system call, a C library,
or any other external resource that you connect to.  Many of these
come built in, like access to the `write` system call, or to the
`clock`; while others come from user libraries, like LAPACK bindings.

More generally, when you wrap a library for use in OCaml, you'll often
find yourself introducing new imperative operations to the language.

## `for` and `while` loops

OCaml provides support for traditional imperative looping constructs,
in particular, `for` and `while` loops, even though neither of them is
strictly necessary.  Anything you can do with such a loop you can also
do with a recursive function, and you can also write higher-order
functions like `Array.iter` that cover much of the same ground.

Nonetheless, explicit `for` and `while` loops are both more idiomatic
for imperative programming and often more concise.

The `for` loop is the simpler of the two.  Indeed, we've already seen
the `for` loop in action --- the `iter` function in `Dictionary` is
built using it.  Here's a simple example of `for`.

```ocaml
# for i = 0 to 3 do Printf.printf "i = %d\n" i done;;
i = 0
i = 1
i = 2
i = 3
- : unit = ()
```

As you can see, the upper and lower bounds are inclusive.  We can also
use `downto` to iterate in the other direction.

```ocaml
# for i = 3 downto 0 do Printf.printf "i = %d\n" i done;;
i = 3
i = 2
i = 1
i = 0
- : unit = ()
```

A `while`-loop on the other hand, takes a condition and a body, and
repeatedly runs the body until the condition is false.  Here's a
simple example of a function for reversing an array in-place.

```ocaml
# let rev_inplace ar =
    let i = ref 0 in
    let j = ref (Array.length ar - 1) in
    (* terminate when the upper and lower indices meet *)
    while !i < !j do
      (* swap the two elements *)
      let tmp = ar.(!i) in
      ar.(!i) <- ar.(!j);
      ar.(!j) <- tmp;
      (* bump the indices *)
      incr i;
      decr j
    done
  ;;
val rev_inplace : 'a array -> unit = <fun>
# let nums = [|1;2;3;4;5|];;
val nums : int array = [|1; 2; 3; 4; 5|]
# rev_inplace nums;;
- : unit = ()
# nums;;
- : int array = [|5; 4; 3; 2; 1|]
```

In the above, we used `incr` and `decr`, which are functions for
incrementing and decrementing an `int ref` by one, respectively.

## Doubly-linked lists

Another common imperative data structure is the doubly-linked list,
which allows traversal in both directions, as well as O(1) deletion of
any element.  Doubly-linked lists are a cyclic data structure, meaning
that it is possible to follow a nontrivial sequence of references from
an element, through other elements, back to itself.  In general,
building cyclic data structures requires the use of side-effects
(although in some limited cases, they can be constructed using `let
rec`). This is done by constructing the data elements first, and then
adding cycles using assignment afterwards.

Core defines a standard doubly-linked list, but let's define our own
implementation for illustration.  First, we define an element `'a
element` with a reference both to the previous and next elements.  The
elements at the ends have nothing to refer to, so we use an option to
allow the reference to be `None`.  The element record fields are
declared as `mutable` to allow them to be modified when the list is
mutated.

The list itself is either empty, or it refers to the first element of
the list.  We use the type `type 'a dlist = 'a element option ref`;
the `ref` allows the list to be mutated, and the value is either
`None` for the empty list, or `Some first_element` when the list is
non-empty.

```ocaml
type 'a element =
  { value : 'a;
    mutable next : 'a element option;
    mutable previous : 'a element option
  }

type 'a dlist = 'a element option ref

let create () = ref None
let is_empty l = (!l = None)

let value elt = elt.value

let first l = !l
let next elt = elt.next
let previous elt = elt.previous
```

The function `create` creates an empty list.  The function `is_empty l`
dereferences the list using the `!` operator, returning true if the value is
`None`, or false otherwise.  The `value` function returns the value stored in an
element.  The `first`, `next`, and `previous` functions allow navigation through
the list.

Next, let's define the function that inserts a value into the list as a new
first element.  We define a new element `elt`, link it into the list, and set
the list reference.

```ocaml
   let insert_first l value =
     let elt = { previous = None; next = !l; value } in
     begin match !l with
     | Some old_first -> old_first.previous <- Some elt
     | None -> ()
     end;
     l := Some elt;
     elt
```

This example introduces the sequencing operator `;` to separate the steps to be
executed in order: first, create the new element `elt`; then set
`old_first.previous` to point to it; then set the list `l` to refer to the `elt`
element; then return the element `elt`.

In general, when a sequence expression `expr1; expr2` is evaluated, `expr1` is
evaluated first, and then `expr2`.  The expression `expr1` must have type
`unit`, and the the value of `expr2` is returned as the value of the entire
sequence.  For example, the sequence `print_string "hello world"; 1 + 2` first
prints the string `"hello world"`, then returns the integer `3`.

There are a few more things to note.  First, semicolon `;` is a _separator_, not
a terminator, like it is in C or Java.  The compiler is somewhat relaxed about
parsing a terminating semicolon, so it may work for you, but you should not rely
on it.

```ocaml
# let i = print_string "Hello world\n"; 2; in i;;
Hello world
- : int = 2
```

Also note, the precedence of a `match` expression is very low, so to separate it
from the following assignment `l := Some new_front`, we surround the match in a
`begin ... end` bracketing (we could also use parentheses).  If we did not, the
final assignment would become part of the `None -> ...` case, which is not what
we want.

To complete this initial part of the implementation, let's define a function for
removal.

```ocaml
   let remove l elt =
     let { previous = previous; next = next } = elt in
     (match previous with
      | Some p -> p.next <- next
      | None -> check_is_first_element l elt; l := next);
     (match next with
      | Some n -> n.previous <- previous;
      | None -> ());
     elt.previous <- None;
     elt.next <- None
```

The `remove` function unlinks the element from the list, the resets the
`previous` and `next` links to `None`.  For safety, we detect duplicate removals
with a check.  In the case where the previous element is `None`, the element
must be first in the list.  Here is the implementation of the
`check_is_first_element` function.

```ocaml
   let check_is_first_element l elt1 =
      match !l with
      | Some elt2 when elt1 == elt2 -> ()
      | _ -> raise (Invalid_argument "element has already been removed")
```

### Iteration

When defining containers like lists, dictionaries, trees, etc. it is
conventional to define some kind of iteration to allow the elements of the
collection to be enumerated.  When the containers are immutable, like `'a list`,
this is normaly done with functions like `iter`, `map`, and `fold`.  Each of
these iteration functions takes a function that will be applied to each of the
elements in order.

```ocaml
# List.iter  [1; 2; 3] ~f:(fun i -> Printf.printf "Element: %d\n" i);;
Element: 1
Element: 2
Element: 3
- : unit = ()
# List.map [1; 2; 3] ~f:((+) 10);;
- : int list = [11; 12; 13]
```

Defining this for doubly-linked lists is simple enough.  The following function
iterates through the list, applying the function `f` to each element in turn.

```ocaml
let iter l ~f =
  let rec loop = function
  | Some { value = v; next = next } -> f v; loop next
  | None -> ()
  in
  loop !l;;

# let l = create ();
  push_front l 1;
  push_front l 2;
  push_front l 3;
  iter l ~f:(Printf.printf "Item: %d\n");;
Item: 3
Item: 2
Item: 1
```

This style of iteration is concise and completely general when you want to
iterate through all of the elements of the list.  In some cases, you may wish to
iterate through only part of the list, for example when searching for a
particular element.  In this case, you can use the functions `next` and `prev`
to navigate through the list.

```ocaml
   (* Find the element containing x, using = for comparison *)
   let find l x : 'a element option =
      let rec search = function
      | None -> None
      | Some elt ->
           if value elt = x then
              Some elt
           else
              search (next elt)
      in
      search !l
```

### Java-style iteration

One possible issue with navagation using the `next` and `prev` functions is that
code is tied specifically to the doubly-linked list code we have just defined.
It won't work for iteration through a hash-table, for example.  Another
technique used in other imperative languages like Java or C++ is to define a
generic `iterator` type that can be used to enumerate and/or mutate the elements
in multiple different kinds of containers.  Here is the Java interface.

```java
public iterface Iterator {
  public boolean hasNext();
  public Object next();
  public void remove();
};
```

At any time, a `Iterator` object refers (optionally) to some element of a
container.  The `hasNext()` method returns true if the iterator refers to an
element; the method `next()` returns the element, and also advances to the next
one; and `remove()` removes the last element returned by the iterator.

If we want to define a similar iterator concept in OCaml, we need to choose how
to represent it.  We _could_ define a separate iterator type for each kind of
container, but this would be inconvenient, since iterators have similar behavior
for many different kinds of containers.  To define a _generic_ iterator, there
are several reasonable choices: we can use first-class modules, or we can use
objects.  One of the simpler approaches is to use objects.

You can skip forward to the Objects chapter for more informatation about
objects, but we'll be using basic objects, which are just collections of
methods, similar to having a record of functions -- we could also implement the
iterator as a record of functions, but the code would be somewhat more verbose.

First, we need to define a generic iterator type.  For clarity, we'll use a more
verbose type than in Java.  We'll separate retrieving a value from advacing to
the next element.  The object type is specified like a record type, but using
angle brackets `< ... >`.

```ocaml
   type 'a iterator =
      < has_value : bool;
        value : 'a;
        next : unit;
        remove : unit;
        insert_after : 'a -> unit
      >
```

Each of the labeled parts `has_value`, `value`, etc. are object _methods_.  This
object type corresponds to an _interface_ consisting of a set of methods.

Next, to define the iterator implementation, we implement each of the methods,
bracketed by `object ... end`, declaring each method with the `method` keyword.

```ocaml
   let iterator (list : 'a dlist) : 'a iterator =
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
         | Some elt ->
              current := elt.next;
              remove list elt  (* This is the 'remove' function above *)
         | None -> raise (Invalid_argument "remove")
       method insert_after value =
          match !current with
          | Some elt -> ignore (insert_after elt value)  (* 'insert_after' above *)
          | None -> raise (Invalid_argument "insert_after")
     end
```

The reference cell `current` holds the current position in the list.  The method
`has_value` returns true if `current` refers to an element, `value` returns the
element, and `next` advances the iterator.  The method `remove` unlinks the
`current` element by setting the previous element's `next` pointer, and the
next's elements `previous` pointer, then advancing `current` to the next
element.  The following example illustrates the semantics.

```ocaml
# let () =
    Printf.printf "\nDList2\n";
    let l = create () in
    let _ = insert_first l 1 in
    let _ = insert_first l 2 in
    let _ = insert_first l 3 in

    let it = iterator l in
    while it#has_value do
      Printf.printf "Item: %d\n" it#value;
      it#next
    done;;
Item: 3
Item: 2
Item: 1
- : unit = ()
```

Note that the doubly-linked list is a _cyclic_ data structure.  Most notably,
the builtin equality _does not work_ in general with cyclic values.

```ocaml
# let l2 = create();
val l2 : '_a dlist
# insert_first l2 1; insert_first l2 3;;
- : unit = ()
# l == l2;;
- : bool = false
# l = l2;;
Out of memory during evaluation.
```

### Hash tables with iterators

Let's return to the example of hash tables, but this time let's define an
iterator-style interface.  We'll use a similar `iterator` object type like we did
for doubly-linked lists, but this time the iteration is over key/value pairs.
The signature changes slightly, the main change being that the `find` function
returns an iterator.  This allows retrieval of the value associated with a key,
and it also allows the entry to be deleted.

```ocaml
module Iterable_dictionary : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iterator : ('a, 'b) t -> ('a * 'b) iterator
  val find : ('a, 'b) t -> key:'a -> ('a * 'b) iterator
end
```

The implementation of `Iterable_dictionary` is similar to the original `Dictionary`
using lists, except now we will use doubly-linked lists.  The `create` function
creates an array of doubly-linked lists.  The `add` function first removes any
existing entry, then add the new element to the front of the bucket.

```ocaml
module Iterable_dictionary = struct
  type ('a, 'b) t = ('a * 'b) DList.t array

  let num_buckets = 17
  let hash_bucket key = (Hashtbl.hash key) mod num_buckets

  let create () = Array.init num_buckets (fun _ -> DList.create ())

  let add table ~key ~data =
    let index = hash_bucket key in
    let it = DList.find table.(index) ~data:(key, data) in
    if it#has_value then it#remove;
    DList.insert_first table.(index) (key, value)

  ...
end
```

We can define iterators in the hash table as a pair of a bucket index and
`DList` iterator into the bucket.  To define this as an object, we'll introduce
a few more object concepts, including mutable fields, private methods, and
initializers.  The function `make_iterator table index_ dlist_it_` returns an
iterator for the bucket with index `index_` and list iterator `dlist_it_`.

```ocaml
  let make_iterator table index_ dlist_it_ =
    object (self)
      val mutable index = index_
      val mutable dlist_it = dlist_it_
      method has_value = dlist_it#has_value
      method value = dlist_it#value
      method next =
         dlist_it#next;
         self#normalize
      method remove =
         dlist_it#remove;
         self#normalize
      method private normalize =
        while not dlist_it#has_value && index < num_buckets - 1 do
          index <- index + 1;
          dlist_it <- DList.iterator table.(index)
        done
      initializer self#normalize
    end
```

The iterator implementation relies on a "normal" form, where the list iterator
_always_ refers to an element.  This is handled by the `normalize` method, which
advances past empty buckets until either a non-empty bucket is found, or the end
of the table is reached.

The `normalize` method is declared as `private`, so that it does not appear as
part of the iterator type.  The `has_value` and `value` methods delegate
directly to the list iterator.  The `next` and `remove` methods also delagate to
the list iterator; however, since the iterator has been mutated, the `normalize`
method is called to advance to the next element.

There are several more things to note.  The syntax `object (self) ... end` means
that the variable `self` refers to the object itself, allowing other method in
the object to be called (like `self#normalize`).  The fields `index` and
`dlist_it` are declared as `val mutable`, which means that they can be modified
by assignment using the `<-` syntax seen in the `normalize` method.  Finally,
the object also has an `initializer` expression, which is called when the object
is first created, in this case normalizing the iterator.

Now that the iterator is defined, we can complete the `Iterable_dictionary`
implementation.

```ocaml
  let iterator table =
    make_iterator table 0 (DList.iterator table.(0))

  let find table ~key =
    let index = hash_bucket key in
    let it = DList.iterator table.(index) in
    while it#has_value && fst it#value <> key do
      it#next
    done;
    if it#has_value then
       make_iterator table index it
    else
       make_iterator table num_buckets it
```

The `iterator` function returns in iterator that refers to the first element in
the table (if the table is non-empty).  The `find` function searches for an
element in the table, returning an iterator referring to that value if found, or
else the an iterator at the end of the table.

Iteration over the hash table is much the same as through a doubly-linked list.
Note that the elements are reordered in the hash table.

```ocaml
# let () =
  let module IHM = Iterable_dictionary in
  let table = IHM.create () in
  IHM.add table ~key:"small" ~data:1.00;
  IHM.add table ~key:"medium" ~data:1.50;
  IHM.add table ~key:"large" ~data:2.25;
  IHM.add table ~key:"enormous" ~data:5.00;

  let it = IHM.iterator table in
  while it#has_value do
    let size, price = it#value in
    Printf.printf "Size %s is $%.02f\n" size price;
    it#next
  done;;
Size large is $2.25
Size medium is $1.50
Size small is $1.00
Size enormous is $5.00
- : unit = ()

# let () =
  let it = IHM.find table "enormous" in
  it#remove;

  let it = IHM.iterator table in
  while it#has_value do
    let size, price = it#value in
    Printf.printf "Size %s is $%.02f\n" size price;
    it#next
  done;;
Size large is $2.25
Size medium is $1.50
Size small is $1.00
- : unit = ()
```

## Lazy computation

There are many instances where imperative programming is used to modify or
improve the performance characteristics of a program, without otherwise changing
the behavior.  In other words, the program could be written without
side-effects, but performance is improved by techniques like lazy computation,
caching, memoization, etc.

One of the simplest of these is the builtin lazy computation.  The keyword
`lazy` can be used to prefix any expression, returning a value of type `'a
Lazy.t`.  The computation is delayed until forced with the `Lazy.force`
function, and then saved thereafter.

```ocaml
# let v = lazy (print_string "performing lazy computation\n"; 1);;
val v : int lazy_t = <lazy>
# Lazy.force v;;
performing lazy computation
- : int = 1
# Lazy.force v;;
- : int = 1
```

The builtin `lazy` computation has a nice syntax, but the technique is pretty
generic, and we can implement it with a mutable value.

```ocaml
module ImpLazy : sig
   type 'a t

   val create : (unit -> 'a) -> 'a t
   val force : 'a t -> 'a
end = struct
   type 'a delayed = Delayed of (unit -> 'a) | Value of 'a
   type 'a t = 'a delayed ref

   let create f = ref (Delayed f)
   let force v =
     match !v with
     | Delayed f ->
          let x = f () in
          v := Value x;
          x
     | Value x ->
          x
end;;
```

The `'a delayed` type contains a delayed value represented as a function, or
else an actual value.  The `ImpLazy.force` function forces the computation; if
it is delayed, the function is evaluated, and the value is mutated to save the
resulting value.  Subsequent calls to `ImpLazy.force` will fall into the `Value`
case, without needing to reevaluate the function.  The main difference between
our module `ImpLazy` and the builtin module `Lazy` is the nice syntax for the
latter.  Rather than writing `ImpLazy.create (fun () -> e)`, the builtin syntax
is just `lazy e`.

### Memoization

We can generalize lazy computations to function _memoization_, where we save the
result of function applications to avoid their recomputation.  One simple
implementation is to use a hash table to save the values by side effect.

```ocaml
module Memo : sig
   type ('a, 'b) t

   val create : unit -> ('a, 'b) t
   val apply : ('a, 'b) t -> func:('a -> 'b) -> arg:'a -> 'b
end = struct
   type ('a, 'b) t = ('a, 'b) Dictionary.t

   let create = Dictionary.create
   let apply table ~func ~arg =
     match Dictionary.find table ~key:arg with
     | Some x -> x
     | None ->
         let x = func arg in
         Dictionary.add table ~key:arg ~data:x;
         x

end;;
```

Memoization is useful for _dynamic programming_, where problems are solved by
breraking them down into simpler subproblems.  If subproblems occur more than
once, memoization can be used to avoid recomputing the subproblem.  A canonical
example of this is the Fibonacci sequence, which is defined by the following
program, which produces the sequence _0, 1, 1, 2, 3, 5, 8, 13, 21, ..._ starting
from 0.

```ocaml
let rec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2);;
```

The complexity of this function is exponential _O(2^i)_, because for large
inputs the function computes two similar-sized subproblems.  To illustrate,
let's time the computation using the `Sys.time` function to measure the wall
clock.

```ocaml
# let time f x =
    let start = Sys.time () in
    let y = f x in
    Printf.printf "Time: %g sec\n" (Sys.time () -. start);
    y;;
val time : ('a -> 'b) -> 'a -> 'b = <fun>
# time fib 40;;
Time: 5.53724 sec
- : int = 102334155
```

Next, let's construct a memoized version of the function, where the recursive
calls are made through a memo table.  This makes a dramatic improvement in
performance.  Since the recursive calls are computed just once, the complexity
is linear, and the computation is fast.

```ocaml
# let memo_fib =
    let memo = Memo.create () in
    let rec fib i =
      if i <= 1 then
         i
      else
         Memo.apply memo ~func:fib ~arg:(i - 1) +
         Memo.apply memo ~func:fib ~arg:(i - 2)
    in
    fib;;
val memo_fib : int -> int = <fun>
# time memo_fib 40;;
Time: 3.7e-05 sec
- : int = 102334155
```

Note that this use of memoization relies on side-effects to cache intermediate
computations, but it doesn't change the values of the function.  Its purpose is
to improve performance.

### Hash consing

"Hash consing" is a technique to share values that are structurally equal.  The
term comes from Lisp, where the technique is used to share s-expressions that
are equal.  In some situations, hash-consing can result in dramatic performance
improvements in two ways.  First, space can be reduced by using a single
physical representation for values that are equal.  Second, values can be
compared for equality using the constant-time physical equality operator `==`.

One of the simplest ways to implement hash-consing is to use a hash-table to
remember (memoize) values that have already been created.  To illustrate, let's
define a kind of numerical expression `Exp.t` consisting of integers, variables,
addition, and multiplication (we can define more operators, but let's keep the
example simple).

The type `Exp.t` is declared as `private`, meaning that pattern matching can be
used on the expressions outside the module, but expressions can't be constructed
without explicitly using the constructors `num`, `var`, `plus`, `times` provided
by the `Exp` module.  These functions enforce the hash-consing, ensuring that
structurally equal expressions are mapped to physically equal representations.

```ocaml
module Exp : sig
  type t = private
  | Num of int
  | Var of string
  | Plus of t * t
  | Times of t * t

  val num : int -> t
  val var : string -> t
  val plus : t -> t -> t
  val times : t -> t -> t
end = struct
  type t =
  | Num of int
  | Var of string
  | Plus of t * t
  | Times of t * t

  let table = Dictionary.create ()
  let merge exp =
    match Dictionary.find table ~key:exp with
    | Some x -> x
    | None ->
         Dictionary.add table ~key:exp ~data:exp;
         exp

  let num i = merge (Num i)
  let var s = merge (Var s)
  let plus e1 e2 = merge (Plus (e1, e2))
  let times e1 e2 = merge (Times (e1, e2))
end;;
```

The implementation defines a hash table `table`, and a `merge` function that
merges an expression into the table, returning the previous value if there was
one, or inserting a new value if there is not.  The constructors can rely on the
fact that subexpressions have already been hash-consed, so they simply call the
merge function to memoize the value.

Note that expressions that are structurally equal are now also physically equal.

```ocaml
# let e1 = Exp.times (Exp.num 10) (Exp.plus (Exp.var "x") (Exp.var "y"));;
val e1 : Exp.t = Exp.Times (Exp.Num 10, Exp.Plus (Exp.Var "x", Exp.Var "y"))
# let e2 = Exp.times (Exp.num 10) (Exp.plus (Exp.var "x") (Exp.var "y"));;
val e2 : Exp.t = Exp.Times (Exp.Num 10, Exp.Plus (Exp.Var "x", Exp.Var "y"))
# e1 == e2;;
- : bool = true
```

Expressions that are not equal are equal are not physically equal either,
however common subexpressions are equal.

```ocaml
# let e3 = Exp.times (Exp.num 10) (Exp.plus (Exp.var "z") (Exp.var "y"));;
val e3 : Exp.t = Exp.Times (Exp.Num 10, Exp.Plus (Exp.Var "z", Exp.Var "y"))
# e1 == e3;;
- : bool = false
# let Exp.Times (a1, Exp.Plus (_, a2)) = e1;;
val a1 : Exp.t = Exp.Num 10
val a2 : Exp.t = Exp.Var "y"
# let Exp.Times (b1, Exp.Plus (_, b2)) = e3;;
val b1 : Exp.t = Exp.Num 10
val b2 : Exp.t = Exp.Var "y"
# a1 == b1;;
- : bool = true
# a2 == b2;;
- : bool = true
```

### Weak hash consing

There is two issues with hash-consing as we have just defined it.  A minor
problem is that hashing is linear in the size of the expression.  This can be
fixed by storing the hash code in the expression itself, avoiding the recursive
computation.  If expressions are small, this won't be much of a benefit, but it
can save time if large expressions are frequently constructed.

A more serious problem is that the hash table holds onto expressions _forever_,
even if they are no longer used in the program.  This can result in a space leak
that cancels out any space saving we had in the first place, perhaps making it
even worse.

To deal with this problem, we can use "weak" hash tables, implemented in the
`Weak` module in the OCaml standard library.  The main difference is that a weak
table may drop values that are no longer being used elsewhere.  Weak tables are
tied into the garbage collector, which removes values that are no longer live.

We define the type `WExp.t` much as before, except including the hash code for
the `Plus` and `Times` expressions.

The weak hash table requires that hash and equality functions be provided
explicitly, so we construct a module `HashExp` that defines the `equal` and
`hash` functions.  Note that equality and hashing are both constant-time
functions -- equality can rely on physical equality of subexpressions, and
hashing can use the explcitly represented hash values.

The module `WeakHash` has the semantics of a set of elements.  The
`WeakHash.merge` function retrieves an element if it already exists, or adds it
otherwise.  The constructors are much as before.

```ocaml
module WExp : sig
  type t = private
  | Num of int
  | Var of string
  | Plus of int * t * t
  | Times of int * t * t

  val num : int -> t
  val var : string -> t
  val plus : t -> t -> t
  val times : t -> t -> t
end = struct
  type t =
  | Num of int
  | Var of string
  | Plus of int * t * t
  | Times of int * t * t

  module HashExp = struct
    type exp = t
    type t = exp
    let equal e1 e2 =
      match e1, e2 with
      | Num i1, Num i2 -> i1 = i2
      | Var v1, Var v2 -> v1 = v2
      | Plus (_, a1, a2), Plus (_, b1, b2)
      | Times (_, a1, a2), Times (_, b1, b2) ->
           a1 == b1 && a2 == b2
      | _ -> false
    let hash = function
    | Num i -> i lxor 0xabababab
    | Var v -> (Hashtbl.hash v) lxor 0xcdcdcdcdc
    | Plus (hash, _, _)
    | Times (hash, _, _) -> hash
  end

  module WeakHash = Weak.Make (HashExp);;

  let table = WeakHash.create 17
  let merge e = WeakHash.merge table e

  let num i = merge (Num i)
  let var s = merge (Var s)
  let plus e1 e2 =
     let hash = (HashExp.hash e1) lxor (HashExp.hash e2) lxor 0x12345678 in
     merge (Plus (hash, e1, e2))

  let times e1 e2 =
     let hash = (HashExp.hash e1) lxor (HashExp.hash e2) lxor 0xdeadbeef in
     merge (Times (hash, e1, e2))
end;;
```

## Concurrency

Concurrency is another tool OCaml programmers can use to simplify
programs in certain cases.  In OCaml, threads do not introduce
parallelism; only one thread may be running at a time.  However,
threads can be used to simplify control flow -- a program can devote
some threads to reading input, others for performing work, others for
producing output, etc.

The OCaml standard library supports threads, where individual threads
of control can be created that run concurrently (but only one thread
at a time), and context switches are _involuntary_, meaning that a
thread may be preempted at any time.

When threads share imperative state, this gives rise to the standard
synchronization issues, where multiple threads may be mutating shared
state at the same time.  To illustrate, let's write a program with two
threads that increment a shared reference cell concurrently.

```ocaml
let value = ref 0

let loop () =
   for i = 1 to 3 do
     let i = !value in
     Printf.printf "i = %d\n" i;
     flush stdout;
     value := i + 1
   done

let thread1 = Thread.create loop ();;
let thread2 = Thread.create loop ();;

Thread.join thread1;;
Thread.join thread2;;

Printf.printf "value = %d\n" !value
```

The reference cell `value` holds a shared value that is incremented 10
times, in a loop, by the `loop` function.  Each iteration of the loop
prints the current value, then assigns the new value.  We create two
threads with the `Thread.create` expressions, then use `Thread.join`
to block until the threads terminate.

The exact behavior of the program is nondeterminstic -- it depends on
the relative sopeed of the two theads.  One output is listed below.

```ocaml
i = 0
ii  ==  01

ii  ==  12

i = 2
value = 3
```

This represents a nearly perfect interleaving of the thread
executions.  Each thread fetches the value from the `value` reference
cell, prints the value, then performs the assignment.  Since both
threads effectively run in lockstep, the final value in the `value`
reference cell is the same as if there were just one thread running.

If this is not the behavior that was expected, one solution is to use
a `Mutex` to ensure that the increment operation is atomic.  We can
allocate a lock with `Mutex.create`, then acquire the lock in the loop
body.

```ocaml
let value = ref 0
let mutex = Mutex.create ()

let loop () =
   for i = 1 to 3 do
     Mutex.lock mutex;
     let i = !value in
     Printf.printf "i = %d\n" i;
     flush stdout;
     value := i + 1;
     Mutex.unlock mutex
   done
```

When we run this program, it produces a deterministic output.

```
i = 0
i = 1
i = 2
i = 3
i = 4
i = 5
value = 6
```

### Dealing with concurrency

In general the interaction of concurrency with imperative programs causes
problems with races.  There are many techniques you can use to address the
issue.

* Do not use assignment, mutable data structures, or perform
  input/output in threads.

* Use _cooperative_ multitasking, where only one thread runs at a
  time, and context switches are _voluntary_.  This is the dominant
  model in `Async`.

* Do not share mutable data between threads.  In practice, this
  usually includes explicit communication channels between threads
  that otherwise have isolated state.

* Give in, and use threads, and the standard synchronization toolkit
  that comes with OCaml, including locks (`Mutex`), condition
  variables, etc.

Out of all of these choices, the simplest one is to use the `Async`
model and cooperative multitasking.  However, let's go ahead and work
through some examples of using traditional concurrent programming
using locks and other synchronization primitives to build a
concurrency library.

### Concurrent hash tables

Let's extend our hash table example to support concurrency.  To begin,
let's first give the signature of the module we will implement.  The
table has operations to add, remove, and find elements, and it also
supports imperative iterators.

```ocaml
module Concurrent_dictionary : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b option
  val remove : ('a, 'b) t -> key:'a -> unit
  val iterator : ('a, 'b) t -> ('a * 'b) iterator
end
```

We'll use the same basic construction that we used to implement the `Dictionary`,
where a hash table contains an array of buckets.  In addition we'll add locking
to ensure that concurrent operations do not interfere.  To reduce lock
contention, we'll use an array of locks to partition the table into multiple
parts.  If operations are uniformly distributed, this should reduce lock
contention.

```ocaml
  type ('a, 'b) element = {
    key : 'a;
    mutable value : 'b
  }
  type ('a, 'b) t = {
    locks : Mutex.t array;
    mutable buckets : ('a, 'b) element list array
  }

  let num_locks = 32
  let num_buckets = 256

  let create () = {
    locks = Array.init num_locks (fun _ -> Mutex.create ());
    buckets = Array.create num_buckets []
  }
```

Each `element` is a key/value pair, where the value is mutable so that
the `add` function can mutate it in place.  For this implementation,
we'll use 32 locks, and start with 256 buckets.

Each bucket is an _association list_, meaning that it is list of key/value pairs
that implement a dictionary.  We can start the implementation by defining
dictionary operations for association lists.  The function `find_assoc` finds
the value associated with a key, and `remove_assoc_exn` removes an association.
The remove function raises an exception `Not_found` if the list does not contain
the association.  We'll use this to optimize removal.

```ocaml
  let rec find_assoc key = function
  | element :: tl ->
       if element.key = key then
          Some element
       else
          find_assoc key tl
  | [] -> None

  let rec remove_assoc_exn key = function
  | element :: tl ->
       if element.key = key then
          tl
       else
          element :: remove_assoc_exn key tl
  | [] -> raise Not_found
```

The locks are intended to partition the table into multiple sub-parts, where
each lock provides synchronization for a contiguous range of buckets.  For
synchronization we define a function `synchronize` that takes a bucket index and
a function, and evaluates the function with the bucket lock acquired, releasing
the lock before returning.

```ocaml
  let synchronize table index f =
    let lock = table.locks.(index * num_locks / num_buckets) in
    Mutex.lock lock;
    let result = f () in
    Mutex.unlock lock;
    result
```

Note that the `synchronize` function is _not_ exception-safe, meaning
that if evaluation of `f ()` raises an exception, the lock will not be
released.  An exception-safe version would catch all exceptions; when
an exception is raised, the lock would be released, and the exception
re-raised.

```ocaml
  let synchronize_exn table index f =
    let lock = table.locks.(index * num_locks / num_buckets) in
    Mutex.lock lock;
    try let result = f () in Mutex.unlock lock; result with
      exn -> Mutex.unlock lock; raise exn
```

To add a new entry to the table, the `add` function acquires the
bucket lock, then uses `find_assoc` to look for an existing
association.  If one is found, the `value` is updated in-place to the
new value.  Otherwise, a new entry is added to the beginning of the
bucket.

```ocaml
  let add table ~key ~data =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    let buckets = table.buckets in
    synchronize table index (fun () ->
          match find_assoc key buckets.(index) with
          | Some element ->
               element.value <- data
          | None ->
               buckets.(index) <- { key = key; value = data } :: buckets.(index))
```

Removing an element from the table is similar.  If here is a previous entry in
the table, the entry is removed.  Otherwise, the `remove_assoc_exn` function
raises `Not_found`, and we leave the bucket unchanged.  The exception is an
optimization to avoid copying the entire list in this case.

```ocaml
  let remove table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    let buckets = table.buckets in
    synchronize table index (fun () ->
      try buckets.(index) <- remove_assoc_exn key buckets.(index) with
      | Not_found -> ())
```

The function to find an association in the table is similar -- we jsut
find the entry in the table and return the value part.  However, this
particular implementation is somewhat more subtle, because it omits
the synchronization step, examining the bucket _without_ acquiring the
lock.

```ocaml
  let find table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    (* Unsynchronized *)
    match find_assoc key table.buckets.(index) with
    | Some element -> Some element.value
    | None -> None
```

From a performance perspective, this is clearly a win, because
retrieving elements from the table has no locking at all.  But why is
it valid?

The reasoning has to do with two things: 1) the semantics we expect
from the table, and 2) the OCaml memory model.  Ideally, the semantics
we would expect is _sequential semantics_, meaning that all memory
operations are processed in _some_ sequence that is compatible with
the order in which they were performed in each thread.  Thus, if some
thread adds two entries for keys `K1` and `K2` in sequential order,
then all other threads will see either, 1) neither entry, or 2) a
entry for `K1`, or 3) a entry for both `K1` and `K2`, but it will
_not_ see an entry for `K2` without also having an entry for `K1`.

Unfortunately, for some processor architectures, primary memory does
not have sequential semantics, due to caching and other effects.
Fortunately for us, OCaml does provide sequential memory semantics due
to its threading model where: 1) only one thread executes at a time,
and 2) there is a memory barrier the prevents reordering of thread
context switches and memort operations, and 3) the compiler does not
reorder memory operations in ways that would violate sequential memory
semantics.

Note, OCaml does not a _guarantee_ this semantics.  The OCaml
implementation may change to support parallelism.  If it does, the
memory semantics will change accordingly.  The simplest fix is just to
synchronize the access.  Performance of `find` operations will
decrease somewhat due to contention.

```ocaml
  let synchronized_find table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    synchronize table index (fun () ->
      (find_assoc key table.buckets.(index)).value)
```

For the final part of the implementation, let's define imperative
iteration.  The iterator object contains a bucket index, and the field
`elements` refers to some suffix of the list stored in the bucket.
The `value` method returns the current elements, and the `next` method
advances the `elements` field.  The method `normalize` is used to
maintain the invariant that the `elements` field always refers to a
value in the table unless the iterator has advanced past the final
element.  The `remove` method removes the current element from the
bucket in which it is stored.

```ocaml
  let rec remove_element_exn elements = function
  | (hd :: tl) as elements' ->
       if elements' == elements then
          tl
       else
          hd :: remove_element_exn elements tl
  | [] -> raise Not_found

  let iterator table =
    let buckets = table.buckets in
    object (self)
      val mutable index = 0
      val mutable elements = buckets.(0)
      method has_value = elements <> []
      method value =
        match elements with
        | { key = key; value = value } :: _ -> key, value
        | [] -> raise (Invalid_argument "value")
      method next =
        elements <- List.tl elements;
        self#normalize
      method remove =
        synchronize table index (fun () ->
          try buckets.(index) <- remove_element_exn elements buckets.(index) with
            Not_found -> ());
        self#next
      method private normalize =
        while elements = [] && index < num_buckets do
          index <- index + 1;
	  elements <- buckets.(index)
        done
      initializer self#normalize
    end
```

The iterator methods are all unsychronized except the method `remove`, which
mutates the bucket.  As a consequence, it means that hash operations that add
and remove elements from the list can happen concurrently with iteration.
Again, this is great from a performance perspective, but it means that iteration
has non-sequential semantics.  In particular, whenever iteration enters a new
bucket, subsequent concurrent operations that add new elements or remove old
ones from that bucket have _no effect_ on the iteration.  Iteration advances
through that bucket as if it were unchanged.

One advantage of this relaxed iteration semantics is peformance, since
iteration is largely unsynchronized.  Another advantage is that
deadlock is less likely.  If we were to _lock_ the bucket during
iteration, then changes to that bucket would not be allowed during
iteration (even by the iterating thread).  We might allow lock
recursion to allow mutations by the iterating thread, but in general
the synchronization might involve multiple threads, resulting in
deadlock.  Lock-free iteration ensures that the
`Concurrent_dictionary` will not be involved in a deadlock cycle.

## Input and output (fix this chapter heading)

Input and output I/O is another kind of imperative operation, where the purpose
is to either read input from a file, stream, or device, _consuming_ the input by
side-effect; or write output to a file, stream, or device, _modifying_ the
output by side-effect.  There are several I/O libraries in OCaml.  There is the
basic builtin I/O library in the `Pervasives` module, and there are moe advanced
I/O libraries in the `Unix` and `Async` modules.  Let's look at the basic
library -- the advanced libraries will be described elsewhere.

For basic I/O OCaml models input and output with _channels_.  An `in_channel` is
used for reading input, and and `out_channel` for producing output.  Each OCaml
process has three standard channels, similar to the three standard files in Unix.

* `stdin : in_channel`.  The "standard input" channel.  By default, input comes
  from the terminal, which handles keyboard input.

* `stdout : out_channel`.  The "standard output" channel.  By default, output
  written to `stdout` appears on the user terminal.

* `stderr : out_channel`.  The "standard error" channel.  This is similar to
  `stdout`, but it is intended for error messages.

The standard library has several functions for reading from and writing to the
standard channels.

* `print_char : char -> unit` prints a single character to `stdout`.

* `print_string : string -> unit` prints a `string` to `stdout` as a sequence of
  characters, without quotes.

* `print_int : int -> unit` prints an integer to `stdout`.

* `print_float : float -> unit` prints a floating-point number to `stdout`.

Functions to write to `stderr` have similar names, using the prefix `prerr_`
rather than `print_` (for example, `prerr_string`).

```ocaml
# let i = 1;;
val i : int = 1
# print_string "The value of i is "; print_int i; print_string ".\n";;
The value of i is 1.
- : unit = ()
```

Input is similar, but there are only a few functions.

* `read_line : unit -> string` reads a line of input from `stdin`.

* `read_int : unit -> int` reads a decimal integer from `stdin`.

* `read_float : unit -> float` reads a floating-point number from `stdin`.

These functions raise the exception `End_of_file` if the input channel is
terminated before the input is read.

Here is a function to read a sequence of integers from `stdin`, sorting them and
printing the result to `stdout`.

```ocaml
# let collate_input () =
   let rec read_input items =
      let item = try Some (read_int ()) with End_of_file -> None in
      match item with
       | Some i -> read_input (i :: items)
       | None -> items
   in
   let items = read_input [] in
   let sorted = List.sort (-) items in
   List.iter (fun i -> print_int i; print_char ' ') sorted;
   print_string "\n";;
val collate_input : unit -> unit = <fun>
#   collate_input ();;
8
56
2
34
-120
19
-120 2 8 19 34 56
- : unit = ()
```

Using exceptions to signal the end of input is a little awkward.  The
`read_input` loop above converts the exception into an `int option`, so that
matching can be performed with `match` and the function is tail-recursive.  The
following function is _not_ tail recursive, and should be avoided.

```ocaml
  (* AVOID -- non tail-recursive input reader *)
# let rec read_input items =
   try read_input (read_int () :: items) with
      End_of_file -> items;;
val read_input : int list -> int list = <fun>
# read_input [];;
34
45
56
-1
- : int list = [-1; 56; 45; 34]
```

Another way to address the input issue is to use iteration, rather than
recursion.  This requires collecting the input in a container than can be
mutated by side-effect.

```ocaml
# let read_input () =
   let items = ref [] in
   try
      while true do
         items := read_int () :: !items
      done;
      []  (* not reached *)
   with End_of_file ->
      !items;;
val read_input : unit -> int list = <fun>
# read_input ();;
45
78
-345
- : int list = [-345; 78; 45]
```

In this loop, the value returns from a successful call to `read_int ()` is added
to the `items` list by side-effect.  When `read_int ()` reaches the end of
input, it raises the `End_of_file` exception, terminating the loop.  The `while`
loop never terminates; the `[]` value afterward is only to satisfy the type
checker.

### Basic file input and output

It isn't necessary to perform all input through the standard channels.  There
are also functions to open files for reading and writing.

* `open_out : string -> out_channel` open a file for writing.

* `open_in : string -> in_channel` open a file for reading.

* `close_out : out_channel -> unit` close an output channel, flushing any pending
  output to the file.

* `close_in : in_channel -> unit` close an input channel.

The functions for input and output are similar to the `print_...` and `read_...`
functions, but they use the prefix `output_` and `input_` and they take a
channel as an argument.  Let's write a function to print the contents of a file.

```ocaml
# let cat filename =
   let inx = open_in filename in
   try
      while true do
         print_string (input_line inx); print_char '\n'
      done
   with End_of_file ->
      close_in inx;;
val cat : string -> unit = <fun>
# let outf = open_out "file.txt";;
val outf : out_channel = <abstr>
# output_string outf "Hello world\n1\n2\n3\n";;
- : unit = ()
# close_out outf;;
- : unit = ()
# cat "file.txt";;
Hello world
1
2
3
- : unit = ()
```

It is important to close channels when you are finished with them, for two
reasons.  One reason is that the runtime system will usually have a limit on the
number of channels that may be open simultaneously.  To avoid problems, you
should close channels you are not using.  In addition, for efficiency, output is
buffered and written to output channels in larger blocks.  The output may not
actually be written to the file unless the `out_channel` is closed, or with an
explicit call to the `flush` function.

* `flush : out_channel -> unit`

```ocaml
# let outf = open_out "file.txt";;
val outf : out_channel = <abstr>
# output_string outf "Hello world\n1\n2\n3\n";;
- : unit = ()
# cat "file.txt";;
- : unit = ()
# flush outf;;
- : unit = ()
# cat "file.txt";;
Hello world
1
2
3
- : unit = ()
```

### Formatted output with Printf

Output with the standard library functions like `output_char`, `output_int`,
`output_string`, etc. is simple, but there is little flexibility, and it is
often verbose.  OCaml also supports _formatted_ output using the `printf`
function, which is modeled after `printf` in the C standard library.  The
`printf` function takes a _format string_ that describe what to print and how to
format it, as well as arguments to be printed.

```ocaml
printf format arg1 ... argN
```

The format string can contain character literals, which are printed without
change, as well as conversion specifications, which specify how to print an
value that is provided as an argument.  A conversion specification begins with a
percent (`%`) character, some flags, and a type specification.  For example,
`%d` is a conversion specification for printing an integer, `%s` prints a
string, and `%f` prints a floating-point value.

```ocaml
# open Printf;;
# printf "int=%d string=%s float=%f\n" 10 "abc" 1e5;;
int=10 string=abc float=100000.000000
- : unit = ()
```

Conversions can also specify additional parameters for formatting the value.
The general form has flags, width specified in number of characters, and decimal
precision used for printing floating-point values.  These parameters are
optional.

```ocaml
% [flags] [width] [.precision] type
```

There are several flags of interest, the flag `'-'` left-justifies the output,
and the flag `'0'` pads numerical values with leading zeroes.
Let's write a program to print a price list in a tabular form.

```ocaml
# let print_prices prices =
    print_string "--------------------------------\n\
                  | Size   | Hexcode    | Price  |\n\
                  --------------------------------\n";
    List.iter (fun (size, code, price) ->
          printf "| %-6s | 0x%08x | %6.2f |\n" size code price) prices;
    print_string "--------------------------------\n";;
val print_prices : (string * int * float) list -> unit = <fun>
# print_prices
   [("small", 0x35, 1.02);
    ("medium", 0x7a1, 50.75);
    ("large", 0xbad, 400.8);
    ("vente", 0x11, 4136.);
    ("enormous", 100000, 100.)];;
--------------------------------
| Size   | Hexcode    | Price  |
--------------------------------
| small  | 0x00000035 |   1.02 |
| medium | 0x000007a1 |  50.75 |
| large  | 0x00000bad | 400.80 |
| vente  | 0x00000011 | 4136.00 |
| enormous | 0x000186a0 | 100.00 |
--------------------------------
- : unit = ()
```

The format specification `"| %-6s | 0x%08x | %6.2f |\n"` specifies that the
first argument is a string that is to be left-justified and printed six
characters wide; the second argument is an integer that should be printed in
hexidecimal (format type `x`) eight characters wide with leading zeroes; and the
third argument is a floating point value that should be printed right-justified,
six characters wide, with two digits after the decimal point.

Note that the width specifies a _minimum_ width.  If the value requires more width to print completely the width is increased.  The price `4136.00` and the size `enormous` both overflow the width, breaking the tabular form.

### Strong typing and format strings

In OCaml, `printf` is type safe.  The format is checked against the arguments at
compile time, and rejected if the format string is malformed, or if the format
does not match the arguments.  In the following examples, `%v` is not a valid
conversion specification, and floating-point values can't be printed with a
decimal conversion specification.

```ocaml
# printf "Hello %v" 1;;
Characters 7-17:
  printf "Hello %v" 1;;
         ^^^^^^^^^^
Error: Bad conversion %v, at char number 6 in format string ``Hello %v''
# printf "Hello %d" 1.;;
Characters 18-20:
  printf "Hello %d" 1.;;
                    ^^
Error: This expression has type float but an expression was expected of type
         int
```

A consequence of strong typing is that the format string must ultimately be a
string _literal_, known at compile time.  It can't be computed by the program at
runtime.

```ocaml
# let fmt = "%s";;
val fmt : string = "%s"
# printf fmt "Hello";;
Characters 7-10:
  printf fmt "Hello";;
         ^^^
Error: This expression has type string but an expression was expected of type
         ('a -> 'b, out_channel, unit) format =
           ('a -> 'b, out_channel, unit, unit, unit, unit) format6
```

Actually, this is not entirely true.  The `printf` function takes a format
string of type `('a, 'b, 'c) format` and it is only the type conversion from
`string` to `format` where the string is required to be a literal.

```ocaml
# let fmt : ('a, 'b, 'c) format = "Size: %s.\n";;
val fmt : (string -> 'c, 'b, 'c) format = <abstr>
# printf fmt "small";;
Size: small
- : unit = ()
```

### Output to channels, strings, and buffers

The function `printf` prints to the standard output channel `stdout`.  There are alse several variations.

* `eprintf format arg1 ... argN`.  Print to the standard error channel `stderr`.

* `fprintf channel format arg1 ... argN`.  Print to the channel `channel`.

* `sprintf format arg1 ... argN`.  Produce a string as output.

* `bprintf buffer format arg1 ... argN`.  Print to a string buffer
  `buffer` of type `Buffer.t`.

```ocaml
# sprintf "The number is %10d." 123;;
- : string = "The number is        123."
# let buf = Buffer.create 32;;
val buf : Buffer.t = <abstr>
# bprintf buf "%s\n" "First line";;
- : unit = ()
# bprintf buf "%20s.\n" "Second line";;
- : unit = ()
# Buffer.contents buf;;
- : string = "First line\n         Second line.\n"
```

### Formatted input with Scanf

The `scanf` function can be used for reading input, similar to
`printf`.  The general form takes a format string and a "receiver"
function that is applied to the input.  The format string contains
literal characters, which must be read literally, and conversion
specifications similar to `printf`.  The result of the receiver
function is returned as the result of `scanf`.

```ocaml
# scanf "%d" (fun i -> i);;
1871
- : int = 1871
```

For another example, consider the parsing a date string that might be
in one of two forms, _month day, year_ or _month/day/year_.  The
`scanf` functions raise the exception `Scan_failure` on error, so we
can read a date string by trying both kinds of format.

```ocaml
# type month = String of string | Int of int;;
type month = String of string | Int of int
# let scan_date_string s =
    try
      sscanf s "%s %d, %d" (fun month day year ->
             year, String month, day)
    with Scan_failure _ | End_of_file ->
      sscanf s "%d/%d/%d" (fun month day year ->
             year, Int month, day);;
val scan_date_string : string -> int * month * int = <fun>
# scan_date_string "May 3, 1921";;
- : int * month * int = (1921, String "May", 3)
# scan_date_string "10/11/2001";;
- : int * month * int = (2001, Int 10, 11)
# scan_date_string "May 3";;
Exception:
Scanf.Scan_failure
 "scanf: bad input at char number 0: ``character 'M' is not a decimal digit''".
```

## Summary

The OCaml language supports a fairly standard imperative programming model, with
looping, assignment, mutable arrays, records, and objects.  If desired, we can
write programs that correspond directly to what we would have written in some
other imperative language like C or Java.  Of course, doing so is really not the
best match -- if you want to write imperative programs, you should probably use
an imperative programming language.

However, there are times when imperative programming might provide efficiency
(as with lazy evaluation, or memoization), or you might require techniques or
data structures that are traditional imperative (like graphs represented with
adjacency lists), and in these cases OCaml usually shines.  Used with
discretion, imperative programming can lead to smaller, simpler programs.
