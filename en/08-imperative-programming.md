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

```ocaml
(* file: dictionary.ml *)
open Core.Std

type ('a, 'b) t = { mutable length: int;
                    buckets: ('a * 'b) list array;
                  }
```

Our first step is to define the type of a dictionary as a record with
two fields.  The first field, `length` is declared as mutable.  In
OCaml, records are immutable by default, but individual fields are
mutable when marked as such.  The second field, `buckets`, is
immutable, but contains an array, which is itself a mutable data
structure, as we'll see.

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

Now we define the function for choosing a hash-bucket based on the
key.  We do this using `Hashtbl.hash` to compute hash values.
`Hashtbl.hash` is a special function provided by the OCaml runtime
that can compute a hash for almost any OCaml value.  It's type is `'a
-> int`, so it can be applied to a value of any type.  (While it can
be applied to any type, it won't succeed for all values.
`Hashtbl.hash` will throw an exception if it encounters a value it
can't handle, like a function or a value from a C libraries that lives
outside the OCaml heap.)

There's also code for `create`, which creates an empty dictionary,
`length`, which grabs the length from the corresponding record field,
and `find`, which looks for a matching key in the table using
`List.find_map` on the corresponding bucket.  (`List.find_map` takes a
list, and a function for transforming the list elements to options,
returning a list of the contents of the returned `Some`s.)  In `find`,
you'll notice that we make use of the `array.(index)` syntax for
looking up a value in an array.

```ocaml
let iter t ~f =
  for i = 0 to Array.length t.buckets - 1 do
    List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
  done
```

`iter` is designed to walk over all the entries in the dictionary.  In
particular, `iter d ~f` will call `f` for each key/value pair in
dictionary `d`.  Note that `f` is expected to return `unit`, since it
is expected to work by side effect rather than by returning a value.

The code for `iter` uses two forms of iteration: a `for` loop to
iterate over the array of buckets; and within that loop, and a call to
`List.iter` to walk over the list of values in a given bucket.  `for`
loops are not of fundamental importance to the language: instead of
using `for`, we could have implemented the outer loop using the
`Array.iter`, which in turn could be implemented as a recursive
function.  But `for` is syntactically convenient, and is often more
familiar and idiomatic when coding imperatively.

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

The above code is for adding and removing mappings from the
dictionary.  This section is made more complicated by the fact that we
need to detect whether we are overwriting or removing an existing
binding, so we can decide whether `t.length` needs to be changed.  The
helper function `bucket_has_key` is used for this purpose.

We use the `<-` operator for updating elements of an array (`array.(i)
<- expr`) and for updating a record field (`record.field <-
expression`).

We also use a single semicolon, `;`, as a sequencing operator, to
allow us to do two side-effecting operations in a row: first, update
the bucket, then update the count.  We could have done this using let
bindings:

```ocaml
    let () = t.buckets.(i) <- filtered_bucket in
    let () = t.length <- t.length - 1 in
```
but `;` is more idiomatic.

In general, when a sequence expression `expr1; expr2` is evaluated,
`expr1` is evaluated first, and then `expr2`.  The expression `expr1`
must have type `unit`, and the the value of `expr2` is returned as the
value of the entire sequence.  For example, the sequence `print_string
"hello world"; 1 + 2` first prints the string `"hello world"`, then
returns the integer `3`.

It's also worth noting that `;` is a _separator_, not a terminator as
it is in C or Java.  The compiler is somewhat relaxed about parsing a
terminating semicolon, so it may work for you, but you should not rely
on it.  Here is an example where we're using `;` as if it were a
terminator

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

Note also that we do all of the side-effecting operations at the very
end of each function.  This is good practice because it minimizes the
chance that such operations will be interrupted with an exception,
leaving the data structure in an inconsistent state.

## Primitive mutable data

We've already encountered two different forms of mutable data: records
with mutable fields, and arrays.  These are two of the building blocks
of imperative programming in OCaml.  We discuss those, and a few
others, below.

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
* `!refcell` returns the contents of the reference cell.
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

Another common imperative data structure is the doubly-linked list.
Doubly-linked lists can be traversed in both directions and elements
can be added and removed from the list in constant time.  Core defines
a doubly-linked list (the module is called `Doubly_linked`) which is a
good choice for real work, but we'll define our own as an
illustration.

Here's the `mli` of the module we'll build.


```ocaml
(* file: dlist.mli *)
open Core.Std

type 'a t
type 'a element

(** Basic list operations  *)
val create   : unit -> 'a t
val is_empty : 'a t -> bool

(** Navigation using [element]s *)
val first : 'a t -> 'a element option
val next  : 'a element -> 'a element option
val prev  : 'a element -> 'a element option
val value : 'a element -> 'a

(** Whole-data-structure iteration *)
val iter    : 'a t -> f:('a -> unit) -> unit
val find_el : 'a t -> f:('a -> bool) -> 'a element option

(** Mutation *)
val insert_first : 'a t -> 'a -> 'a element
val insert_after : 'a element -> 'a -> 'a element
val remove : 'a t -> 'a element -> unit
```

Note that there are two types defined here: `'a t`, the type of a
list, and `'a element`, the type of an element.  Elements act as
pointers to the interior of a list, and allow us to navigate the list
and give us a point at which to apply mutating operations.

Now let's look at the implementation.  We'll start by defining `'a
element` and `'a t`.

```ocaml
(* file: dlist.ml *)
open Core.Std

type 'a element =
  { value : 'a;
    mutable next : 'a element option;
    mutable previous : 'a element option
  }

type 'a t = 'a element option ref
```

An `'a element` is a record containing the value to be stored
in that node as well as optional (and mutable) fields pointing to the
previous and next elements.  At the beginning of the list, the `prev`
field is `None`, and at the end of the list, the `next` field is
`None`.


The type of the list itself, `'a t`, is an optional, mutable reference
to an `element`.  This reference is `None` if the list is empty, and
`Some` otherwise.

Now we can define a few basic functions that operate on lists and
elements.

```ocaml
let create () = ref None
let is_empty l = !l = None

let value elt = elt.value

let first l = !l
let next elt = elt.next
let prev elt = elt.prev
```

These all follow relatively straight-forwardly from our type
definitions.

<note><title> Cyclic data structures </title>

Doubly-linked lists are a cyclic data structure, meaning that it is
possible to follow a nontrivial sequence of pointers that closes in on
itself.  In general, building cyclic data structures requires the use
of side-effects.  This is done by constructing the data elements
first, and then adding cycles using assignment afterwards.

There is an exception to this, though: you can construct fixed-size
cyclic data-structures using `let rec`.

```ocaml
# let rec endless_loop = 1 :: 2 :: 3 :: endless_loop;;
val endless_loop : int list =
  [1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1;
   2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2;
   ...]
```

This approach is quite limited, however.  General purpose cyclic data
structures require mutation.

</note>

### Modifying the list

Now, we'll start considering operations that mutate the list, starting
with `insert_first`, which inserts an element at the front of the
list.

```ocaml
let insert_first l value =
  let new_elt = { prev = None; next = !l; value } in
  begin match !l with
  | Some old_first -> old_first.prev <- Some new_elt
  | None -> ()
  end;
  l := Some new_elt;
  new_elt
```

`insert_first` first defines a new element `new_elt`, and then links
it into the list, finally setting the the list itself to point to
`new_elt`.  Note that the precedence of a `match` expression is very
low, so to separate it from the following assignment `l := Some
new_front`, we surround the match in a `begin ... end` bracketing (we
could also use parentheses).  If we did not, the final assignment
would become part of the `None -> ...` case, which is not what we
want.

In order to add elements later in the list, we can use `insert_after`,
which takes an `element` as an argument, after which it inserts a new
element.

```ocaml
let insert_after elt value =
  let new_elt = { value; prev = Some elt; next = elt.next } in
  begin match elt.next with
  | Some old_next -> old_next.prev <- Some new_elt
  | None -> ()
  end;
  elt.next <- Some new_elt;
  new_elt
```

Finally, we need a `remove` function.

```ocaml
let remove l elt =
  let { prev; next; _ } = elt in
  begin match prev with
  | Some prev -> prev.next <- next
  | None -> l := next
  end;
  begin match next with
  | Some next -> next.prev <- prev;
  | None -> ()
  end;
  elt.prev <- None;
  elt.next <- None
```

Note that the above code is careful to change the `prev` pointer of
the following element, and the `next` pointer of the previous element,
if they exist.  If there's no previous element, then the list pointer
itself is updated.  In any case, the next and previous pointers of the
element itself are set to `None`.

These functions are more fragile than they may seem.  In particular,
misuse of the interface may lead to corrupted data.  For example,
double-removing an element will cause the main list reference to be
set to `None`, thus emptying the list.  Similar problems arise from
removing an element from a list it doesn't belong to.

This shouldn't be a big surprise.  Complex imperative data structures
can be quite tricky; considerably trickier than their pure
equivalents.  The issues described above can be dealt with by more
careful error detection, and such error correction is taken care of in
modules like Core's `Doubly_linked`.  You should use imperative data
structures from a well-designed library when you can.  And when you
can't, you should make sure that the code you write is careful about
error detection.

### Iteration functions

When defining containers like lists, dictionaries and trees, you'll
typically want to define a set of iteration functions, like `iter`,
`map`, and `fold`, which let you concisely express common iteration
patterns.

`Dlist` has two such iterators: `iter`, the goal of which is to call a
`unit` producing function on every element of the list, in order; and
`find_el`, which runs a provided test function on each values stored
in the list, returning the first `element` that passes the test.  Both
`iter` and `find_el` are implemented using simple recursive loops that
use `next` to walk from element to element, and `value` to extract the
element from a given node.

```ocaml
let iter l ~f =
  let rec loop = function
    | None -> ()
    | Some el -> f (value el); loop (next el)
  in
  loop !l

let find_el l ~f =
  let rec loop = function
    | None -> None
    | Some elt ->
      if f (value elt) then Some elt
      else loop (next elt)
  in
  loop !l
```

## Laziness and other unobservable effects

There are many instances where you basically want to program in a pure
style, but you want to make limited use of side-effects to improve the
performance of your code, without really changing anything else.  Such
side effects are sometimes called _unobservable effects_, and they are
a useful way of leveraging OCaml's imperative features while still
maintaining most of the benefits of pure programming.

One of the simplest unobservable effect is _laziness_.  A lazy value
is one that is not computed until it is actually needed.  In OCaml,
lazy values are created using the `lazy` keyword, which can be used to
prefix any expression, returning a value of type `'a Lazy.t`.  The
evaluation of that expression is delayed until forced with the
`Lazy.force` function.

```ocaml
# let v = lazy (print_string "performing lazy computation\n"; sqrt 16.);;
val v : float lazy_t = <lazy>
# Lazy.force v;;
performing lazy computation
- : float = 4.
# Lazy.force v;;
- : float = 4.
```

You can see from the print statement that the actual computation was
performed only once, and only after `force` had been called.

To better understand how laziness works, let's walk through the
implementation of our own lazy type.  We'll start by declaring types
to represent a lazy value.

```ocaml
# type 'a lazy_state =
  | Delayed of (unit -> 'a)
  | Value of 'a
  | Exn of exn
type 'a lazy_state = Delayed of (unit -> 'a) | Value of 'a | Exn of exn
```

A `lazy_state` represents the possible states of a lazy value.  A
lazy value is `Delayed` before it has been run, where `Delayed` holds
a function for computing the value in question.  A lazy value is in
the `Value` state when it has been forced and the computation ended
normally.  The `Exn` case is for when the lazy value has been forced,
but the computation ended with an exception.  A lazy value is just a
reference to a `lazy_state`.

We can create a lazy value based on a thunk, _i.e._, a function that
takes a unit argument.  Wrapping an expression in a thunk is another
way to suspend the computation of an expression.

```ocaml
# let create_lazy f = ref (Delayed f);;
val create_lazy : (unit -> 'a) -> 'a lazy_state ref = <fun>
# let v = create_lazy
    (fun () -> print_string "performing lazy computation\n"; sqrt 16.);;
  val v : float lazy_state ref = {contents = Delayed <fun>}
```

Now we just need a way to force a lazy value.  The following function
does just that.

```ocaml
# let force v =
    match !v with
    | Value x -> x
    | Exn e -> raise e
    | Delayed f ->
      try
        let x = f () in
        v := Value x;
        x
      with exn ->
        v := Exn exn;
        raise exn
   ;;
val force : 'a lazy_state ref -> 'a = <fun>
```

Which we can use in the same way we used `Lazy.force`:

```ocaml
# force v;;
performing lazy computation
- : float = 4.
# force v;;
- : float = 4.
```

The main difference between our implementation of laziness and the
built-in version is syntax.  Rather than writing `create_lazy (fun ()
-> sqrt 16.)`, we can just write `lazy (sqrt 16.)`.

### Memoization

Another unobservable effect is _memoization_.  A memoized function
remembers the result of previous invocations of the function so that
they can be returned without further computation when the same
arguments are presented again.

Here's a function that takes as an argument an arbitrary
single-argument function and returns a memoized version of that
function.  Here we'll use Core's `Hashtbl` module, rather than our toy
`Dictionary`.

```ocaml
# let memoize f =
    let table = Hashtbl.Poly.create () in
    (fun x ->
      match Hashtbl.find table x with
      | Some y -> y
      | None ->
        let y = f x in
        Hashtbl.add_exn table ~key:x ~data:y;
        y
    );;
val memoize : ('a -> 'b) -> 'a -> 'b = <fun>
```

Note that we use `Hashtbl.Poly.create` to create a hash table using
OCaml's built-in polymorphic hash function.  It's also possible to
create a hash-table using a hash function specialized to a given
type.

Memoization can be useful whenever you have a function that is
expensive to recompute, and you don't mind caching old values
indefinitely.  But memoization is also useful for efficiently
implementing some recursive algorithms.  One good example is the
algorithm for computing the _edit distance_ (also called the
Levenshtein distance) between two strings.  The edit distance is the
number of single-character changes (including letter switches,
insertions and deletions) required to convert one string to the other.
This kind of distance metric can be useful for a variety of
approximate string matching problems, like spell checkers.

Consider the following code for computing the edit distance.
Understanding the algorithm isn't important here, but you should pay
attention to the structure of the recursive calls.

```ocaml
# let rec edit_distance s t =
    match String.length s, String.length t with
    | (0,x) | (x,0) -> x
    | (len_s,len_t) ->
      let s' = String.drop_suffix s 1 in
      let t' = String.drop_suffix t 1 in
      let cost_to_drop_both =
        if s.[len_s - 1] = t.[len_t - 1] then 0 else 1
      in
      List.reduce_exn ~f:Int.min
        [ edit_distance s' t  + 1
        ; edit_distance s  t' + 1
        ; edit_distance s' t' + cost_to_drop_both
        ]
  ;;
val edit_distance : string -> string -> int = <fun>
# edit_distance "OCaml" "ocaml";;
- : int = 2
```

The thing to note is that if you call `edit_distance "OCaml" "ocaml"`,
then that will in turn dispatch the following calls:

```ocaml
edit_distance "OCam" "ocaml"
edit_distance "OCaml" "ocam"
edit_distance "OCam" "ocam"
```

And these calls will in turn dispatch other calls:

```ocaml
edit_distance "OCam" "ocaml"
   edit_distance "OCa" "ocaml"
   edit_distance "OCam" "ocam"
   edit_distance "OCa" "ocam"
edit_distance "OCaml" "ocam"
   edit_distance "OCam" "ocam"
   edit_distance "OCaml" "oca"
   edit_distance "OCam" "oca"
edit_distance "OCam" "ocam"
   edit_distance "OCa" "ocam"
   edit_distance "OCam" "oca"
   edit_distance "OCa" "oca"
```

As you can see, some of these calls are repeats.  For example, there
are two different calls to `edit_distance "OCam" "oca"`.  The number
of redundant calls grows exponentially with the size of the strings,
meaning that our implementation of `edit_distance` is brutally slow
for large strings.  We can see this by writing a small timing
function.

```ocaml
# let time f =
    let start = Time.now () in
    let y = f () in
    let stop = Time.now () in
    printf "Time: %s\n" (Time.Span.to_string (Time.diff stop start));
    y ;;
val time : (unit -> 'a) -> 'a = <fun>
```

And now we can use this to try out some examples.

```ocaml
# time (fun () -> edit_distance "OCaml" "ocaml");;
Time: 5.11003ms
- : int = 2
# time (fun () -> edit_distance "OCaml 4.01" "ocaml 4.01");;
Time: 19.3322s
- : int = 2
```

Just those few extra characters made it almost 4000 times slower!

Memoization would be a huge help here, but to fix the problem, we need
to memoize the calls that `edit_distance` makes to itself.  To see how
to do this, let's step away from `edit_distance`, and instead consider
a much simpler example: computing the nth element of the Fibonacci
sequence.  The Fibonacci sequence by definition starts out with two
`1`'s, with every subsequent element being the sum of the previous
two.  The classic recursive definition of Fibonacci is as follows:

```ocaml
# let rec fib i =
    if i <= 1 then 1 else fib (i - 1) + fib (i - 2);;
```
This is, however, exponentially slow, for the same reason that
`edit_distance` was slow: we end up making many redundant calls to
`fib`.  It shows up quite dramatically in the performance.

```ocaml
# time (fun () -> fib 5);;
Time: 0.0100136ms
- : int = 8
# time (fun () -> fib 10);;
Time: 0.0441074ms
- : int = 89
# time (fun () -> fib 20);;
Time: 5.17392ms
- : int = 10946
# time (fun () -> fib 40);;
Time: 51.4205s
```

Here, `fib 40` takes almost a minute to compute, as opposed to five
_milliseconds_ for `fib 20`.

So, how can we use memoization to make this faster?  The tricky bit is
that we need to insert the memoization before the recursive calls
within `fib`.  We can't just define `fib` in the ordinary way and
memoize it after the fact and expect any improvement.

The first step is to write `fib` in a way that partially unwinds the
recursion.  The following definition of `fib_recur` expects as its
first argument to be passed a function that it can use for making its
recursive calls.

```ocaml
# let fib_recur recur i =
    if i <= 1 then i
    else recur (i - 1) + recur (i - 2) ;;
val fib_recur : (int -> int) -> int -> int = <fun>
```

We can now turn this back into an ordinary Fibonacci function by tying
the recursive knot, as shown below.

```ocaml
# let rec fib i = fib_recur fib i
val fib : int -> int = <fun>
# fib 5;;
- : int = 8
```

And we can even write a polymorphic function that we'll call `fix`
that can tie the recursive not for any function of this form.

```ocaml
# let fix f =
    let rec f' x = f f' x in
    f'
  ;;
val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
# let fib = fix fib_recur;;
val fib : int -> int = <fun>
# fib 5;;
- : int = 8
```

This is a pretty strange piece of code, and it may take a few minutes
of staring at this to figure out what's going on.  Like `fib_recur`,
the function `f` passed into `fix` is a function that isn't recursive,
but takes as an argument of a function that it will call.  What `fix`
does is to essentially feed `f` to itself, thus making a true
recursive function.

This is clever enough, but all we've done so far is find a new way to
implement the same old slow Fibonacci function.  To make it faster, we
need a function like `fix` that inserts memoization when it ties the
recursive knot.  Here is just such a function.

```ocaml
# let memo_fix f =
     let rec f' = lazy (memoize (fun x -> f (Lazy.force f') x)) in
     Lazy.force f'
  ;;
val memo_fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
```

It's not obvious why we need to use laziness here.   It seems like you
should be able to get away with just dropping the the `lazy` and the
`Lazy.force` from the above code, as follows.

```ocaml
# let memo_fix f =
     let rec f' = memoize (fun x -> f f' x) in
     f'
  ;;
      Characters 35-60:
       let rec f' = memoize (fun x -> f f' x) in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
```

OCaml rejects the definition because OCaml, as a strict language,
can't safely construct the closure `(fun x -> f f' x)` until `f'`
itself is defined, and `f'` can't be defined until `memoize` runs, by
which time the closure already needs to exist.  Generally, OCaml
requires that the right hand side of a `let rec` be either a function,
a `lazy` value or a variant or record constructor.

In any case, using `memo_fix`, we can now build an efficient version
of `fib`.

```ocaml
# let fib = memo_fix fib_recur;;
val fib : int -> int = <fun>
# time (fun () -> fib 40);;
Time: 0.236034ms
```

We can even combine this into one compact form that makes this look as
much as possible like an ordinary function declaration.  Here, we're
essentially using `memo_fix` as a custom form of `let rec`.

```ocaml
# let fib = memo_fix (fun fib i ->
    if i <= 1 then 1 else fib (i - 1) + fib (i - 2));;
val fib : int -> int = <fun>
```

This very same approach will work for `edit_distance`.  The one change
we'll need to make is that `edit_server` will now take a pair of
strings as a single argument, since `memoize` only works sensibly for
single-argument functions.  We can always recover the original
interface with a wrapper function.

```ocaml
# let edit_distance = memo_fix (fun edit_distance (s,t) ->
    match String.length s, String.length t with
    | (0,x) | (x,0) -> x (* if either string is empty, return the length of the
                            other string. *)
    | (len_s,len_t) ->
      let s' = String.drop_suffix s 1 in
      let t' = String.drop_suffix t 1 in
      let cost_to_drop_both =
        if s.[len_s - 1] = t.[len_t - 1] then 0 else 1
      in
      List.reduce_exn ~f:Int.min
        [ edit_distance (s',t ) + 1
        ; edit_distance (s ,t') + 1
        ; edit_distance (s',t') + cost_to_drop_both
        ]) ;;
val edit_distance : string * string -> int = <fun>
```

And this new version of `edit_distance` is indeed much more efficient
than the one we started with.

```ocaml
# time (fun () -> edit_distance ("OCaml 4.01","ocaml 4.01"));;
Time: 2.14601ms
- : int = 2
```
This is about ten thousand times faster than our original
implementation.

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
