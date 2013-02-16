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

Finally, we need to a `remove` function.

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

There are many instances where imperative programming is used to
modify or improve the performance characteristics of a program,
without otherwise changing the behavior.  In other words, the program
could be written without side-effects, but performance is improved by
having some side-effecting functions working behind the scenes.

One of the simplest of these is _laziness_.  A lazy value is one that
is not computed until it is actually needed.  In OCaml, lazy values
are created using the `lazy` keyword, which can be used to prefix any
expression, returning a value of type `'a Lazy.t`.  The computation of
is delayed until forced with the `Lazy.force` function.

```ocaml
# let v = lazy (print_string "performing lazy computation\n"; sqrt 16.);;
val v : float lazy_t = <lazy>
# Lazy.force v;;
performing lazy computation
- : float = 4.
# Lazy.force v;;
- : float = 4.
```

In the above, we stuck a print statement in the middle of the
expression just to make it clear when the actual computation was
happening.

You can think of laziness as a form of mutation, where the result of a
computation is stored the first time that force is called.  Indeed,
using mutation we can implement our own lazy values directly.

```ocaml
# module Our_lazy : sig
     type 'a t

     val create : (unit -> 'a) -> 'a t
     val force : 'a t -> 'a
  end = struct
     type 'a maybe_delayed = Delayed of (unit -> 'a) | Value of 'a | Exn of exn
     type 'a t = 'a maybe_delayed ref

     let create f = ref (Delayed f)
     let force v =
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
  end;;
```

The `'a maybe_delayed` type either contains a delayed value,
represented as a function, or the result of the execution of that
function, which may be an ordinary return value, or may be an
exception.  An `Our_lazy.t` is a `ref` pointing to a `maybe_delayed`
value, and `Our_lazy.force` will run the delayed function, storing the
result in the `ref`, or simply returns the result of that function if
it has been forced previously.  Note that the form of that result may
be an exception, in which case the exception will be re-raised rather
than returned.

`Our_lazy` can be used in roughly the same way as the built-in `lazy`:

```ocaml
# let v = Our_lazy.create
    (fun () -> print_string "performing lazy computation\n"; sqrt 16.);;
val v : float Our_lazy.t = <abstr>
# Our_lazy.force v;;
performing lazy computation
- : float = 4.
# Our_lazy.force v;;
- : float = 4.
```

The main difference between our implementation of laziness and the
built-in version is syntax.  Rather than writing `Our_lazy.create (fun
() -> sqrt 16.)`, we can just write `lazy (sqrt 16.)`.

Laziness lets you do considerably more than just delay a single
computation.  You can also use laziness to do things like building
so-called infinite data structures, which are really data-structures
of unbounded size that expand only to the degree necessary to complete
a given calculation.  We'll discuss more about how to use laziness in
[xref](???).

### Memoization

Another unobservable effect is _memoization_.  A memoized function
remembers the result of previous invocations of the function so that
they can be provided without further computation when the same
arguments are presented again.

Here's a function that takes as an argument an arbitrary
single-argument function and returns a memoized version of that
function.  Note that it uses Core's `Hashtbl` module, rather than our
toy `Dictionary`.

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

As you can see, some of these calls are repeats.  For example,
there are two different calls to `edit_distance "OCam" "oca"`.  The
amount of repeated calls grows exponentially with the size of the
strings, meaning that our implementation of `edit_distance` is
brutally slow for large strings.  We can see this by writing a small
timing function.

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

Memoization would be a huge help here, but to make any improvement
here, we need to memoize the calls that `edit_distance` makes to
itself.  To see how to do this, let's step away from `edit_distance`,
and instead consider a simpler example: computing the nth element of
the Fibonacci sequence.  The Fibonacci sequence by definition starts
out with two `1`'s, with every subsequent element being the sum of the
previous two.  The classic recursive definition of Fibonacci is as
follows:

```ocaml
# let rec fib i =
    if i <= 1 then 1 else fib (i - 1) + fib (i - 2);;
```

This is, however, exponentially slow, for the same reason that
`edit_distance` was slow, as we can see:

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

So, how do we memoize the function?  The tricky bit is that we need to
insert the memoization before the recursive calls, so we can't just do
the memoization on the outside after `fib` is defined.  The first step
is to write `fib` in a way that unwinds the recursion, allowing us to
explicitly pass in the function to call recursively.

```ocaml
# let fib_recur recur i =
    if i <= 1 then i
    else recur (i - 1) + recur (i - 2) ;;
val fib_recur : (int -> int) -> int -> int = <fun>
```

We can now turn this back into an ordinary Fibonacci function by tying
the recursive knot:

```ocaml
# let rec fib i = fib_recur fib i
val fib : int -> int = <fun>
# fib 5;;
- : int = 8
```

And we can even write a higher-order function that we'll call `fix`
that ties this knot for us for any function of this type.

```ocaml
# let rec fix f x = f (fix f) x;;
val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
# let fib = fix fib_recur;;
val fib : int -> int = <fun>
# fib 5;;
- : int = 8
```

But really, we've just done another way of implementing the same old
slow Fibonacci function.  To make it faster, we need a function like
`fix` that inserts memoization when it ties the recursive knot.  Here
is just such a function.

```ocaml
# let memo_fix f x =
    let rec f' x = f (Lazy.force memo_f') x
    and memo_f' = lazy (memoize f')
    in
    f' x
 ;;
val memo_fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
```

Note the use of laziness here, which allows us to define `f'` in terms
of each other `memo_f'`.  Laziness is generally useful in making it
possible to make complex recursive definitions.

And now, we can build ourselves an efficient version of Fibonacci.

```ocaml
# let fib = memo_fix fib_recur;;
val fib : int -> int = <fun>
# time (fun () -> fib 40);;
Time: 0.236034ms
```

Now, we can go back to our implementation of `edit_distance`, and pull
the same trick.  First, here's the version of `edit_server` with the
recursion taken out.  Note that we also change `edit_server` so that
it takes the pair of strings to be compared as a single argument,
since memoization only works sensibly for single-argument functions.

```ocaml
# let edit_distance_recur recur (s,t) =
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
        [ recur (s',t ) + 1
        ; recur (s ,t') + 1
        ; recur (s',t') + cost_to_drop_both
        ] ;;
val edit_distance_recur : (string * string -> int) -> string * string -> int =
  <fun>
```

And now, we can use `memo_fix` to create an efficient, memoized
version of this function.  While we're at it, we'll flip the calling
convention back to being an ordinary curried function.

```ocaml
# let edit_distance x y = memo_fix edit_distance_recur (x,y);;
val edit_distance : string -> string -> int = <fun>
# time (fun () -> edit_distance "OCaml 4.01" "ocaml 4.01");;
Time: 2.14601ms
- : int = 2
```

This is about ten thousand times faster than our original
implementation.

Note that this use of memoization relies on side-effects to cache
intermediate computations, but it doesn't change the values of the
function.  It's all about improving performance, not about changing
behavior.

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
