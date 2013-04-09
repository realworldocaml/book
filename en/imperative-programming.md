# Imperative Programming

Most of the code shown so far in this book, and indeed, most OCaml
code in general, is _pure_.  Pure code works without mutating the
program's internal state, performing I/O, reading the clock, or in any
other way interacting with changeable parts of the world.  Thus, a
pure function behaves like a mathematical function, always returning
the same results when given the same inputs, and never affecting the
world except insofar as it returns the value of its computation.
_Imperative_ code, on the other hand, operates by side-effects that
modify a program's internal state or interact with the outside world.
An imperative function has a new effect, and potentially returns
different results, every time it's called.

Pure code is the default in OCaml, and for good reason --- it's
generally easier to reason about, less error prone and more
composable.  But imperative code is of fundamental importance to any
practical programming language because real-world tasks require that
you interact with the outside world, which is by its nature
imperative.  Imperative programming can also be important for
performance.  While pure code is quite efficient in OCaml, there are
many algorithms that can only be implemented efficiently using
imperative techniques.

OCaml offers a happy compromise here, making it easy and natural to
program in a pure style, but also providing great support for
imperative programming where you need it.  This chapter will walk you
through OCaml's imperative features, and help you use them to their
fullest.

## Example: Imperative dictionaries

We'll start with the implementation of a simple imperative dictionary,
_i.e._, a mutable mapping from keys to values.  This is really for
illustration purposes; both Core and the standard library provide
imperative dictionaries, and for most real world tasks, you should use
one of those implementations.

Our dictionary, like those in Core and the standard library, will be
implemented as a hash table.  In particular, we'll use an _open
hashing_ scheme, which is to say the hash table will be an array of
buckets, each bucket containing a list of key/value pairs that have
been hashed into that bucket.

Here's the interface we'll match, provided as an `mli`.  Here, the
type `('a, 'b) t` is used for a dictionary with keys of type `'a` and
data of type `'b`.

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

The `mli` also includes a collection of helper functions whose purpose
and behavior should be largely inferrable from their names and type
signatures.  Notice that a number of the functions, in particular,
ones like `add` that modify the dictionary, return unit.  This is
typical of functions that act by side-effect.

We'll now walk through the implementation (contained in the
corresponding `ml` file) piece by piece, explaining different
imperative constructs as they come up.

Our first step is to define the type of a dictionary as a record with
two fields.

```ocaml
(* file: dictionary.ml *)
open Core.Std

type ('a, 'b) t = { mutable length: int;
                    buckets: ('a * 'b) list array;
                  }
```

The first field, `length` is declared as mutable.  In OCaml, records
are immutable by default, but individual fields are mutable when
marked as such.  The second field, `buckets`, is immutable, but
contains an array, which is itself a mutable data structure, as we'll
see.

Now we'll start putting together the basic functions for manipulating
a dictionary.

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

Note that `num_buckets` is a constant.  That's because, for
simplicity's sake, we're using a fixed-length bucket array.  For a
practical implementation, the length of the array would have to be
able to grow as the number of elements in the dictionary increases.

The function `hash_bucket` is used throughout the rest of the module
to choose the position in the array that a given key should be stored
at.  It is implemented on top of `Hashtbl.hash`, which is a hash
function provided by the OCaml runtime that can be applied to values
of any type.  Thus, its own type is polymorphic: `'a -> int`.

While `Hashtbl.hash` can be used with any type, it won't necessarily
succeed for all values.  `Hashtbl.hash` will throw an exception if it
encounters a value it can't handle, like a function or a value from a
C libraries that lives outside the OCaml heap.

The other functions defined above are fairly straightforward:

- `create` creates an empty dictionary.
- `length` grabs the length from the corresponding record field, thus
  returning the number of entries stored in the dictionary.
- `find` looks for a matching key in the table and returns the
  corresponding value if found as an option.

A new piece of syntax has also popped up in `find`: we write
`array.(index)` to grab a value from an array.  Also, `find` uses
`List.find_map`, which you can see the type of by typing it into the
toplevel:

```ocaml
# List.find_map;;
- : 'a list -> f:('a -> 'b option) -> 'b option = <fun>
```

`List.find_map` iterates over the elements of the list, calling `f` on
each one until a `Some` is returned by `f`, at which point the value
returned by `f` is returned by `find_map`.  If `f` returns `None` on
all values, then `None` is returned by `find_map`.

Now let's look at the implementation of `iter`:

```ocaml
let iter t ~f =
  for i = 0 to Array.length t.buckets - 1 do
    List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
  done
```

`iter` is designed to walk over all the entries in the dictionary.  In
particular, `iter d ~f` will call `f` for each key/value pair in
dictionary `d`.  Note that `f` must return `unit`, since it is
expected to work by side effect rather than by returning a value, and
the overall `iter` function returns `unit ` as well.

The code for `iter` uses two forms of iteration: a `for` loop to walk
over the array of buckets; and within that loop, and a call to
`List.iter` to walk over the list of values in a given bucket.  We
could have done the outer loop with a recursive function instead of a
`for` loop, but `for` loops are syntactically convenient, and are more
familiar and idiomatic in the context of imperative code.

The following code is for adding and removing mappings from the
dictionary.

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
  if bucket_has_key t i key then (
    let filtered_bucket =
      List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
    in
    t.buckets.(i) <- filtered_bucket;
    t.length <- t.length - 1
  )
```

This above code is made more complicated by the fact that we need to
detect whether we are overwriting or removing an existing binding, so
we can decide whether `t.length` needs to be changed.  The helper
function `bucket_has_key` is used for this purpose.

Another new piece of syntax shows up in both `add` and `remove`: the
use of the `<-` operator to update elements of an array (`array.(i) <-
expr`) and for updating a record field (`record.field <- expression`).

We also use a single semicolon, `;`, as a sequencing operator, to
allow us to do a sequence of side-effecting operations in a row:
first, update the bucket, then update the count.  We could have done
this using let bindings:

```ocaml
    let () = t.buckets.(i) <- filtered_bucket in
    t.length <- t.length - 1
```

but `;` is more concise and idiomatic.  More generally,

```ocaml
<expr1>;
<expr2>;
...
<exprN>
```

is eqivalent to

```ocaml
let () = <expr1> in
let () = <expr2> in
...
<exprN>
```

When a sequence expression `expr1; expr2` is evaluated, `expr1` is
evaluated first, and then `expr2`.  The expression `expr1` should have
type `unit` (though this is a warning rather than a hard restriction),
and the value of `expr2` is returned as the value of the entire
sequence.  For example, the sequence `print_string "hello world"; 1 +
2` first prints the string `"hello world"`, then returns the integer
`3`.

Note also that we do all of the side-effecting operations at the very
end of each function.  This is good practice because it minimizes the
chance that such operations will be interrupted with an exception,
leaving the data structure in an inconsistent state.

## Primitive mutable data

Now that we've looked at a complete example, let's take a more
systematic look at imperative programming in OCaml.  We encountered
two different forms of mutable data above: records with mutable fields
and arrays.  We'll now discuss these in more detail, along with the
other primitive forms of mutable data that are available in OCaml.

### Array-like data

OCaml supports a number of array-like data structures; _i.e._, mutable
integer-indexed containers that provide constant-time access to their
elements.  We'll discuss several of them below.

#### Ordinary arrays

The `array` type is used for general purpose polymorphic arrays.  The
`Array` module has a variety of utility functions for interacting with
arrays, including a number of mutating operations.  These include
`Array.set`, for setting an individual element, and `Array.blit`, for
efficiently copying values from one range of indices to another.

Arrays also come with special syntax for retrieving an element from an
array:

```ocaml
array.(index)
```

and for setting an element in an array:

```ocaml
array.(index) <- expr
```

Array literals are written using `[|` and `|]` as delimiters.  Thus,
`[| 1; 2; 3 |]` is a literal integer array.

#### Strings

Strings are essentially byte-arrays which are often used for textual
data.  The main advantage of using a `string` in place of a `Char.t
array` (a `Char.t` is an 8-bit character) is that the former is
considerably more space efficient; an array uses one word --- 8 bytes
on a 64-bit machine --- to store a single entry, whereas strings use
one byte per character.

Strings also come with their own syntax for getting and setting
values: `string.[index]` and `string.[index] <- expr` respectively,
and string literals are bounded by quotes.  There's also a module
`String` where you'll find useful functions for working with strings.
We'll talk more about the `String` module in
[xref](#text-processing-and-unicode).

#### Bigarrays

A `Bigarray.t` is a handle to a block of memory stored outside of the
OCaml heap.  These are mostly useful for interacting with C or Fortran
libraries, and are discussed in
[xref](#managing-external-memory-with-bigarrays).  Bigarrays too have
their own getting and setting syntax: `bigarray.{index}` and
`bigarray.{index} <- expr`.  There is no literal syntax for bigarrays.

### Mutable record and object fields and ref cells

As we've seen, records are immutable by default, but individual record
fields can be declared as mutable.  These mutable fields can be set
using the `<-` operator, _i.e._, `record.field <- expr`.

As we'll see in [xref](#object-oriented-programming), fields of an
object can similarly be declared as mutable, and can then be modified
in much the same way as record fields.

#### Ref Cells

Variables in OCaml are never mutable --- they can refer to mutable
data, but what the variable points to can't be changed.  Sometimes,
though, you want to do exactly what you would do with a mutable
variable in another language: define a single, mutable value. In OCaml
this is typically achieved using a `ref`, which is essentially a
container with a single mutable polymorphic field.

The definition for the ref type is as follows:

```ocaml
type 'a ref = { mutable contents : 'a }
```

The standard library defines the following operators for working with
refs.

* `ref expr` constructs a reference cell containing the value defined
  by the expression `expr`.
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

All of these are operations are just ordinary OCaml functions, and
could be defined as follows.

```ocaml
let ref x = { contents = x }
let (!) r = r.contents
let (:=) r x = r.contents <- x
```

### Foreign functions

Another source of imperative operations in OCaml is resources that
come from interfacing with external libraries through OCaml's foreign
function interface (FFI).  The FFI opens OCaml up to imperative
constructs that are exported by system calls or other external
libraries.  Many of these come built in, like access to the `write`
system call, or to the `clock`; while others come from user libraries,
like LAPACK bindings.

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

OCaml also supports `while` loops, which include a condition and a
body.  The loop first evaluates the condition, and then, if it
evaluates to true, evaluates the body and starts the loop again.
Here's a simple example of a function for reversing an array in-place.

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

In the above, we used `incr` and `decr`, which are build-in functions
for incrementing and decrementing an `int ref` by one, respectively.

## Example: Doubly-linked lists

Another common imperative data structure is the doubly-linked list.
Doubly-linked lists can be traversed in both directions and elements
can be added and removed from the list in constant time.  Core defines
a doubly-linked list (the module is called `Doubly_linked`), but we'll
define our own linked list library as an illustration.

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


The type of the list itself, `'a t`, is an mutable reference to an
optional `element`.  This reference is `None` if the list is empty,
and `Some` otherwise.

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

### Memoization and dynamic programming

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
OCaml's built-in polymorphic hash function.

Memoization can be useful whenever you have a function that is
expensive to recompute, and you don't mind caching old values
indefinitely.  One important caution: every time you create a memoized
function, there's something of a built-in memory leak.  As long as you
hold on to the memoized function, you're holding every result it has
returned thus far.

Memoization is also useful for efficiently implementing some recursive
algorithms.  One good example is the algorithm for computing the _edit
distance_ (also called the Levenshtein distance) between two strings.
The edit distance is the number of single-character changes (including
letter switches, insertions and deletions) required to convert one
string to the other.  This kind of distance metric can be useful for a
variety of approximate string matching problems, like spell checkers.

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
    let x = f () in
    let stop = Time.now () in
    printf "Time: %s\n" (Time.Span.to_string (Time.diff stop start));
    x ;;
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

Just those few extra characters made it almost four thousand times
slower!

Memoization would be a huge help here, but to fix the problem, we need
to memoize the calls that `edit_distance` makes to itself.  This
technique is sometimes referred to as _dynamic programming_.  To see
how to do this, let's step away from `edit_distance`, and instead
consider a much simpler example: computing the nth element of the
Fibonacci sequence.  The Fibonacci sequence by definition starts out
with two `1`'s, with every subsequent element being the sum of the
previous two.  The classic recursive definition of Fibonacci is as
follows:

```ocaml
# let rec fib i =
    if i <= 1 then 1 else fib (i - 1) + fib (i - 2);;
```

This is, however, exponentially slow, for the same reason that
`edit_distance` was slow: we end up making many redundant calls to
`fib`.  It shows up quite dramatically in the performance.

```ocaml
# time (fun () -> fib 20);;
Time: 5.17392ms
- : int = 10946
# time (fun () -> fib 40);;
Time: 51.4205s
- : int = 165580141
```

Here, `fib 40` takes almost a minute to compute, as opposed to five
_milliseconds_ for `fib 20`.

So, how can we use memoization to make this faster?  The tricky bit is
that we need to insert the memoization before the recursive calls
within `fib`.  We can't just define `fib` in the ordinary way and
memoize it after the fact and expect the first call to `fib` to be
improved (though of course repeated calls will be improved).

```ocaml
# let fib = memoize fib;;
val fib : int -> int = <fun>
# time (fun () -> fib 40);;
Time: 52.6s
- : int = 165580141
# time (fun () -> fib 40);;
Time: 0.00596046ms
- : int = 165580141
```

In order to make `fib` fast, our first step will be to rewrite `fib`
in a way that unwinds the recursion.  The following version expects as
its first argument a function (called `fib`) that will be called in
lieu of the usual recursive call.

```ocaml
# let fib_norec fib i =
    if i <= 1 then i
    else fib (i - 1) + fib (i - 2) ;;
val fib_norec : (int -> int) -> int -> int = <fun>
```

We can now turn this back into an ordinary Fibonacci function by tying
the recursive knot, as shown below.

```ocaml
# let rec fib i = fib_norec fib i
val fib : int -> int = <fun>
# fib 5;;
- : int = 8
```

We can even write a polymorphic function that we'll call `make_rec`
that can tie the recursive not for any function of this form.

```ocaml
# let make_rec f_norec =
    let rec f x = f_norec f x in
    f
  ;;
val make_rec : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
# let fib = make_rec fib_norec;;
val fib : int -> int = <fun>
# fib 5;;
- : int = 8
```

This is a pretty strange piece of code, and it may take a few minutes
of thought to figure out what's going on.  Like `fib_norec`, the
function `f_norec` passed into `make_rec` is a function that isn't
recursive, but takes as an argument a function that it will call.
What `make_rec` does is to essentially feed `f_norec` to itself, thus
making it a true recursive function.

This is clever enough, but all we've really done is find a new way to
implement the same old slow Fibonacci function.  To make it
faster, we need variant on `make_rec` that inserts memoization when it
ties the recursive knot.  We'll call that function `memo_rec`.

```ocaml
# let memo_rec f_norec x =
     let fref = ref (fun _ -> assert false) in
     let f = memoize (fun x -> f_norec !fref x) in
     fref := f;
     f x
  ;;
val memo_rec : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
```

Note that `memo_rec` has the same signature as `make_rec`.

We're using the reference here as a way of tying the recursive knot
without using a `let rec`, which for reasons we'll describe later
wouldn't work here.

Using `memo_rec`, we can now build an efficient version of `fib`.

```ocaml
# let fib = memo_rec fib_norec;;
val fib : int -> int = <fun>
# time (fun () -> fib 40);;
Time: 0.236034ms
```

And as you can see, the exponential time complexity is now gone.

The memory behavior here is important.  If you look back at the
definition of `memo_rec`, you'll see that the call to memo_rec does
not trigger a call to `memoize`.  Only when the final argument to
`fib` is presented does `memoize` get called, and the result of that
call falls out of scope when the `fib` call returns.  That means that,
unlike ordinary memoization, calling `memo_rec` on a function does not
create a memory leak --- the memoization table is collected after the
computation completes.

We can use `memo_rec` as part of a single declaration that makes this
look like it's little more than a special form of `let rec`.

```ocaml
# let fib = memo_rec (fun fib i ->
    if i <= 1 then 1 else fib (i - 1) + fib (i - 2));;
val fib : int -> int = <fun>
```

Memoization is overkill for implementing Fibonacci, and indeed, the
`fib` defined above is not especially efficient, allocating space
linear in the number passed in to `fib`.  It's easy enough to write a
Fibonacci function that takes a constant amount of space.

But memoization is a good approach for optimizing `edit_distance`, and
we can apply the same approach we used on `fib` here.  We will need to
change `edit_distance` to take a pair of strings as a single argument,
since `memo_rec` only works on single-argument functions.  (We can
always recover the original interface with a wrapper function.)  With
just that change and the addition of the `memo_rec` call, we can get a
memoized version of `edit_distance`:

```ocaml
# let edit_distance = memo_rec (fun edit_distance (s,t) ->
    match String.length s, String.length t with
    | (0,x) | (x,0) -> x
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

This new version of `edit_distance` is much more efficient than the
one we started with; the following call is about ten thousand times
faster than it was without memoization.

```ocaml
# time (fun () -> edit_distance ("OCaml 4.01","ocaml 4.01"));;
Time: 2.14601ms
- : int = 2
```

<note> <title> Limitations of `let rec` </title>

You might wonder why we didn't tie the recursive knot in
`memo_rec` using `let rec`, as we did for `make_rec` earlier.  Here's
code that tries to do just that:

```ocaml
# let memo_rec f_norec =
     let rec f = memoize (fun x -> f_norec f x) in
     f
  ;;
      Characters 41-72:
       let rec f = memoize (fun x -> f_norec f x) in
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
```

OCaml rejects the definition because OCaml, as a strict language, has
limits on what it can put on the right hand side of a `let rec`.  In
particular, imagine how the following code snippet would be compiled.

```ocaml
let rec x = x + 1
```

Note that `x` is an ordinary value, not a function.  As such, it's not
clear how to execute this code.  In some sense, you could imagine it
compiling down to an infinite loop, but there's no looping control
structure to make that happen.

To avoid such cases, the compiler only allow three possible constructs
to show up on the right-hand side of a `let rec`: a function
definition, a constructor, or the lazy keyword.  This excludes some
reasonable things, like our definition of `memo_rec`, but it also
blocks things that don't make sense, like our definition of `x`.

It's worth noting that these restrictions don't show up in a lazy
language like Haskell.  Indeed, we can make something like our
definition of `x` work if we use OCaml's laziness:

```ocaml
# let rec x = lazy (Lazy.force x + 1);;
val x : int lazy_t = <lazy>
```

Of course, actually trying to compute this will fail.  OCaml's `lazy`
throws an exception when a lazy value tries to force itself as part of
its own evaluation.

```ocaml
# Lazy.force x;;
Exception: Lazy.Undefined.
```

But we can create useful recursive definitions possible with `lazy` as
well.  In particular, we can use laziness to make our definition of
`memo_rec` work without explicit mutation.

```ocaml
# let lazy_memo_rec f_norec x =
     let rec f = lazy (memoize (fun x -> f_norec (Lazy.force f) x)) in
     (Lazy.force f) x
  ;;
val lazy_memo_rec : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
# time (fun () -> lazy_memo_rec fib_norec 40);;
Time: 0.298977ms
- : int = 102334155
```

Laziness is more constrained than explicit mutation, and so in some
cases can lead to code whose behavior is easier to think about.

</note>

## Input and Output

Imperative programming is about more than modifying in-memory
data-structures.  Any function that doesn't boil down to a
deterministic transformation from its arguments to its return value is
imperative in nature.  That includes not only things that mutate your
program's data, but also operations that interact with the world
outside of your program.  An important example of this kind of
interaction is I/O, _i.e._, operations for reading or writing data to
things like files, terminal input and output, and network sockets.

There are multiple I/O libraries in OCaml.  In this section we'll
discuss OCaml's buffered I/O library that can be used through the
`In_channel` and `Out_channel` modules in Core.  Other I/O primitives
are also available through the `Unix` module in Core as well as
`Async`, the asynchronous I/O library that is covered in
[xref](#concurrent-programming-with-async).  Most of the functionality
in Core's `In_channel`, `Out_channel` (and in Core's `Unix` module)
derives from the standard library, but we'll use Core's interfaces
here.

### Terminal I/O

OCaml's buffered I/O library is organized around two types:
`in_channel`, for channels you read from, and `out_channel`, for
channels you write to.  `In_channel` and `Out_channel` modules only
have direct support for channels corresponding to files and terminals;
other kinds of channels can be created through the `Unix` module.

We'll start our discussion of I/O by focusing on the terminal.
Following the UNIX model, communication with the terminal is organized
around three channels, which correspond to the three standard file
descriptors in Unix:

* `In_channel.stdin`.  The "standard input" channel.  By default,
  input comes from the terminal, which handles keyboard input.

* `In_channel.stdout`.  The "standard output" channel.  By default,
  output written to `stdout` appears on the user terminal.

* `In_channel.stderr`.  The "standard error" channel.  This is similar
  to `stdout`, but is intended for error messages.

The values `stdin`, `stdout` and `stderr` are useful enough that they
are also available in the global name-space directly, without having
to go through the `In_channel` and `Out_channel` modules.

Let's see this in action in a simple interactive application.  The
following program, `time_converter`, prompts the user for a timezone,
and then prints out the current time in that timezone.  Here, we use
Core's `Zone` module for looking up a timezone, and the `Time` module
for computing the current time and printing it out in the timezone in
question.

```ocaml
(* file: time_converter.ml *)
open Core.Std

let () =
  Out_channel.output_string stdout "Pick a timezone: ";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> failwith "No timezone provided"
  | Some zone_string ->
    let zone = Zone.find_exn zone_string in
    let time_string = Time.to_localized_string (Time.now ()) zone in
    Out_channel.output_string stdout
      (String.concat
         ["The time in ";Zone.to_string zone;" is "; time_string;"\n"]);
    Out_channel.flush stdout
```

We can build this program (using `ocamlbuild` with the `_tags` file
described in [xref](#single-file-programs)) and run it, you'll see
that it prompts you for input, as follows:

```
$ ./time_converter.byte
Pick a timezone:
```

You can then type in the name of a timezone and hit return, and it
will print out the current time in the timezone in question.

```
Pick a timezone: Europe/London
The time in Europe/London is 2013-03-06 02:15:13.602033
```

We called `Out_channel.flush` on `stdout` because `out_channel`s are
buffered, which is to say that OCaml doesn't immediately do a write
every time you call `output_string`.  Instead, writes are buffered
until either enough has been written to trigger the flushing of the
buffers, or until a flush is explicitly requested.  This greatly
increases the efficiency of the writing process, by reducing the
number of system calls.

Note that `In_channel.input_line` returns a `string option`, with
`None` indicating that the input stream has ended (_i.e._, an
end-of-file condition).  `Out_channel.output_string` is used to print
the final output, and `Out_channel.flush` is called to flush that
output to the screen.  The final flush is not technically required,
since the program ends after that instruction, at which point all
remaining output will be flushed anyway, but the flush is nonetheless
good practice.

### Formatted output with `printf`

Generating output with functions like `Out_channel.output_string` is
simple and easy to understand, but can be a bit verbose.  OCaml also
supports formatted output using the `printf` function, which is
modeled after `printf` in the C standard library.  `printf` takes a
_format string_ that describe what to print and how to format it, as
well as arguments to be printed, as determined by the formatting
directives embedded in the format string.  So, for example, we can
write:


```ocaml
# printf "%i is an integer, %F is a float, \"%s\" is a string\n"
    3 4.5 "five";;
3 is an integer, 4.5 is a float, "five" is a string
- : unit = ()
```

Importantly, and unlike C's `printf`, the `printf` in OCaml is
type-safe.  In particular, if we provide an argument whose type
doesn't match what's presented in the format string, we'll get a type
error.

```ocaml
# printf "An integer: %i\n" 4.5;;
Characters 26-29:
  printf "An integer: %i\n" 4.5;;
                            ^^^
Error: This expression has type float but an expression was expected of type
         int
```

<note> <title> Understanding format strings </title>

The format strings used by `printf` turn out to be quite different
from ordinary strings.  This difference ties to the fact that OCaml
format strings, unlike their equivalent in C, are type-safe.  In
particular, the compiler checks that the types referred to by the
format string match the types of the rest of the arguments passed to
`printf`.

To check this, OCaml needs to analyze the contents of the format
string at compile time, which means the format string needs to be
available as a string literal at compile time.  Indeed, if you try to
pass an ordinary string to `printf`, the compiler will complain.

```ocaml
# let fmt = "%i is an integer, %F is a float, \"%s\" is a string\n";;
val fmt : string = "%i is an integer, %F is a float, \"%s\" is a string\n"
# printf fmt 3 4.5 "five";;
Characters 7-10:
  printf fmt 3 4.5 "five";;
         ^^^
Error: This expression has type string but an expression was expected of type
         ('a -> 'b -> 'c -> 'd, out_channel, unit) format =
           ('a -> 'b -> 'c -> 'd, out_channel, unit, unit, unit, unit)
           format6
```

If OCaml infers that a given string literal is a format string, then
it parses it at compile time as such, choosing its type in accordance
with the formatting directives it finds.  Thus, if we add a
type-annotation indicating that the string we're defining is actually
a format string, it will be interpreted as such:

```ocaml
# let fmt : ('a, 'b, 'c) format =
    "%i is an integer, %F is a float, \"%s\" is a string\n";;
  val fmt : (int -> float -> string -> 'c, 'b, 'c) format = <abstr>
```

And accordingly, we can pass it to `printf`.

```ocaml
# printf fmt 3 4.5 "five";;
3 is an integer, 4.5 is a float, "five" is a string
- : unit = ()
```

If this looks different from everything else you've seen so far,
that's because it is.  This is really a special case in the
type-system.  Most of the time, you don't need to worry about this
special handling of format strings --- you can just use `printf` and
not worry about the details.  But it's useful to keep the broad
outlines of the story in the back of your head.

</note>

Now let's see how we can rewrite our time conversion program to be a
little more concise using `printf`.

```ocaml
(* file: time_converter.ml *)
open Core.Std

let () =
  printf "Pick a timezone: %!";
  match In_channel.input_line stdin with
  | None -> failwith "No timezone provided"
  | Some zone_string ->
    let zone = Zone.find_exn zone_string in
    let time_string = Time.to_localized_string (Time.now ()) zone in
    printf "The time in %s is %s.\n%!" (Zone.to_string zone) time_string
```

In the above example, we've used only two formatting directives: `%s`,
for including a string, and `%!` which causes `printf` to flush the
channel.

`printf`'s formatting directives offer a significant amount of
control, allowing you to specify things like:

- alignment and padding
- escaping rules for strings
- whether numbers should be formatted in decimal, hex or binary
- precision of float conversions

There are also `printf`-style functions that target outputs other than
`stdout`, including:

- `eprintf`, which prints to `stderr`.
- `fprintf`, which prints to an arbitrary `out_channel`
- `sprintf`, which returns a formatted string

All of this, and a good deal more, is described in the API
documentation for the `Printf` module in the OCaml Manual.

### File I/O

Another common use of `in_channel`s and `out_channel`s is for working
with files.  Here's a couple of functions, one that creates a file
full of numbers, and the other that reads in such a file and returns
the sum of those numbers.

```ocaml
# let create_number_file filename numbers =
    let outc = Out_channel.create filename in
    List.iter numbers ~f:(fun x -> fprintf outc "%d\n" x);
    Out_channel.close outc
  ;;
 val create_number_file : string -> int Core.Std.List.t -> unit = <fun>
# let sum_file filename =
     let file = In_channel.create filename in
     let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
     let sum = List.fold ~init:0 ~f:(+) numbers in
     In_channel.close file;
     sum
  ;;
val sum_file : string -> int = <fun>
# create_number_file "numbers.txt" [1;2;3;4;5];;
- : unit = ()
# sum_file "numbers.txt";;
- : int = 15
```

For both of these functions we followed the same basic sequence: we
first create the channel, then use the channel, and finally close the
channel.  The closing of the channel is important, since without it,
we won't release resources associated with the file back to the
operating system.

One problem with the code above is that if it throws an exception in
the middle of its work, it won't actually close the file.  If we try to
read a file that doesn't actually contain numbers, we'll see such an
error:

```ocaml
# sum_file "/etc/hosts";;
Exception: (Failure "Int.of_string: \"##\"").
```

And if we do this over and over in a loop, we'll eventually run out of
file descriptors.

```ocaml
# for i = 1 to 10000 do try ignore (sum_file "/etc/hosts") with _ -> () done;;
- : unit = ()
# sum_file "numbers.txt";;
Exception: (Sys_error "numbers.txt: Too many open files").
```

And now, you'll need to restart your toplevel if you want to open any
more files!

To avoid this, we need to make sure that our code cleans up after
itself.  We can do this using the `protect` function described in
[xref](#error-handling), as follows.

```ocaml
# let sum_file filename =
     let file = In_channel.create filename in
     protect ~f:(fun () ->
         let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
         List.fold ~init:0 ~f:(+) numbers)
       ~finally:(fun () -> In_channel.close file)
  ;;
val sum_file : string -> int = <fun>
```

And now, the file descriptor leak is gone:

```ocaml
# for i = 1 to 10000 do try ignore (sum_file "/etc/hosts") with _ -> () done;;
- : unit = ()
# sum_file "numbers.txt";;
- : int = 15
```

This is really an example of a more general complexity of imperative
programming.  When programming imperatively, you need to be quite
careful to make sure that exceptions don't leave you in an awkward
state.

`In_channel` also supports some idioms that handle some of the details
of this for you.  For example, the `with_file` function takes a
filename and a function for processing that file, and takes care of
the opening and closing of the file transparently.

```ocaml
# let sum_file filename =
     In_channel.with_file filename ~f:(fun file ->
       let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
       List.fold ~init:0 ~f:(+) numbers)
  ;;
val sum_file : string -> int = <fun>
```

Another misfeature of our implementation of `sum_file` is that we
read the entire file into memory before processing it.  For a large
file, it's more efficient to process a line at a time.  You can use
the `In_channel.fold_lines` function to do just that.

```ocaml
# let sum_file filename =
     In_channel.with_file filename ~f:(fun file ->
       In_channel.fold_lines file ~init:0 ~f:(fun sum line ->
         sum + Int.of_string line))
  ;;
val sum_file : string -> int = <fun>
```

This is just a taste of the functionality of `In_channel` and
`Out_channel`.  To get a fuller understanding you should review the
API documentation for those modules.

## Order of evaluation

The order in which expressions are evaluated is an important part of
the definition of a programming language, and it is particularly
important when programming imperatively.  Most programming languages
you're likely to have encountered are _strict_, and OCaml is too.  In
a strict language, when you bind an identifier to the result of some
expression, the expression is evaluated before the variable is
defined.  Similarly, if you call a function on a set of arguments,
those arguments are evaluated before they are passed to the function.

Consider the following simple example.  Here, we have a collection of
angles and we want to determine if any of them have a negative `sin.
The following snippet of code would answer that question.

```ocaml
# let x = sin 120. in
  let y = sin 75.  in
  let z = sin 128. in
  List.exists ~f:(fun x -> x < 0.) [x;y;z]
  ;;
- : bool = true
```

In some sense, we don't really need to compute the `sin 128`, because
`sin 75.` is negative, so we could know the answer before even
computing `sin 128`.

It doesn't have to be this way.  Using the `lazy` keyword, we can
write the original computation so that `sin 128.` won't ever be
computed.

```ocaml
# let x = lazy (sin 120.) in
  let y = lazy (sin 75.)  in
  let z = lazy (sin 128.) in
  List.exists ~f:(fun x -> Lazy.force x < 0.) [x;y;z]
  ;;
- : bool = true
```

We can confirm that fact by a few well placed `printf`s.

```ocaml
# let x = lazy (printf "1\n"; sin 120.) in
  let y = lazy (printf "2\n"; sin 75.)  in
  let z = lazy (printf "3\n"; sin 128.) in
  List.exists ~f:(fun x -> Lazy.force x < 0.) [x;y;z]
  ;;
1
2
- : bool = true
```

OCaml is strict by default for a good reason: Lazy evaluation and
imperative programming generally don't mix well, because laziness
makes it harder to reason about when a given side effect is going to
occur.  Understanding the order of side-effects is essential to
reasoning about the behavior of an imperative program.

In a strict language, we know that expressions that are bound by a
sequence of let-bindings will be evaluated in the order that they're
defined.  But what about the evaluation order within a single
expression?  Officially, the answer is that evaluation order within an
expression is undefined.  In practice, OCaml has only one compiler,
and that behavior is a kind of _de facto_ standard.  Unfortunately, the
evaluation order in this case is often the oppose of what one might
expect.

Consider the following example.

```ocaml
# List.exists ~f:(fun x -> x < 0.)
    [ (printf "1\n"; sin 120.);
      (printf "2\n"; sin 75.);
      (printf "3\n"; sin 128.); ]
  ;;
3
2
1
- : bool = true
```

Here, you can see that the sub-expression that came last was actually
evaluated first!  This is generally the case for many different kinds
of expressions.  If you want to make sure of the evaluation order of
different sub-expressions, you should express them as a series of
`let` bindings.

## Side-effects and weak polymorphism

Consider the following simple imperative function.

```ocaml
# let remember =
    let cache = ref None in
    (fun x ->
       match !cache with
       | Some y -> y
       | None -> cache := Some x; x)
  ;;
```

`remember` simply caches the first value that's passed to it,
returning that value on every call.  It's not a terribly useful
function, but it raises an interesting question: what type should it
have?

On its first call, `remember` returns the same value its passed, which
means its input type and return type should match.  Accordingly,
`remember` should have the type `t -> t` for some type `t`.  There's
nothing about `remember` that ties the choice of `t` to any particular
type, so you might expect OCaml to generalize, replacing `t` with a
polymorphic type variable.  It's this kind of generalization that
gives us polymorphic types in the first place.  The identity function,
as an example, gets a polymorphic type in this way.

```ocaml
# let identity x = x;;
val identity : 'a -> 'a = <fun>
# identity 3;;
- : int = 3
# identity "five";;
- : string = "five"
```

As you can see, the polymorphic type of `identity` lets it operate on
values with different types.

This is not what happens with `remember`, though.  Here's the type
that OCaml infers for remember, which looks almost, but not quite,
like the type of the identity function.

```ocaml
val remember : '_a -> '_a = <fun>
```

The underscore in the type variable `'_a` tells us that the variable
is only _weakly polymorphic_, which is to say that it can be used with
any _single_ type.  That makes sense, because, unlike `identity`,
`remember` always returns the value it was passed on its first
invocation, which means it can only be used with one type.

OCaml will convert a weakly polymorphic variable to a concrete type as
soon as it gets a clue as to what concrete type it is to be used as,
as you can see below.

```ocaml
# let remember_three () = remember 3;;
val remember_three : unit -> int = <fun>
# remember;;
- : int -> int = <fun>
# remember "avocado";;
Characters 9-18:
  remember "avocado";;
           ^^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int
```

Note that the type of `remember` was settled by the definition of
`remember_three`, even though `remember_three` was never called!

### The Value Restriction

So, when does the compiler infer weakly polymorphic types?  As we've
seen, we need weakly polymorphic types when a value of unknown type is
stored in a persistent mutable cell.  Because the type-system isn't
precise enough to determine all cases where this might happen, OCaml
uses a rough rule to flag cases where it's sure there are no
persistent refs, and to only infer polymorphic types in those cases.
This rule is called _the value restriction_.

The core of the value restriction is the observation that some kinds
of simple values by their nature can't contain refs, including:

- Constants (_i.e._, things like integer and floating point literals)
- Constructors that contain only other simple values
- Function declarations, _i.e._, expressions that begin with `fun` or
  `function`, or, the equivalent let binding, `let f x = ...`.
- `let` bindings of the form `let var = <expr1> in <expr2>`, where
  both `<expr1>` and `<expr2>` are simple values.

Thus, the following expression is a simple value, and as a result, the
types of values contained within it are allowed to be polymorphic.

```ocaml
# (fun x -> [x;x]);;
- : 'a -> 'a list = <fun>
```

But, if we write down an expression that isn't a simple value by the
above definition, we'll get different results.  For example, consider
what happens if we try to memoize the function defined above.

```ocaml
# memoize (fun x -> [x;x]);;
- : '_a -> '_a list = <fun>
```

The memoized version of the function does in fact need to be
restricted to a single type, because it uses mutable state behind the
scenes to cache previous invocations of the function it has passed.
But OCaml would make the same determination even if the function in
question did no such thing.  Consider this example:

```ocaml
# identity (fun x -> [x;x]);;
- : '_a -> '_a list = <fun>
```

It would be safe to infer a weakly polymorphic variable here, but
because OCaml's type system doesn't distinguish between pure and
impure functions, it can't separate those two cases.

The value restriction doesn't require that there is no mutable state,
only that there is no _persistent_ mutable state that could share
values between uses of the same function.  Thus, a function that
produces a fresh reference every time it's called can have a fully
polymorphic type:

```ocaml
# let f () = ref None;;
val f : unit -> 'a option ref = <fun>
```

But a function that has a mutable cache that persists across calls,
like memoize, can only be weakly polymorphic.

### Partial application and the value restriction

Most of the time, when the value restriction kicks in, it's for a good
reason, _i.e._, it's because the value in question can actually only
safely be used with a single type.  But sometimes, the value
restriction kicks in when you don't want it.  The most common such
case is partially applied functions.  A partially applied function,
like any function application, is not a simple value, and as such,
functions created by partial application are sometimes less general
than you might expect.

Consider the `List.init` function, which is used for creating lists
where each element is created by calling a function on the index of
that element.

```ocaml
# List.init;;
- : int -> f:(int -> 'a) -> 'a list = <fun>
# List.init 10 ~f:Int.to_string;;
- : string list = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
```

Imagine we wanted to create a specialized version of `List.init` that
always created lists of length 10.  We could do that using partial
application, as follows.

```ocaml
# let list_init_10 = List.init 10;;
val list_init_10 : f:(int -> '_a) -> '_a list = <fun>
```

As you can see, we now infer a weakly polymorphic type for the
resulting function, and for good reason.  There's nothing that tells
us that `List.init` isn't creating a persistent `ref` somewhere inside
of it that would be shared across multiple calls to `list_init_10`.
We can eliminate this possibility, and at the same time get the
compiler to infer a polymorphic type, by using explicit variables
rather than partial application.

```ocaml
# let list_init_10 ~f = List.init 10 ~f;;
val list_init_10 : f:(int -> 'a) -> 'a list = <fun>
```

This transformation is referred to as _eta expansion_, and is often
useful to resolve problems that arise from the value restriction.

### Relaxing the value restriction

OCaml is actually a little better at inferring polymorphic types than
is implied above.  The value restriction as we described it above is
basically a syntactic check: there are a few operations that you can
do that count as simple values, and anything that's a simple value can
be generalized.

But OCaml actually has a relaxed version of the value restriction that
can make some use of type information to allow polymorphic types for
things that are not simple values.

For example, we saw above that a function application, even a simple
application of the identity function, is not a simple value and thus
can turn a polymorphic value into a weakly polymorphic one.

```ocaml
# identity (fun x -> [x;x]);;
- : '_a -> '_a list = <fun>
```

But that's not always the case.  When the type of the returned value
is immutable, then OCaml can typically infer a fully polymorphic type.

```ocaml
# identity [];;
- : 'a list = []
```

On the other hand, if the returned type is potentially mutable, then
the result will be weakly polymorphic.

```ocaml
# [||];;
- : 'a array = [||]
# identity [||];;
- : '_a array = [||]
```

A more important example of this comes up when defining abstract data
types.  Consider the following simple data-structure for an immutable
list type that supports constant-time concatenation.

```ocaml
# module Concat_list : sig
    type 'a t
    val empty : 'a t
    val singleton : 'a -> 'a t
    val concat  : 'a t -> 'a t -> 'a t  (* constant time *)
    val to_list : 'a t -> 'a list       (* linear time   *)
  end = struct

    type 'a t = Empty | Singleton of 'a | Concat of 'a t * 'a t

    let empty = Empty
    let singleton x = Singleton x
    let concat x y = Concat (x,y)

    let rec to_list_with_tail t tail =
      match t with
      | Empty -> tail
      | Singleton x -> x :: tail
      | Concat (x,y) -> to_list_with_tail x (to_list_with_tail y tail)

    let to_list t =
      to_list_with_tail t []

  end;;
 module Concat_list :
  sig
    type 'a t
    val empty : 'a t
    val singleton : 'a -> 'a t
    val concat : 'a t -> 'a t -> 'a t
    val to_list : 'a t -> 'a list
  end
```

The details of the implementation don't matter so much, but it's
important to note that a `Concat_list.t` is unquestionably an
immutable value.  However, when it comes to the value restriction,
OCaml treats it as if it were mutable.

```ocaml
# Concat_list.empty;;
- : 'a Concat_list.t = <abstr>
# identity Concat_list.empty;;
- : '_a Concat_list.t = <abstr>
```

The issue here is that the signature, by virtue of being abstract, has
obscured the fact that `Concat_list.t` is in fact an immutable
data-type.  We can resolve this in one of two ways: either by making
the type concrete (_i.e._, exposing the implementation in the `mli`),
which is often not desirable; or by marking the type variable in
question as _covariant_.  We'll learn more about variance and
covariance in [xref](#object-oriented-programming), but for now, you
can think of it as an annotation which can be put in the interface of
a pure data structure.

Thus, if we replace `type 'a t` in the interface with `type +'a t`,
that will make it explicit in the interface that the data-structure
doesn't contain any persistent references to values of type `'a`, at
which point, OCaml can infer polymorphic types for expressions of this
type that are not simple values.

```ocaml
# identity Concat_list.empty;;
- : 'a Concat_list.t = <abstr>
```
