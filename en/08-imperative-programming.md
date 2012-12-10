# Imperative Programming

The OCaml programming language is _functional_, meaning that functions are
first-class values that can be passed around like any other.  However, this
doesn't mean that OCaml programs are _pure_.  The language includes assignment,
mutable values like arrays and strings.  Evaluation order is strict and
sequential.  In principle, you can port many imperative programs directly to
OCaml.  If you find yourself doing this a lot, then OCaml may not be the right
programming language for your problem.  However, there are times when imperative
programming is both appropriate and efficient, and OCaml shines at supporting
programs with both functional and imperative aspects.

To illustrate imperative programming, let's start by implementing a hash table.
Hash tables are an efficient way to implement imperative _dictionaries_.  There
are full-featured implementations of hash tables in Core as well as in the OCaml
standard library.  For illustration, we'll construct just a basic dictionary
using _open hashing_, where the hash table consists of an array of buckets, each
of which contain a linked list of elements.  We'll use regular (pure) OCaml
lists, and an array for the buckets.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module HashMap : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b
  val iter : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
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
    | (k, d) :: _ when k = key -> d
    | _ :: t -> find t
    | [] -> raise Not_found
    in
    find table.(hash_bucket key)

  let iter table ~f =
    for i = 0 to num_buckets - 1 do
      List.iter table.(i) ~f:(fun (key, data) -> f ~key ~data)
    done
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The signature for the `HashMap` declares the type of dictionaries `('a, 'b) t`,
with keys of type `'a` and associated values of type `'b`.  It also includes
three functions.  The `create` function constructs an empty dictionary.  The
`add` function adds a key/value association in the dictionary, by _side-effect_.
That is, the hash table is modified _in place_, and any references to the table
will be able to observe the change.  Furthermore, the `add` method doesn't
return a useful value; the type `unit` contains just the trivial value `()`,
which is used by convention to represent "nothing."  The `find` function returns
the value associated with a key, raising the exception `Not_found` if the table
does not contain the key.  The `iter` function iterates through all of the
elements in the table, applying the function `f` to each element in turn.

The hash table is implemented as an array of buckets (the array is fixed-size,
in this example).  The OCaml runtime provides a builtin polymorphic hash
function `Hashtbl.hash` that works for almost any value in OCaml, excluding
functions and abstract values like C data.  The `create` function creates a new
array where all buckets are empty.  The `add` function uses the hash function to
determine the appropriate bucket, then adds a new key/value association to the
bucket through an array _assignment_ `table.(index) <- (key, data) ::
table.(index)`, which _replaces_ the bucket with a new one where the new
key/value pair is added to the front of the list.  The `find` function performs
a linear search through the appropriate bucket to find the value associated with
a key.  The `iter` function iterates through each of the elements in the
buckets.

## Imperative operations

This example illustrates _one_ of the mutating operations in OCaml:
array field assignment.  There are just three others: the contents of
a string can be mutated, and so can record and object fields that have
been declared as `mutable`.

* `array.(index) <- expr`: Array field assignment.  See also the `Array.blit`
  functions for mutating multiple fields at once.

* `string.[index] <- char`: String element assignment.  See also the
  `String.blit` functions for mutating substrings.

* `record.label <- expr`: Record field assignment.  The field `label` must
   be declared as `mutable`.
   
* `object.label <- expr`: Object field assignment.  The field `label` must
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

## Looping

The `iter` function iterates through all of the elements in the table using a
`for` loop.  There are two kinds of loops in OCaml, `for` loops and `while`
loops.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
for index = <initial> to <final> do <body> done
for index = <initial> downto <final> do <body> done
while <condition> do <body> done
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A loop `for index = <initial> to <final> do <body> done` advances by
from the `<initial>` integer to the `<final>` one (inclusive).  The
iterations are evaluated if `<final>` is smaller than `<initial>`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# for i = 0 to 3 do
    Printf.printf "i = %d\n" i
  done;;
  i = 0
i = 1
i = 2
i = 3
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A downto loop `for = <initial> downto <final> do <body> done` advances
downward by 1 on each iteration.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# for i = 3 downto 0 do Printf.printf "i = %d\n" i done;;
i = 3
i = 2
i = 1
i = 0
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A while-loop iterates until the condition is false.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# let i = ref 0;;
val i : int ref = {contents = 0}
# while !i < 3 do Printf.printf "i = %d\n" !i; i := !i + 1 done;;
i = 0
i = 1
i = 2
- : unit = ()
# while !i < 3 do Printf.printf "i = %d\n" !i; i := !i + 1 done;;
- : unit = ()
# !i;;  
- : int = 3
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
# List.iter  [1; 2; 3] ~f:(fun i -> Printf.printf "Element: %d\n" i);;
Element: 1
Element: 2
Element: 3
- : unit = ()
# List.map [1; 2; 3] ~f:((+) 10);;
- : int list = [11; 12; 13]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defining this for doubly-linked lists is simple enough.  The following function
iterates through the list, applying the function `f` to each element in turn.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
  DList.iter l ~f:(Printf.printf "Item: %d\n");;
Item: 3
Item: 2
Item: 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This style of iteration is concise and completely general.  However, with
imperative containers, we often want more control.  We may not want to iterate
through all elements, and we often want to mutate the container as we iterate.
One conventional way to do this is to define a generic `iterator` type that can
be used to enumerate and/or mutate the elements in a container.  This is a style
seen, for example, in Java (type `Iterator`) or the C++ Standard Template
Library.

Let's define a Java-style kind of generic iterator object that allows manual
enumeration and mutation of the container.  Here is the Java interface.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {java}
public iterface Iterator {
  public boolean hasNext();
  public Object next();
  public void remove();
};
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At any time, a `Iterator` object refers (optionally) to some element of a
container.  The `hasNext()` method returns true if the iterator refers to an
element; the method `next()` returns the element, and also advances to the next
one; and `remove()` removes the last element returned by the iterator.

When we define a similar iterator concept in OCaml, we need to choose how to
represent it.  We _could_ define a separate iterator type for each kind of
container, but this would be inconvenient, since iterators have similar behavior
for many different kinds of containers.  To define a _generic_ iterator, there
are several reasonable choices: we can use first-class modules, or we can use
objects.  The simpler approach is to use objects.

You can skip forward to the Objects chapter for more informatation about
objects, but we'll be using basic objects, which are just collections of
methods, similar to having a record of functions -- we could also implement the
iterator as a record of functions, but the code would be somewhat more verbose.

First, we need to define a generic iterator type.  For clarity, we'll use a more
verbose type than in Java.  We'll separate retrieving a value from advacing to
the next element.  The object type is specified like a record type, but using
angle brackets `< ... >`.

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
# let l = create ();;
val l : '_a dlist
# push_front l 1;
  push_front l 2;
  push_front l 3;
  iter (Printf.printf "Item: %d\n") l;;
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
# let l2 = create();
val l2 : '_a dlist
# push_front l2 1; push_front l2 3;;
- : unit = ()
# l == l2;;
- : bool = false
# l = l2;;
Out of memory during evaluation.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Doubly-linked list module

Now that we have defined iterators, let's declare the complete signature for
doubly-linked lists as a module.  The type of elements `'a element` is internal
to the implementation, and the type of lists `'a DList.t` is abstract.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module DList : sig
   type 'a t

   val create : unit -> 'a t
   val is_empty : 'a t -> bool
   val push_front : 'a t -> data:'a -> unit
   val front : 'a t -> 'a
   val pop_front : 'a t -> 'a
   val iter : ('a -> unit) -> 'a t -> unit
   val iterator : 'a t -> 'a iterator
   val find : 'a t -> data:'a -> 'a iterator
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have seen the definition of all of the functions except `find`, which searches
for an element in the list (sequentially), returning an iterator that refers to
that element if it exists.  The implementation simply creates an iterator, then
uses a loop to search sequentially for the element.  If the element is found,
the returned iterator refers to that value, otherwise the iterator does not have
a value.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module DList = struct
   ...

   let find l ~data =
     let it = iterator l in
     while it#has_value && it#value <> data do
        it#next
     done;
     it
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Hash tables with iterators

Let's return to the example of hash tables, but this time let's define an
iterator-style interface.  We'll use the same `iterator` object type as we did
for doubly-linked lists, but this time the iteration is over key/value pairs.
The signature changes slightly, the main change being tat the `find` function
returns an iterator.  This allows retrieval of the value associated with a key,
and it also allows the entry to be deleted.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module IterableHashMap : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iterator : ('a, 'b) t -> ('a * 'b) iterator
  val find : ('a, 'b) t -> key:'a -> ('a * 'b) iterator
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The implementation of `IterableHashMap` is similar to the original `HashMap`
using lists, except now we will use doubly-linked lists.  The `create` function
creates an array of doubly-linked lists.  The `add` function first removes any
existing entry, then add the new element to the front of the bucket.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module IterableHashMap = struct
  type ('a, 'b) t = ('a * 'b) DList.t array

  let num_buckets = 17
  let hash_bucket key = (Hashtbl.hash key) mod num_buckets

  let create () = Array.init num_buckets (fun _ -> DList.create ())

  let add table ~key ~data =
    let index = hash_bucket key in
    let it = DList.find table.(index) ~data:(key, data) in
    if it#has_value then it#remove;
    DList.push_front table.(index) (key, value)

  ...
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can define iterators in the hash table as a pair of a bucket index and
`DList` iterator into the bucket.  To define this as an object, we'll introduce
a few more object concepts, including mutable fields, private methods, and
initializers.  The function `make_iterator table index_ dlist_it_` returns an
iterator for the bucket with index `index_` and list iterator `dlist_it_`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The iterator implementation relies on a "normal" form, where the list iterator
_always_ refers to an element.  This is handled by the `normalize` method, which
advances past empty buckets until either a non-empty bucket is found, or the end
of the table is reached.

The `normalize` method is declared a `private`, so that it does not appear as
part of the iterator type.  The `has_value` and `value` methods delagate
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

Now that the iterator is defined, we can complete the `IterableHashMap`
implementation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `iterator` function returns in iterator that refers to the first element in
the table (if the table is non-empty).  The `find` function searches for an
element in the table, returning an iterator referring to that value if found, or
else the an iterator at the end of the table.

## Lazy computation

There are many instances where imperative programming is used to change or
improve the performance characteristics of a program, without otherwise changing
the behavior.  In other words, the program could be written without
side-effects, but performance is improved by techniques like lazy computation,
caching, memoization, etc.

One of the simplest of these is the builtin lazy computation.  The keyword
`lazy` can be used to prefix any expression, returning a value of type `'a
Lazy.t`.  The computation is delayed until forced with the `Lazy.force`
function, and then saved thereafter.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# let v = lazy (print_string "performing lazy computation\n"; 1);;
val v : int lazy_t = <lazy>
# Lazy.force v;;
performing lazy computation
- : int = 1
# Lazy.force v;;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The builtin `lazy` computation has a nice syntax, but the technique is pretty
generic, and we can implement it with a mutable value.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module Memo : sig
   type ('a, 'b) t

   val create : unit -> ('a, 'b) t
   val apply : ('a, 'b) t -> func:('a -> 'b) -> arg:'a -> 'b
end = struct
   type ('a, 'b) t = ('a, 'b) HashMap.t

   let create = HashMap.create
   let apply table ~func ~arg =
      try HashMap.find table ~key:arg with
         Not_found ->
            let x = func arg in
            HashMap.add table ~key:arg ~data:x;
            x
end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Memoization is useful for _dynamic programming_, where problems are solved by
breraking them down into simpler subproblems.  If subproblems occur more than
once, memoization can be used to avoid recomputing the subproblem.  A canonical
example of this is the Fibonacci sequence, which is defined by the following
program, which produces the sequence _0, 1, 1, 2, 3, 5, 8, 13, 21, ..._ starting
from 0.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
let rec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2);;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The complexity of this function is exponential _O(2^i)_, because for large
inputs the function computes two similar-sized subproblems.  To illustrate,
let's time the computation using the `Sys.time` function to measure the wall
clock.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# let time f x =
    let start = Sys.time () in
    let y = f x in
    Printf.printf "Time: %g sec\n" (Sys.time () -. start);
    y;;
val time : ('a -> 'b) -> 'a -> 'b = <fun>
# time fib 40;;
Time: 5.53724 sec
- : int = 102334155
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Next, let's construct a memoized version of the function, where the recursive
calls are made through a memo table.  This makes a dramatic improvement in
performance.  Since the recursive calls are computed just once, the complexity
is linear, and the computation is fast.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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

  let table = HashMap.create ()
  let merge exp =
     try HashMap.find table ~key:exp with
        Not_found ->
           HashMap.add table ~key:exp ~data:exp;
           exp

  let num i = merge (Num i)
  let var s = merge (Var s)
  let plus e1 e2 = merge (Plus (e1, e2))
  let times e1 e2 = merge (Times (e1, e2))
end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The implementation defines a hash table `table`, and a `merge` function that
merges an expression into the table, returning the previous value if there was
one, or inserting a new value if there is not.  The constructors can rely on the
fact that subexpressions have already been hash-consed, so they simply call the
merge function to memoize the value.

Note that expressions that are structurally equal are now also physically equal.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
# let e1 = Exp.times (Exp.num 10) (Exp.plus (Exp.var "x") (Exp.var "y"));;
val e1 : Exp.t = Exp.Times (Exp.Num 10, Exp.Plus (Exp.Var "x", Exp.Var "y"))
# let e2 = Exp.times (Exp.num 10) (Exp.plus (Exp.var "x") (Exp.var "y"));;
val e2 : Exp.t = Exp.Times (Exp.Num 10, Exp.Plus (Exp.Var "x", Exp.Var "y"))
# e1 == e2;;
- : bool = true
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Expressions that are not equal are equal are not physically equal either,
however common subexpressions are equal.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml-toplevel}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The reference cell `value` holds a shared value that is incremented 10
times, in a loop, by the `loop` function.  Each iteration of the loop
prints the current value, then assigns the new value.  We create two
threads with the `Thread.create` expressions, then use `Thread.join`
to block until the threads terminate.

The exact behavior of the program is nondeterminstic -- it depends on
the relative sopeed of the two theads.  One output is listed below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
i = 0
ii  ==  01

ii  ==  12

i = 2
value = 3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This represents a nearly perfect interleaving of the thread
executions.  Each thread fetches the value from the `value` reference
cell, prints the value, then performs the assignment.  Since both
threads effectively run in lockstep, the final value in the `value`
reference cell is the same as if there were just one thread running.

If this is not the behavior that was expected, one solution is to use
a `Mutex` to ensure that the increment operation is atomic.  We can
allocate a lock with `Mutex.create`, then acquire the lock in the loop
body.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we run this program, it produces a deterministic output.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
i = 0
i = 1
i = 2
i = 3
i = 4
i = 5
value = 6
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module ConcurrentHashMap : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b
  val remove : ('a, 'b) t -> key:'a -> unit
  val iterator : ('a, 'b) t -> ('a * 'b) iterator
end = struct
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We'll use the same basic construction that we used to implement the
`HashMap` -- a hash table contains an array of buckets.  In addition
we'll add locking to ensure that concurrent operations do not
interfere.  In addition, to reduce lock contention, we'll use an array
of locks to partition the table into multiple parts.  If operations
are randomly disitribted, this should reduce lock contention.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each `element` is a key/value pair, where the value is mutable so that
the `add` function can mutate it in place.  For this implementation,
we'll use 32 locks, and start with 256 buckets.

Each bucket is an _association list_, meaning that it is list of
key/value pairs that implement a dictionary.  We can start the
implementation by defining dictionary operations for association
lists.  The function `find_assoc` finds the value associated with a
key, and `remove_assoc` removes an association.  Both functions raise
an exception `Not_found` if the list does not contain the association.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
  let rec find_assoc key = function
  | { key = key' } as element :: _ when key' = key -> element
  | _ :: tl -> find_assoc key tl
  | [] -> raise Not_found

  let rec remove_assoc key = function
  | { key = key' } :: tl when key' = key -> tl
  | hd :: tl -> hd :: remove_assoc key tl
  | [] -> raise Not_found
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The locks are intended to partition the table into multiple sub-parts,
where each lock provides synchronization for a contiguous range of
buckets.  To make synchronization each we define a function
`synchronize` that takes a bucket index and a function, and evaluates
the function with the bucket lock acquired, releasing the lock before
returning.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
  let synchronize table index f =
    let lock = table.locks.(index * num_locks / num_buckets) in
    Mutex.lock lock;
    let result = f () in
    Mutex.unlock lock;
    result
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the `synchronize` function is _not_ exception-safe, meaning
that if evaluation of `f ()` raises an exception, the lock will not be
released.  An exception-safe version would catch all exceptions; when
an exception is raised, the lock would be released, and the exception
re-raised.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
  let synchronize_exn table index f =
    let lock = table.locks.(index * num_locks / num_buckets) in
    Mutex.lock lock;
    try let result = f () in Mutex.unlock lock; result with
      exn -> Mutex.unlock lock; raise exn
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To add a new entry to the table, the `add` function acquires the
bucket lock, then uses `find_assoc` to look for an existing
association.  If one is found, the `value` is updated in-place to the
new value.  Otherwise, a new entry is added to the beginning of the
bucket.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
  let add table ~key ~data =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    let buckets = table.buckets in
    synchronize table index (fun () ->
      try (find_assoc key buckets.(index)).value <- data with
        Not_found ->
          buckets.(index) <- { key = key; value = data } :: buckets.(index))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Removing an element from the table is similar.  If here is a previous
entry in the table, the entry is removed.  Otherwise, the table is
left unchanged.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
  let remove table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    let buckets = table.buckets in
    synchronize table index (fun () ->
      try buckets.(index) <- remove_assoc key buckets.(index) with
        Not_found -> ())
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The function to find an association in the table is similar -- we jsut
find the entry in the table and return the value part.  However, this
particular implementation is somewhat more subtle, because it omits
the synchronization step, examining the bucket _without_ acquiring the
lock.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
  let find table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
	(* Unsynchronized! *)
    (find_assoc key table.buckets.(index)).value
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
  let synchronized_find table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    synchronize table index (fun () ->
      (find_assoc key table.buckets.(index)).value)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the final part of the implementation, let's define imperative
iteration.  The iterator object contains a bucket index, and the field
`elements` refers to some suffix of the list stored in the bucket.
The `value` method returns the current elements, and the `next` method
advances the `elements` field.  The method `normalize` is used to
maintain the invariant that the `elements` field always refers to a
value in the table unless the iterator has advanced past the final
element.  The `remove` method removes the current element from the
bucket in which it is stored.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
  let rec remove_element elements = function
  | (_ :: tl) as elements' when elements' == elements -> tl
  | hd :: tl -> hd :: remove_element elements tl
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
          try buckets.(index) <- remove_element elements buckets.(index) with
            Not_found -> ());
        self#next
      method private normalize =
        while elements = [] && index < num_buckets do
          index <- index + 1;
	  elements <- buckets.(index)
        done
      initializer self#normalize
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All method are unsychronized except the method `remove`, which mutates
the bucket.  As a consequence, it means that hash operations that add
and remove elements from the list can happen concurrently with
iteration.  Again, this is great from a performance perspective, but
it means that iteration has non-sequential semantics.  In particular,
whenever iteration enters a new bucket, subsequent concurrent
operations that add new elements or remove old ones have _no effect_
on the iteration.  Iteration advances through that bucket as if it
were unchanged.

One advantage of this relaxed iteration semantics is peformance, since
iteration is largely unsynchronized.  Another advantage is that
deadlock is less likely.  If we were to _lock_ the bucket during
iteration, then changes to that bucket would not be allowed during
iteration (even by the iterating thread).  We might allow lock
recursion to allow mutations by the iterating thread, but in general
the synchronization might involve multiple threads, resulting in
deadlock.  Lock-free iteration ensures that the `ConcurrentHashMap`
will not be involved in a deadlock cycle.

## Summary

The OCaml language supports a fairly standard imperative programming model, with
looping, assignment, mutable arrays, records, and objects.  If desired, we can
write programs that correspond directly to what we would have written in some
other imperative language like C or Java.  Of course, doing so is really not the
best match -- if you want to write imperative programs, you should probably use
an imperative programming language.

However, there are times when imperative programming might provide
efficiency (as with lazy evaluation, or memoization), or you might
require techniques or data structures that are traditional imperative
(like graphs represented with adjacency lists), and in these cases
OCaml usually shines.  Used with discretion, imperative programming
can lead to smaller, simpler programs.
