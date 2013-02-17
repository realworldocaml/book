# Java-style iteration

One downside of navigating through a list using the `next` and `prev`
functions is that it ties your code specifically to the `Dlist`
module.  So, code you write to iterate over a double-linked list can't
be used to iterate over an array, or a hash table.

Imperative object oriented languages like Java or C++ deal with this
situation by defining iterators to enumerate and/or mutate the
elements in multiple different kinds of containers.  We can
essentially do the same thing in OCaml.  There are multiple approaches
to this, but one simple and convenient one is to use OCaml's object
system.

First, let's define the type of the iterator object.

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

## Hash tables with iterators

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

