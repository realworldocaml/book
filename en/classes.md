# Classes

Programming with objects directly is great for encapsulation, but one
of the main goals of object-oriented programming is code re-use
through inheritance.  For inheritance, we need to introduce _classes_.
In object-oriented programming, a class is a "recipe" for creating
objects.  The recipe can be changed by adding new methods and fields,
or it can be changed by modifying existing methods.

## OCaml Classes

In OCaml, class definitions must be defined as toplevel statements in
a module.  A class is not an object, and a class definition is not an
expression.  The syntax for a class definition uses the keyword
`class`.

```ocaml
# class point =
  object
    val mutable x = 0
    method get = x
    method set y = x <- y
  end;;
class point :
  object
    val mutable x : int
    method get : int
    method set : int -> unit
  end
```

The type `class point : ... end` is a _class type_.  This particular
type specifies that the `point` class defines a mutable field `x`, a
method `get` that returns an `int`, and a method `set` with type `int
-> unit`.

To produce an object, classes are instantiated with the keyword `new`.

```ocaml
# let p = new point;;
val p : point = <obj>
# p#get;;
- : int = 0
# p#set 5;;
- : unit = ()
# p#get;;
- : int = 5
```

Inheritance uses an existing class to define a new one.  For example,
the following class definition supports an addition method `moveby`
that moves the point by a relative amount.

```ocaml
# class movable_point =
  object (self : 'self)
    inherit point
    method moveby dx = self#set (self#get + dx)
  end;;
class movable_point :
  object
    val mutable x : int
    method get : int
    method moveby : int -> unit
    method set : int -> unit
  end
```

This new `movable_point` class also makes use of the
`(self : 'self)` binding after the `object` keyword.  The variable
`self` stands for the current object, allowing self-invocation, and
the type variable `'self` stands for the type of the current object
(which in general is a subtype of `movable_point`).


##  An Example: Cryptokit

Let's take a break from describing the object system with a more
practical example that uses the OCaml cryptographic library.

<note>
<title>Installing the Cryptokit library</title>

The Cryptokit library can be installed via OPAM via `opam install cryptokit`.
Once that's finished compiling and installing, you just need to `#require
"cryptokit"` in your toplevel to load the library and make the modules
available.

</note>

Our first example mimics the `md5` command, which reads in an input
file and returns a hexadecimal representation of its MD5 cryptographic hash.
Cryptokit defines a number of different functions and collects them together
under the `Cryptokit.hash` class type:

```ocaml
class type hash = object
  method add_byte : int -> unit
  method add_char : char -> unit
  method add_string : string -> unit
  method add_substring : string -> int -> int -> unit
  method hash_size : int
  method result : string
  method wipe : unit
end

val hash_string : hash -> string -> string
```

Concrete hash objects can be instantiated from various sub-modules in Cryptokit.  The simplest ones such as MD5 or SHA1 do not take any special input parameters to build the object. The `hmac_sha1` takes a string key to initialise the Message Authenticate Code for that particular hash function.

```ocaml
# Cryptokit.Hash.md5;;
- : unit -> Cryptokit.hash = <fun>
# Cryptokit.Hash.sha1;;
- : unit -> Cryptokit.hash = <fun>
# Cryptokit.MAC.hmac_sha1;;
- : string -> Cryptokit.hash = <fun>
```

Hash objects hold state and are thus naturally imperative. Once instantiated, data is fed into them by the addition functions, the `result` is computed and finally the contents erased via `wipe`.
The `hash_string` convenience function applies the hash function fully to a string, and returns the result.
The `md5` command is quite straight-forward now:

```ocaml
open Core.Std
open Cryptokit

let _ =
  In_channel.(input_all stdin)
  |> hash_string (Hash.md5 ())
  |> transform_string (Hexa.encode ())
  |> print_endline
```

After opening the right modules, we read in the entire standard input into an OCaml string.
This is then passed onto the MD5 hash function, which returns a binary string.
This binary is passed through the `Hexa` hexadecimal encoder, which returns an ASCII
representation of the input.  The output of this command will be the same as the `md5` command (or `md5sum` in some systems).

We can extend this simple example by selecting either the `md5` or `sha1` hash function at runtime depending on the name of our binary.  `Sys.argv` is an array containing the arguments the command was invoked with, and the first entry is the name of the binary itself.

```ocaml
open Core.Std
open Cryptokit

let _ =
  let hash_fn =
    match Filename.basename Sys.argv.(0) with
    |"md5" -> Hash.md5 ()
    |"sha1" -> Hash.sha1 ()
    |_ -> Hash.md5 ()
  in
  In_channel.(input_all stdin)
  |> hash_string hash_fn
  |> transform_string (Hexa.encode ())
  |> print_endline
```

Now let's try something more advanced.  The `openssl` library is installed on most systems, and can be used to encrypt plaintext using several encryption strategies.  At its simplest, it will take a secret phrase and derive an appropriate key and initialisation vector.

```
$ openssl enc -nosalt -aes-128-cbc -base64 -k "ocaml" -P
key=6217C07FF169F6AB2EB2731F855095F1
iv =8164D5477E66E6A9EC99A8D58ACAADAF
```

We've selected the `-nosalt` option here to make the output deterministic, and the `-P` option prints out the derived key and IV and exits.  The algorithm used to derive these results is described in the `man EVP_BytesToKey` manual page (you may need to install the OpenSSL documentation packages on your system first).  We can implement this derivation function using an imperative style:

```ocaml
let md5 s = hash_string (Hash.md5 ()) s

let evp_byte_to_key password tlen =
  let o = Hexa.encode () in
  let v = ref (md5 password) in
  o#put_string !v;
  while o#available_output/2 < tlen do
    let n = md5 (!v ^ password) in
    o#put_string n;
    v := n;
  done;
  String.uppercase o#get_string

let _ =
  let secret = "ocaml" in
  let key_len = 16 * 2 in
  let iv_len = 16 * 2 in
  let x = evp_byte_to_key secret (key_len+iv_len) in
  let key = String.sub x ~pos:0 ~len:key_len in
  let iv = String.sub x ~pos:key_len ~len:iv_len in
  Printf.printf "key=%s\niv =%s\n%!" key iv
```

The derivation algorithm takes an input password and desired total length (the addition of the key and IV length).
It initialises a `Hexa.encode` transformer, which will accept arbitrary binary data and output a hexadecimal string (with two output bytes per input byte).  A reference stores the last digest that's been calculated, and then the algorithm iterates until it has sufficient data to satisfy the required key length.

Notice how the encoder object is used as an accumulator, by using the `put_string` and `available_output` to keep track of progress.  Objects don't *require* an imperative style though, and the same algorithm can be written more functionally:

```ocaml
let evp_byte_to_key password tlen =
  let rec aux acc v =
    match String.length acc < tlen with
    | true ->
      let v = md5 (v ^ password) in
      aux (acc^v) v
    | false -> acc
  in
  let v = md5 password in
  String.uppercase (transform_string (Hexa.encode ()) (aux v v))
```

In this version, we don't use any references, and instead a recursive function keeps track of the last digest in use and the accumulated result string.  This version isn't quite as efficient as the previous one due to the careless use of string concatenation for the accumulator, but this can easily be fixed by using the `Buffer` module instead.

## Class parameters and polymorphism ##

A class definition serves as the _constructor_ for the class.  In
general, a class definition may have parameters that must be provided
as arguments when the object is created with `new`.

Let's build an example of an imperative singly-linked list using
object-oriented techniques.  First, we'll want to define a class for a
single element of the list.  We'll call it a `node`, and it will hold
a value of type `'a`.  When defining the class, the type parameters
are placed in square brackets before the class name in the class
definition.  We also need a parameter `x` for the initial value.

```ocaml
class ['a] node x =
object
  val mutable value : 'a = x
  val mutable next_node : 'a node option = None

  method get = value
  method set x = value <- x

  method next = next_node
  method set_next node = next_node <- node
end;;
```

The `value` is the value stored in the node, and it can be retrieved
and changed with the `get` and `set` methods.  The `next_node` field
is the link to the next element in the stack.  Note that the type
parameter `['a]` in the definition uses square brackets, but other
uses of the type can omit them (or use parentheses if there is more
than one type parameter).

The type annotations on the `val` declarations are used to constrain
type inference.  If we omit these annotations, the type inferred for
the class will be "too polymorphic," `x` could have some type `'b` and
`next_node` some type `'c option`.

```ocaml
  class ['a] node x =
  object
    val mutable value = x
    val mutable next_node = None

    method get = value
    method set x = value <- x

    method next = next_node
    method set_next node = next_node <- node
  end;;
Error: Some type variables are unbound in this type:
         class ['a] node :
           'b ->
           object
             val mutable next_node : 'c option
             val mutable value : 'b
             method get : 'b
             method next : 'c option
             method set : 'b -> unit
             method set_next : 'c option -> unit
           end
       The method get has type 'b where 'b is unbound
```

In general, we need to provide enough constraints so that the compiler
will infer the correct type.  We can add type constraints to the
parameters, to the fields, and to the methods.  It is a matter of
preference how many constraints to add.  You can add type constraints
in all three places, but the extra text may not help clarity.  A
convenient middle ground is to annotate the fields and/or class
parameters, and add constraints to methods only if necessary.

Next, we can define the list itself.  We'll keep a field `head` that
refers to the first element in the list, and `last` that refers to the
final element in the list.  The method `insert` adds an element to the
end of the list.

```ocaml
class ['a] slist =
object
   val mutable first : ('a) node option = None
   val mutable last : ('a) node option = None

   method is_empty = first = None

   method insert x =
      let new_node = Some (new node x) in
      match last with
         Some last_node ->
            last_node#set_next new_node;
            last <- new_node
       | None ->
            first <- new_node;
            last <- new_node
end;;
```

## Object types ##

This definition of the class `slist` is not complete, we can construct
lists, but we also need to add the ability to traverse the elements in
the list.  One common style for doing this is to define a class for an
`iterator` object.  An iterator provides a generic mechanism to
inspect and traverse the elements of a collection.  This pattern isn't
restricted to lists, it can be used for many different kinds of
collections.

There are two common styles for defining abstract interfaces like
this.  In Java, an iterator would normally be specified with an
interface, which specifies a set of method types.  In languages
without interfaces, like C++, the specification would normally use
_abstract_ classes to specify the methods without implementing them
(C++ uses the "= 0" definition to mean "not implemented").

```java
// Java-style iterator, specified as an interface.
interface <T> iterator {
  T Get();
  boolean HasValue();
  void Next();
};

// Abstract class definition in C++.
template<typename T>
class Iterator {
 public:
  virtual ~Iterator() {}
  virtual T get() const = 0;
  virtual bool has_value() const = 0;
  virtual void next() = 0;
};
```

OCaml support both styles.  In fact, OCaml is more flexible than these
approaches because an object type can be implemented by any object
with the appropriate methods; it does not have to be specified by the
object's class _a priori_.  We'll leave abstract classes for later.
Let's demonstrate the technique using object types.

First, we'll define an object type `iterator` that specifies the
methods in an iterator.

```ocaml
type 'a iterator = < get : 'a; has_value : bool; next : unit >;;
```

Next, we'll define an actual iterator for the class `slist`.  We can
represent the position in the list with a field `current`, following
links as we traverse the list.

```ocaml
class ['a] slist_iterator cur =
object
  val mutable current : 'a node option = cur

  method has_value = current <> None

  method get =
     match current with
        Some node -> node#get
      | None -> raise (Invalid_argument "no value")

  method next =
     match current with
        Some node -> current <- node#next
      | None -> raise (Invalid_argument "no value")
end;;
```

Finally, we add a method `iterator` to the slist class to produce an
iterator.  To do so, we construct an `slist_iterator` that refers to
the first node in the list, but we want to return a value with the
object type `iterator`.  This requires an explicit coercion using the
`:>` operator.

```ocaml
class ['a] slist = object
...
   method iterator = (new slist_iterator first :> 'a iterator)
end

# let l = new slist;;
# l.insert 5;;
# l.insert 4;;
# let it = l#iterator;;
# it#get;;
- : int = 5
# it#next;;
- : unit = ()
# it#get;;
- : int = 4
# it#next;;
- : unit = ()
# it#has_value;;
- : bool = false
```

We may also wish to define functional-style methods, `iter f` takes a
function `f` and applies it to each of the elements of the list.

```ocaml
method iter f =
  let it = self#iterator in
  while it#has_value do
    f it#get
    it#next
  done
```

What about functional operations similar to `List.map` or `List.fold`?
In general, these methods take a function that produces a value of
some other type than the elements of the set.  For example, the
function `List.fold` has type `'a list -> ('b -> 'a -> 'b) -> 'b ->
'b`, where `'b` is an arbitrary type.  To replicate this in the
`slist` class, we need a method type `('b -> 'a -> 'b) -> 'b -> 'b`,
where the method type is polymorphic over `'b`.

The solution is to use a type quantifier, as shown in the following
example.  The method type must be specified directly after the method
name, which means that method parameters must be expressed using a
`fun` or `function` expression.

```ocaml
method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
   (fun f x ->
         let y = ref x in
         let it = self#iterator in
         while it#has_value do
            y := f !y it#get;
            it#next
         done;
         !y)
```

## Class types ##

Once we have defined the list implementation, the next step is to wrap
it in a module or `.ml` file and give it a type so that it can be used
in the rest of our code.  What is the type?

Before we begin, let's wrap up the implementation in an explicit
module (we'll use explicit modules for illustration, but the process
is similar when we want to define a `.mli` file).  In keeping with the
usual style for modules, we define a type `'a t` to represent the type
of list values.

```ocaml
module SList = struct
   type 'a iterator = < get : 'a; has_value : bool; next : unit >
   type 'a t = < is_empty : bool; insert : 'a -> unit; iterator : 'a iterator >

   class ['a] node x = object ... end
   class ['a] slist_iterator cur = object ... end
   class ['a] slist = object ... end

   let make () = new slist
end;;
```

We have multiple choices in defining the module type, depending on
how much of the implementation we want to expose.  At one extreme, a
maximally-abstract signature would completely hide the class
definitions.

```ocaml
module AbstractSList : sig
   type 'a iterator = < get : 'a; has_value : bool; next : unit >
   type 'a t = < is_empty : bool; insert : 'a -> unit; iterator : 'a iterator >

   val make : unit -> 'a t
end = SList
```

The abstract signature is simple because we ignore the classes.  But
what if we want to include them in the signature, so that other modules
can inherit from the class definitions?  For this, we need to specify
types for the classes, called _class types_.  Class types do not
appear in mainstream object-oriented programming languages, so you may
not be familiar with them, but the concept is pretty simple.  A class
type specifies the type of each of the visible parts of the class,
including both fields and methods.  Just like for module types, you
don't have to give a type for everything; anything you omit will be
hidden.

```ocaml
module VisibleSList : sig
  type 'a iterator = < get : 'a; has_value : bool; next : unit >
  type 'a t = < is_empty : bool; insert : 'a -> unit; iterator : 'a iterator >

  class ['a] node : 'a ->
  object
     method get : 'a
     method set : 'a -> unit
     method next : 'a node option
     method set_next : 'a node option -> unit
  end

  class ['a] slist_iterator : 'a node option ->
  object
     method has_value : bool
     method get : 'a
     method next : unit
  end

  class ['a] slist :
  object
    val mutable first : 'a node option
    val mutable last : 'a node option
    method is_empty : bool
    method insert : 'a -> unit
    method iterator : 'a iterator
  end

  val make : unit -> 'a slist
end = SList
```

In this signature, we've chosen to make nearly everything visible.
The class type for `slist` specifies the types of the fields `first`
and `last`, as well as the types of each of the methods.  We've also
included a class type for `slist_iterator`, which is of somewhat more
questionable value, since the type doesn't appear in the type for
`slist` at all.

One more thing, in this example the function `make` has type `unit ->
'a slist`.  But wait, we've stressed _classes are not types_, so
what's up with that?  In fact, what we've said is entirely true,
classes and class names *are not* types.  However, class names can be
used to stand for types.  When the compiler sees a class name in type
position, it automatically constructs an object type from it by
erasing all the fields and keeping only the method types.  In this
case, the type expression `'a slist` is exactly equivalent to `'a t`.

### Binary methods ###

A _binary method_ is a method that takes an object of `self` type.
One common example is defining a method for equality.

```ocaml
# class square w =
  object (self : 'self) 
    method width = w
    method area = self#width * self#width
    method equals (other : 'self) = other#width = self#width
  end;;
class square : int ->
  object ('a)
    method area : int
    method equals : 'a -> bool
    method width : int
  end
# class rectangle w h =
  object (self : 'self)
    method width = w
    method height = h
    method area = self#width * self#height
    method equals (other : 'self) = other#width = self#width && other#height = self#height
  end;;
...
# (new square 5)#equals (new square 5);;
- : bool = true
# (new rectangle 5 6)#equals (new rectangle 5 7);;
- : bool = false
```

This works, but there is a problem lurking here.  The method `equals`
takes an object of the exact type `square` or `rectangle`.  Because of
this, we can't define a common base class `shape` that also includes
an equality method.

```ocaml
# type shape = < equals : shape -> bool; area : int >;;
# let sq = new square 5;;
# (sq :> shape);;
Characters 0-13:
  (sq :> shape);;
  ^^^^^^^^^^^^^
Error: Type square = < area : int; equals : square -> bool; width : int >
       is not a subtype of shape = < area : int; equals : shape -> bool > 
Type shape = < area : int; equals : shape -> bool > is not a subtype of
  square = < area : int; equals : square -> bool; width : int > 
```

The problem is that a `square` expects to be compared with a `square`,
not an arbitrary shape; similarly for `rectangle`.

This problem is fundamental.  Many languages solve it either with
narrowing (with dynamic type checking), or by method overloading.
Since OCaml has neither of these, what can we do?

One proposal we could consider is, since the problematic method is
equality, why not just drop it from the base type `shape` and use
polymorphic equality instead?  Unfortunately, the builtin equality
has very poor behavior when applied to objects.

```ocaml
# (object method area = 5 end) = (object method area = 5 end);;
- : bool = false
```

The problem here is that the builtin polymorphic equality compares the
method implementations, not their return values.  The method
implementations (the function values that implement the methods) are
different, so the equality comparison is false.  There are other
reasons not to use the builtin polymorphic equality, but these false
negatives are a showstopper.

If we want to define equality for shapes in general, the remaining
solution is to use the same approach as we described for narrowing.
That is, introduce a _representation_ type implemented using variants,
and implement the comparison based on the representation type.

```ocaml
type shape_repr =
 | Square of int
 | Circle of int
 | Rectangle of int * int;;
 
type shape = < repr : shape_repr; equals : shape -> bool; area : int >;;

class square w =
object (self : 'self)
  method width = w
  method area = self#width * self#width
  method repr = Square self#width
  method equals (other : shape) = self#repr = other#repr
end;;
```

The binary method `equals` is now implemented in terms of the concrete
type `shape_repr`.  In fact, the objects are now isomorphic to the
`shape_repr` type.  When using this pattern, you will not be able to
hide the `repr` method, but you can hide the type definition using the
module system.

```ocaml
module Shapes : sig
  type shape_repr
  type shape = < repr : shape_repr; equals : shape -> bool; area -> int >
  
  class square : int ->
    object
	  method width : int
	  method area : int
	  method repr : shape_repr
	  method equals : shape -> bool
	end
end = struct
  type shape_repr = Square of int | Circle of int | Rectangle of int * int
  ...
end;;
```

## Private methods ##

Methods can be declared _private_, which means that they may be
called by subclasses, but they are not visible otherwise (similar to a
_protected_ method in C++).

To illustrate, let's build a class `vector` that contains an array of
integers, resizing the storage array on demand.  The field `values`
contains the actual values, and the `get`, `set`, and `length` methods
implement the array access.  For clarity, the resizing operation is
implemented as a private method `ensure_capacity` that resizes the
array if necessary.

```ocaml
# class vector =
  object (self : 'self)
     val mutable values : int array = [||]
  
     method get i = values.(i)
     method set i x =
        self#ensure_capacity i;
        values.(i) <- x
     method length = Array.length values
  
     method private ensure_capacity i =
        if self#length <= i then
           let new_values = Array.create (i + 1) 0 in
           Array.blit values 0 new_values 0 (Array.length values);
           values <- new_values
  end;;
# let v = new vector;;
# v#set 5 2;;
# v#get 5;;
- 2 : int
# v#ensure_capacity 10;;
Characters 0-1:
  v#ensure_capacity 10;;
  ^
Error: This expression has type vector
       It has no method ensure_capacity
```

To be precise, the method `ensure_capacity` is part of the class type,
but it is not part of the object type.  This means the object `v` has
no method `ensure_capacity`.  However, it is available to subclasses.
We can extend the class, for example, to include a method `swap` that
swaps two elements.

```ocaml
# class swappable_vector =
  object (self : 'self)
     inherit vector

     method swap i j =
        self#ensure_capacity (max i j);
        let tmp = values.(i) in
        values.(i) <- values.(j);
        values.(j) <- tmp
  end;;
```

Yet another reason for private methods is to factor the implementation
and support recursion.  Moving along with this example, let's build a
binary heap, which is a binary tree in heap order: where the label of
parent elements is smaller than the labels of its children.  One
efficient implementation is to use an array to represent the values,
where the root is at index 0, and the children of a parent node at
index `i` are at indexes `2 * i` and `2 * i + 1`.  To insert a node
into the tree, we add it as a leaf, and then recursively move it up
the tree until we restore heap order.

```ocaml
class binary_heap =
object (self : 'self)
   val values = new swappable_vector

   method min =
      if values#length = 0 then
         raise (Invalid_argument "heap is empty");
      values#get 0

   method add x =
      let pos = values#length in
      values#set pos x;
      self#move_up pos

   method private move_up i =
      if i > 0 then
         let parent = (i - 1) / 2 in
            if values#get i < values#get parent then begin
               values#swap i parent;
               self#move_up parent
            end
end;;
```

The method `move_up` implements the process of restoring heap order as
a recursive method (though it would be straightforward avoid the
recursion and use iteration here).

The key property of private methods is that they are visible to
subclasses, but not anywhere else.  If you want the stronger guarantee
that a method is _really_ private, not even accessible in subclasses,
you can use an explicit typing that omits the method.  In the following
code, the `move_up` method is explicitly omitted from the object type,
and it can't be invoked in subclasses.

```ocaml
# class binary_heap :
  object
    method min : int
	method add : int -> unit
  end =
  object (self : 'self) {
    ...
	method private move_up i = ...
  end;;
```

## Virtual classes and methods ##

A _virtual_ class is a class where some methods or fields are
declared, but not implemented.  This should not be confused with the
word "virtual" as it is used in C++.  In C++, a "virtual" method uses
dynamic dispatch, regular non-virtual methods use static dispatched.
In OCaml, _all_ methods use dynamic dispatch, but the keyword
_virtual_ means the method or field is not implemented.

In the previous section, we defined a class `swappable_vector` that
inherits from `array_vector` and adds a `swap` method.  In fact, the
`swap` method could be defined for any object with `get` and `set`
methods; it doesn't have to be the specific class `array_vector`.

One way to do this is to declare the `swappable_vector` abstractly,
declaring the methods `get` and `set`, but leaving the implementation
for later.  However, the `swap` method can be defined immediately.

```ocaml
class virtual abstract_swappable_vector =
object (self : 'self)
   method virtual get : int -> int
   method virtual set : int -> int -> unit
   method swap i j =
      let tmp = self#get i in
      self#set i (self#get j);
      self#set j tmp
end;;
```

At some future time, we may settle on a concrete implementation for the vector.
We can inherit from the `abstract_swappable_bvector` to get the `swap` method "for free."
Here's one implementation using arrays.

```ocaml
class array_vector =
object (self : 'self)
   inherit abstract_swappable_vector

   val mutable values = [||]
   method get i = values.(i)
   method set i x =
      self#ensure_capacity i;
      values.(i) <- x
   method length = Array.length values

   method private ensure_capacity i =
      if self#length <= i then
         let new_values = Array.create (i + 1) 0 in
            Array.blit values 0 new_values 0 (Array.length values);
            values <- new_values
end
```

Here's a different implementation using `HashTbl`.

```ocaml
class hash_vector =
object (self : 'self)
   inherit abstract_swappable_vector

   val table = Hashtbl.create 19

   method get i =
      try Hashtbl.find table i with
         Not_found -> 0

   method set = Hashtbl.add table
end;;
```

One way to view a `virtual` class is that it is like a functor, where
the "inputs" are the declared, but not defined, virtual methods and
fields.  The functor application is implemented through inheritance,
when virtual methods are given concrete implementations.

We've been mentioning that fields can be virtual too.  Here is another
implementation of the swapper, this time with direct access to the
array of values.

```ocaml
class virtual abstract_swappable_array_vector =
object (self : 'self)
   val mutable virtual values : int array
   method private virtual ensure_capacity : int -> unit

   method swap i j =
      self#ensure_capacity (max i j);
      let tmp = values.(i) in
      values.(i) <- values.(j);
      values.(j) <- tmp
end;;
```

This level of dependency on the implementation details is possible,
but it is hard to justify the use of a virtual class -- why not just
define the `swap` method as part of the concrete class?  Virtual
classes are better suited for situations where there are multiple
(useful) implementations of the virtual parts.  In most cases, this
will be public virtual methods.

## Multiple inheritance

When a class inherits from more than one superclass, it is using
_multiple inheritance_.  Multiple inheritance extends the variety of
ways in which classes can be combined, and it can be quite useful,
particularly with virtual classes.  However, it can be tricky to use,
particularly when the inheritance hierarchy is a graph rather than a
tree, so it should be used with care.

### How names are resolved

The main "trickiness" of multiple inheritance is due to naming -- what
happens when a method or field with some name is defined in more than
one class?

If there is one thing to remember about inheritance in OCaml, it is
this: inheritance is like textual inclusion.  If there is more than
one definition for a name, the last definition wins.  Let's look at
some artificial, but illustrative, examples.

First, let's consider what happens when we define a method more than
once.  In the following example, the method `get` is defined twice;
the second definition "wins," meaning that it overrides the first one.

```ocaml
# class m1 =
object (self : 'self)
   method get = 1
   method f = self#get
   method get = 2
end;;
class m1 : object method f : int method get : int end
# (new m1)#f;;
- : int = 2
```

Fields have similar behavior, though the compiler produces a warning
message about the override.

```ocaml
# class m2 =
# class m2 =
  object (self : 'self)
     val x = 1
     method f = x
     val x = 2
  end;;
Characters 69-74:
     val x = 2
         ^^^^^
Warning 13: the instance variable x is overridden.
The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)
class m2 : object val x : int method f : int end
# (new m2)#f;;
- : int = 2
```

Of course, it is unlikely that you will define two methods or two
fields of the same name in the same class.  However, the rules for
inheritance follow the same pattern: the last definition wins.  In the
following definition, the `inherit` declaration comes last, so the
method definition `method get = 2` overrides the previous definition,
always returning 2.

```ocaml
# class m4 = object method get = 2 end;;
# class m5 =
  object
    val mutable x = 1
    method get = x
    method set x' = x <- x'
    inherit m4
  end;;
class m5 : object val mutable x : int method get : int method set : int -> unit end
# let x = new m5;;
val x : m5 = <obj>
# x#set 5;;
- : unit = ()
# x#get;;
- : int = 2
```

To reiterate, to understand what inheritance means, replace each
`inherit` directive with its definition, and take the last definition
of each method or field.  This holds even for private methods.
However, it does _not_ hold for private methods that are "really"
private, meaning that they have been hidden by a type constraint.  In
the following definitions, there are three definitions of the private
method `g`.  However, the definition of `g` in `m8` is not overridden,
because it is not part of the class type for `m8`.

```ocaml
# class m6 =
  object (self : 'self)
     method f1 = self#g
     method private g = 1
  end;;
class m6 : object method f1 : int method private g : int end
# class m7 =
  object (self : 'self)
     method f2 = self#g
     method private g = 2
  end;;
class m7 : object method f2 : int method private g : int end
# class m8 : object method f3 : int end =
  object (self : 'self)
     method f3 = self#g
     method private g = 3
  end;;
class m8 : object method f3 : int end
# class m9 =
  object (self : 'self)
     inherit m6
     inherit m7
     inherit m8
  end;;
# class m9 :
  object
    method f1 : int
    method f2 : int
    method f3 : int
    method private g : int
  end
# let x = new m9;;
val x : m9 = <obj>
# x#f1;;
- : int = 2
# x#f3;;
- : int = 3
```

### Mixins

When should you use multiple inheritance?  If you ask multiple people,
you're likely to get multiple (perhaps heated) answers.  Some will
argue that multiple inheritance is overly complicated; others will
argue that inheritance is problematic in general, and one should use
object composition instead.  But regardless of who you talk to, you
will rarely hear that multiple inheritance is great and you should use
it widely.

In any case, if you're programming with objects, there's one general
pattern for multiple inheritance that is both useful and reasonably
simple, the _mixin_ pattern.  Generically, a _mixin_ is just a virtual
class that implements a feature based on another one.  If you have a
class that implements methods _A_, and you have a mixin _M_ that
provides methods _B_ from _A_, then you can inherit from _M_ --
"mixing" it in -- to get features _B_.

That's too abstract, so let's give an example based on collections.
In Section XXX:Objecttypes, we introduced the _iterator_ pattern,
where an _iterator_ object is used to enumerate the elements of a
collection.  Lots of containers can have iterators, singly-linked
lists, dictionaries, vectors, etc.

```ocaml
type 'a iterator = < get : 'a; has_value : bool; next : unit >;;
class ['a] slist : object ... method iterator : 'a iterator end;;
class ['a] vector : object ... method iterator : 'a iterator end;;
class ['a] deque : object ... method iterator : 'a iterator end;;
class ['a, 'b] map : object ... method iterator : 'b iterator end;;
...
```

The collections are different is some ways, but they share a common
pattern for iteration that we can re-use.  For a simple example, let's
define a mixin that implements an arithmetic sum for a collection of
integers.

```ocaml
# class virtual int_sum_mixin =
  object (self : 'self)
     method virtual iterator : int iterator
     method sum =
        let it = self#iterator in
        let total = ref 0 in
        while it#has_value do
           total := !total + it#get;
           it#next
        done;
        !total
  end;;
# class int_slist =
  object
     inherit [int] slist
	 inherit int_sum_mixin
  end;;
# let l = new int_slist;;
val l : int_slist = <obj>
# l#insert 5;;
# l#insert 12;;
# l#sum;;
- : int = 17
# class int_deque =
  object
     inherit [int] deque
	 inherit int_sum_mixin
  end;;
```

In this particular case, the mixin works only for a collection of
integers, so we can't add the mixin to the polymorphic class
definition `['a] slist` itself.  However, the result of using the
mixin is that the integer collection has a method `sum`, and it is
done with very little of the fuss we would need if we used object
composition instead.

The mixin pattern isn't limited to non-polymorphic classes, of course.
We can use it to implement generic features as well.  The following
mixin defines functional-style iteration in terms of the imperative
iterator pattern.

```ocaml
class virtual ['a] fold_mixin =
object (self : 'self)
   method virtual iterator : 'a iterator
   method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
      (fun f x ->
            let y = ref x in
            let it = self#iterator in
            while it#has_value do
               y := f !y it#get;
               it#next
            done;
            !y)
end;;

class ['a] slist_with_fold =
object
   inherit ['a] slist
   inherit ['a] fold_mixin
end;;
```


  
