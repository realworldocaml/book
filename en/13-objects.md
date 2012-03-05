Object Oriented Programming
===========================

We've already seen several tools that OCaml provides for organizing
programs.  There are compilation units, interfaces, modules, and
functors.  In addition to all of this, OCaml also support
object-oriented programming.  There are objects, classes, and their
associated types.  Objects are great for encapsulation and
abstraction, and classes are great for code re-use.

If you want to use objects, it isn't required to use classes, you can
use objects directly.  Here is an example of a simple object.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let p =
  object
    val mutable x = 0
    method get = x
    method set i = x <- i
  end;;
val p : < get : int; set : int -> unit > = <obj>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The object has an integer value `x`, a method `get` that returns x,
and a method `set` that updates the value of x.

The object type is enclosed in angle brackets `< ... >`, containing
just the types of the methods.  Fields, like x, are not part of the
public interface of an object.  All interaction with an object is
through its methods.  The syntax for a method invocation (also called
"sending a message" to the object) uses the `#` character.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# p#get;
- : int = 0
# p#set 17;;
- : unit = ()
# p#get;;
- : int = 17
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Objects can also be constructed by functions.  If we want to specify
the initial value of the object, we can define a function that takes
the initial value and produces an object.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let make i =
  object
    val mutable x = i
    method get = x
    method set y = x <- y
  end;;
val make : 'a -> < get : 'a; set : 'a -> unit > = <fun>
# let p = make 5;;
val p : < get : int; set : int -> unit > = <obj>
# p#get;;
- : int = 5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the types of the function `make` and the returned object now
use the polymorphic type `'a`.  When make is invoked on a concrete
value `5`, we get the same object type as before, with type `int` for
the value.

## Object Polymorphism ##

Functions can also take object arguments.  Let's construct a new
object `average` that thats the average of any two objects with a
`get` method.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let average p1 p2 =
  object
    method get = (p1#get + p2#get) / 2
  end;;
val average : < get : int; .. > -> < get : int; .. > -> < get : int > = <fun>
# let p1 = make 5;;
# let p2 = make 15;;
# let a = average p1 p2;;
# a#get;;
- : int = 10
# p2#set 25;;
# a#get;;
- : int = 15
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the type for `average` uses the object type `< get : int;
.. >`.  The `..` are ellipsis, standing for any other methods.  The
type `< get : int; .. >` specifies an object that must have at least a
`get` method, and possibly some others as well.  If we try using the
exact type `< get : int >` for an object with more methods, the type
conversion will fail.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let (p : < get : int >) = make 5;;
Error: This expression has type < get : int; set : int -> unit >
       but an expression was expected of type < get : int >
       The second object type has no method set
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<sidebar>
It may not be apparent, but an elided object type is actually
polymorphic.  If we try to write a type definition, we get an obscure
error.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# type point = < get:int; .. >;;
Error: A type variable is unbound in this type declaration.
In type < get : int; .. > as 'a the variable 'a is unbound
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Technically speaking, a `..` in an object type is called a _row
variable_ and this typing scheme is called _row polymorphism_.  Even
though `..` doesn't look like a type variable, it actually is.  The
error message suggests a solution, which is to add the `as 'a` type
constraint.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# type 'a point = < get:int; .. > as 'a;;
type 'a point = 'a constraint 'a = < get : int; .. >
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In other words, the type `'a point` is equal to `'a`, where `'a = <
get : int; .. >`.  That may seem like an odd way to say it, and in
fact, this type definition is not really an abbreviation because `'a`
refers to the entire type.
</sidebar>

An object of type `< get:int; .. >` can be any object with a method
`get:int`, it doesn't matter how it is implemented.  So far, we've
constructed two objects with that type; the function `make`
constructed one, and so did `average`.  When the method `#get` is
invoked, the actual method that is run is determined by the object.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let print_point p = Printf.printf "Point: %d\n" p#get;;
val print_point : < get : int; .. > -> unit = <fun>
# print_point (make 5);;
Point: 5
# print_point (average (make 5) (make 15));;
Point: 10
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Classes ##

Programming with objects directly is great for encapsulation, but one
of the main goals of object-oriented programming is code re-use
through inheritance.  For inheritance, we need to introduce _classes_.
In object-oriented programming, a class is a "recipe" for creating
objects.  The recipe can be changed by adding new methods and fields,
or it can be changed by modifying existing methods.

In OCaml, class definitions must be defined as top-level expressions
in a compilation unit or module.  A class is not an object, and a
class definition is not an expression.  The syntax for a class
definition uses the keyword `class`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type `class point : ... end` is a _class type_.  This particular
type specifies that the `point` class defines a mutable field `x`, a
method `get` that returns an `int`, and a method `set` with type `int
-> unit`.

To produce an object, classes are instantiated with the keyword `new`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let p = new point;;
val p : point = <obj>
# p#get;;
- : int = 0
# p#set 5;;
- : unit = ()
# p#get;;
- : int = 5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Inheritance uses an existing class to define a new one.  For example,
the following class definition supports an addition method `moveby`
that moves the point by a relative amount.  This also makes use of the
`(self : 'self)` binding after the `object` keyword.  The variable
`self` stands for the current object, allowing self-invocation, and
the type variable `'self` stands for the type of the current object
(which in general is a subtype of `movable_point`).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
# let movable_point =
  object (self : 'self)
    inherit point
    method moveby dx = self#set (self#get + dx)
  end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
class ['a] node x =
object
  val mutable value : 'a = x
  val mutable next_node : ('a) node option = None

  method get = value
  method set x = value <- x

  method next = next_node
  method set_next node = next_node <- node
end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `value` is the value stored in the node, and it can be retrieved
and changed with the `get` and `set` methods.  The `next_node` field
is the link to the next element in the stack.  Note that the type
parameter `['a]` in the definition uses square brackets, but other
uses of the type use parentheses.

Next, we can define the list itself.  We'll keep a field `head` the
refers to the first element in the list, and `last` refers to the
final element in the list.  The method `insert` adds an element to the
end of the list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Object types ##

Of course, this definition of the class `slist` is only part of the
implementation, we also need to add the ability in inspect the
elements in the list.  One common style for doing this is to define a
class for an `iterator` object.  An iterator provides a generic
mechanism to inspect and traverse the elements of a collection.  This
isn't just for a list, but for many different kinds of collections.

There are also two common styles for defining abstract interfaces like
this.  In Java, an iterator would normally be specified with an
interface, which specifies a set of method types.  In languages
without interfaces, like C++, the specification would normally use
_abstract_ classes to specifiy the methods without implementing them
(C++ uses the "= 0" definition to mean "unimplemented").

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OCaml support both styles of definition.  In fact, if we view object
types like interfaces, OCaml is even more flexible because an object
type can be implemented by any object with the appropriate methods, it
does not have to be specified by the object's class _a priori_.  We'll
leave abstract classes for later.  Let's demonstrate the technique using
object.types.

First, we'll define an object type `iterator` that specifies the
methods in an iterator.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
class type ['a] iterator =
object
  method get : 'a
  method has_value : bool
  method next : unit
end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Next, we'll define an actual iterator for the class `slist`.  We can
represent the position in the list with a field `current`, following
links as we traverse the list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, we add a method `iterator` to the sxlist class to produce an
iterator.  To do so, we construct an `slist_iterator` that refers to
the first node in the list, but we want to return a value with the
object type `iterator`.  This requires an explicit coercion using the
`:>` operator.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
class ['a] slist = object
...
   method iterator =
      (new slist_iterator first :> 'a iterator)
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We may also wish to define functional-style versions for iterating
through the list.  Iterating through the list without changing it is
easy enough, we can just construct an iterator to iterate through the
list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
method iter f =
  let it = self#iterator in
  while it#has_value do
    f it#get
    it#next
  end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

What about functional operations similar to `List.map` or `List.fold`?
In general, these methods take a function that produces a value of
some other type than the elements of the set.  For example, the
function `List.fold` has type `'a list -> ('b -> 'a -> 'b) -> 'b ->
'b`, where `'b` is an arbitrary type.  To replicate this in the
`slist` class, we need a method type `('b -> 'a -> 'b) -> 'b -> 'b`,
where the method type is polymorphic over `'b`.

The solution is to use a type quantifier, as shown in the folllowing
example.  The method type must be specified directly after the method
name, which means that method parameters must be expressed using a
`fun` or `function` expression.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
   (fun f x ->
         let y = ref x in
         let it = self#iterator in
         while it#has_value do
            y := f !y it#get;
            it#next
         done;
         !y)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Immutable objects ##

Many people consider object-oriented programming to be intrinsically
imperative, where an object is like a state machine.  Sending a
message to an object causes it to change state, possibily sending
messages to other objects.

Indeed, in many programs, this makes sense, but it is by no means
required.  Let's define an object-oriented version of lists similar to
the imperative list above.  We'll implement it with a regular list
type `'a list`, and insertion will be to the beginning of the list
instead of to the end.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
class ['a] flist =
object (self : 'self)
   val elements : 'a list = []

   method is_empty = elements = []

   method insert x : 'self = {< elements = x :: elements >}

   method iterator =
      (new flist_iterator elements :> 'a iterator)

   method iter (f : 'a -> unit) = List.iter f elements

   method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
      (fun f x -> List.fold_left f x elements)
end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A key part of the implementation is the definition of the method
`insert`.  The expression `{< ... >}` produces a copy of the current
object, with the same type, and the specified fields updated.  In
other words, the `new_fst new_x` method produces a copy of the object,
with `x` replaced by `new_x`.  The original object is not modified,
and the value of `y` is also unaffected.

There are some restriction on the use of the expression `{< ... >}`.
It can be used only within a method body, and only the values of
fields may be updated.  Method implementations are fixed at the time
the object is created, they cannot be changed dynamically.

We use the same object type `iterator` for iterators, but implement it
differently.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
class ['a] flist_iterator l =
object
   val mutable elements : 'a list = l

   method has_value = l <> []

   method get =
      match l with
         h :: _ -> h
       | [] -> raise (Invalid_argument "list is empty")

   method next =
      match l with
         _ :: l -> elements <- l
       | [] -> raise (Invalid_argument "list is empty")
end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
