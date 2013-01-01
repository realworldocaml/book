Object Oriented Programming
===========================

_(yminsky: If we don't feel like these are "great" tools, maybe we
shouldn't say it!)_

_(yminsky: I wonder if it's worth emphasizing what makes objects
unique early on.  I think of them as no better of an encapsulation
tool than closures.  What makes them unique in my mind is that they
are some combination of lighter weight and more dynamic than the
alternatives (modules, records of closures, etc.))_

_(yminsky: I'm not sure where we should say it, but OCaml's object
system is strikingly different from those that most people are used
to.  It would be nice if we could call those differences out clearly
somewhere.  The main difference I see is the fact that subtyping and
inheritance are not tied together, and that subtyping is structural.)_

We've already seen several tools that OCaml provides for organizing
programs, particularly first-class modules.  In addition, OCaml also
supports object-oriented programming.  There are objects, classes, and
their associated types.  Objects are good for encapsulation and
abstraction, and classes are good for code re-use.

## When to use objects ##

You might wonder when to use objects.  First-class modules are more
expressive (a module can include types, classes and objects cannot),
and modules, functors, and algebraic data types offer a wide range of
ways to express program structure.  In fact, many seasoned OCaml
programmers rarely use classes and objects, if at all.

What exactly is object-oriented programming?  Mitchell [6] points out
four fundamental properties.

* _Abstraction_: the details of the implementation are hidden in the
  object; the interface is just the set of publically-accessible
  methods.
* _Subtyping_: if an object `a` has all the functionality of an object
  `b`, then we may use `a` in any context where `b` is expected.
* _Dynamic lookup_: when a message is sent to an object, the method to
  be executed is determined by the implementation of the object, not
  by some static property of the program.  In other words, different
  objects may react to the same message in different ways.
* _Inheritance_: the definition of one kind of object can be re-used
  to produce a new kind of object.

Modules already provide these features in some form, but the main
focus of classes is on code re-use through inheritance and late
binding of methods.  This is a critical property of classes: the
methods that implement an object are determined when the object is
instantiated, a form of _dynamic_ binding.  In the meantime, while
classes are being defined, it is possible (and necessary) to refer to
methods without knowing statically how they will be implemented.

In contrast, modules use static (lexical) scoping.  If you want to
parameterize your module code so that some part of it can be
implemented later, you would write a function/functor.  This is more
explicit, but often more verbose than overriding a method in a class.

In general, a rule of thumb might be: use classes and objects in
situations where dynamic binding is a big win, for example if you have
many similar variations in the implementation of a concept.  Real
world examples are fairly rare, but one good example is Xavier Leroy's
[Cryptokit][Cryptokit], which provides a variety of cryptographic
primitives that can be combined in building-block style.

[Cryptokit]: http://gallium.inria.fr/~xleroy/software.html#cryptokit

## OCaml objects ##

If you already know about object oriented programming in a language
like Java or C++, the OCaml object system may come as a surprise.
Foremost is the complete separation of subtyping and inheritance in
OCaml.  In a language like Java, a class name is also used as the type
of objects created by instantiating it, and the subtyping rule
corresponds to inheritance.  For example. if we implement a class
`Stack` in Java by inheriting from a class `Deque`, we would be
allowed to pass a stack anywhere a deque is expected (this is a silly
example of course, practitioners will point out that we shouldn't do
it).

OCaml is entirely different.  Classes are used to construct objects
and support inheritance, including non-subtyping inheritance.  Classes
are not types.  Instead, objects have _object types_, and if you want
to use objects, you aren't required to use classes at all.  Here is an
example of a simple object.

```ocaml
# let p =
  object
    val mutable x = 0
    method get = x
    method set i = x <- i
  end;;
val p : < get : int; set : int -> unit > = <obj>
```

The object has an integer value `x`, a method `get` that returns x,
and a method `set` that updates the value of x.

The object type is enclosed in angle brackets `< ... >`, containing
just the types of the methods.  Fields, like x, are not part of the
public interface of an object.  All interaction with an object is
through its methods.  The syntax for a method invocation (also called
"sending a message" to the object) uses the `#` character.

```ocaml
# p#get;
- : int = 0
# p#set 17;;
- : unit = ()
# p#get;;
- : int = 17
```

Objects can also be constructed by functions.  If we want to specify
the initial value of the object, we can define a function that takes
the initial value and produces an object.

```ocaml
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
```

Note that the types of the function `make` and the returned object now
use the polymorphic type `'a`.  When make is invoked on a concrete
value `5`, we get the same object type as before, with type `int` for
the value.

## Object Polymorphism ##

_(yminsky: Maybe this is a good time to talk about the nature of
object subtyping?)_

Functions can also take object arguments.  Let's construct a new
object `average` that's the average of any two objects with a
`get` method.

```ocaml
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
```

Note that the type for `average` uses the object type `< get : int;
.. >`.  The `..` are ellipsis, standing for any other methods.  The
type `< get : int; .. >` specifies an object that must have at least a
`get` method, and possibly some others as well.  If we try using the
exact type `< get : int >` for an object with more methods, type
inference will fail.

```ocaml
# let (p : < get : int >) = make 5;;
Error: This expression has type < get : int; set : int -> unit >
       but an expression was expected of type < get : int >
       The second object type has no method set
```

<sidebar>
<title>Elisions are polymorphic</title>

The `..` in an object type is an elision, standing for "possibly
more methods."  It may not be apparent from the syntax, but an elided
object type is actually polymorphic.  If we try to write a type
definition, we get an obscure error.

```ocaml
# type point = < get:int; .. >;;
Error: A type variable is unbound in this type declaration.
In type < get : int; .. > as 'a the variable 'a is unbound
```

A `..` in an object type is called a _row variable_ and this typing
scheme is called _row polymorphism_.  Even though `..` doesn't look
like a type variable, it actually is.  The error message suggests a
solution, which is to add the `as 'a` type constraint.

```ocaml
# type 'a point = < get:int; .. > as 'a;;
type 'a point = 'a constraint 'a = < get : int; .. >
```

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

```ocaml
# let print_point p = Printf.printf "Point: %d\n" p#get;;
val print_point : < get : int; .. > -> unit = <fun>
# print_point (make 5);;
Point: 5
# print_point (average (make 5) (make 15));;
Point: 10
```

## Classes ##

Programming with objects directly is great for encapsulation, but one
of the main goals of object-oriented programming is code re-use
through inheritance.  For inheritance, we need to introduce _classes_.
In object-oriented programming, a class is a "recipe" for creating
objects.  The recipe can be changed by adding new methods and fields,
or it can be changed by modifying existing methods.

In OCaml, class definitions must be defined as top-level statements in
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

_(yminsky: You say that inheritance uses an existing class to define a
new one, but the example below looks like using an existing class to
define a new module.  Is that what's going on?  Or is a new class
being created implicitly?  If the latter, it might be better to be
more explicit in this example and name the new class.)_

Inheritance uses an existing class to define a new one.  For example,
the following class definition supports an addition method `moveby`
that moves the point by a relative amount.  This also makes use of the
`(self : 'self)` binding after the `object` keyword.  The variable
`self` stands for the current object, allowing self-invocation, and
the type variable `'self` stands for the type of the current object
(which in general is a subtype of `movable_point`).

```ocaml
# class movable_point =
  object (self : 'self)
    inherit point
    method moveby dx = self#set (self#get + dx)
  end;;
```

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

Next, we can define the list itself.  We'll keep a field `head` the
refers to the first element in the list, and `last` refers to the
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

```
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
with the appropriate methods, it does not have to be specified by the
object's class _a priori_.  We'll leave abstract classes for later.
Let's demonstrate the technique using object types.

First, we'll define an object type `iterator` that specifies the
methods in an iterator.

```ocaml
type 'a iterator = < get : 'a; has_value : bool; next : unit >;;`
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
  end
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

```ocaml
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
```

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

```ocaml
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
   
We have multiple choices in definining the module type, depending on
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
and `last`, as well ad the types of each of the methods.  We've also
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
  
