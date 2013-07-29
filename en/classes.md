# Classes

Programming with objects directly is great for encapsulation, but one of the
main goals of object-oriented programming is code re-use through inheritance.
For inheritance, we need to introduce _classes_.  In object-oriented
programming, a class is a "recipe" for creating objects.  The recipe can be
changed by adding new methods and fields, or it can be changed by modifying
existing methods.

## OCaml Classes

In OCaml, class definitions must be defined as toplevel statements in a module.
A class is not an object, and a class definition is not an expression.  The
syntax for a class definition uses the keyword `class`.

```frag
((typ ocaml)(name classes/istack.topscript)(part 0))
```

The `class istack : object ... end` result shows that we have created a class
`istack` with _class type_ `object ... end`.  Like module types, class types
are completely separate from regular OCaml types (e.g. `int`, `string`, `list`)
and, in particular, should not be confused with object types (e.g. `< get :
int; .. >`). The class type describes the class itself rather than the objects
that the class creates. This particular class type specifies that the `istack`
class defines a mutable field `v`, a method `pop` that returns an `int option`,
and a method `push` with type `int -> unit`.

To produce an object, classes are instantiated with the keyword `new`.

```frag
((typ ocaml)(name classes/istack.topscript)(part 1))
```

You may have noticed that the object `s` has been given the type `istack`. But
wait, we've stressed _classes are not types_, so what's up with that?  In fact,
what we've said is entirely true, classes and class names *are not* types.
However, for convenience, the definition of the class `istack` also defines an
object type `istack` with the same methods as the class. This type definition
is equivalent to:

```frag
((typ ocaml)(name classes/istack.topscript)(part 2))
```

Note that this type represents any object with these methods: objects created
using the `istack` class will have this type, but objects with this type may
not have been created by the `istack` class.

## Class parameters and polymorphism ##

A class definition serves as the _constructor_ for the class.  In
general, a class definition may have parameters that must be provided
as arguments when the object is created with `new`.

Let's implement a class for creating simple stack objects. When defining the
class, the type parameters are placed in square brackets before the class name
in the class definition.  We also need a parameter `init` for the initial
contents of the stack.

```frag
((typ ocamltop)(name classes/stack.topscript)(part 0))
```

Note that the type parameter `['a]` in the definition uses square brackets, but
other uses of the type can omit them (or use parentheses if there is more than
one type parameter).

The type annotation on the `val` declaration is used to constrain type
inference.  If we omit these annotations, the type inferred for the class will
be "too polymorphic": `init` could have some type `'b list`.

```frag
((typ ocamltop)(name classes/stack.topscript)(part 1))
```

In general, we need to provide enough constraints so that the compiler
will infer the correct type.  We can add type constraints to the
parameters, to the fields, and to the methods.  It is a matter of
preference how many constraints to add.  You can add type constraints
in all three places, but the extra text may not help clarity.  A
convenient middle ground is to annotate the fields and/or class
parameters, and add constraints to methods only if necessary.

## Object types as interfaces ##

We may wish to traverse the elements on our stack. One common style
for doing this in object-oriented languages is to define a class for
an `iterator` object.  An iterator provides a generic mechanism to
inspect and traverse the elements of a collection.

There are two common styles for defining abstract interfaces like
this.  In Java, an iterator would normally be specified with an
interface, which specifies a set of method types.

```frag
((typ java)(name classes/Iterator.java))
```

In languages
without interfaces, like C++, the specification would normally use
_abstract_ classes to specify the methods without implementing them
(C++ uses the "= 0" definition to mean "not implemented").

```frag
((typ cpp)(name classes/citerator.cpp))
```

OCaml supports both styles.  In fact, OCaml is more flexible than these
approaches because an object type can be implemented by any object
with the appropriate methods; it does not have to be specified by the
object's class _a priori_.  We'll leave abstract classes for later.
Let's demonstrate the technique using object types.

First, we'll define an object type `iterator` that specifies the
methods in an iterator.

```frag
((typ ocamltop)(name classes/iter.topscript)(part 0))
```

Next, we'll define an actual iterator for lists.  We can use this to
iterate over the contents of our stack.

```frag
((typ ocamltop)(name classes/iter.topscript)(part 1))
```

Finally, we add a method `iterator` to the `stack` class to produce an
iterator.  To do so, we construct a `list_iterator` that refers to
the current contents of the stack.

```frag
((typ ocamltop)(name classes/iter.topscript)(part 2))
```

Now we can build a new stack, push some values to it, and
iterate over them.

```frag
((typ ocamltop)(name classes/iter.topscript)(part 3))
```

### Functional iterators

In practise, most OCaml programmers avoid iterator objects in favor of
functional-style techniques. For example, the alternative stack class below
takes a function `f` and applies it to each of the elements on the stack.

```frag
((typ ocamltop)(name classes/iter.topscript)(part 4))
```

What about functional operations like `map` and `fold`?  In general,
these methods take a function that produces a value of some other
type than the elements of the set.

For example, a `fold` method for our `['a] stack` class should have type `('b
-> 'a -> 'b) -> 'b -> 'b`, where the method type is polymorphic over `'b`. To
express this we must use a type quantifier, as shown in the following example.

```frag
((typ ocamltop)(name classes/iter.topscript)(part 5))
```

Polymorphic method types must be specified directly _after_ the method name,
which means that method parameters must be expressed using a `fun` or
`function` expression.

## Inheritance

Inheritance uses an existing class to define a new one.  For example,
the following class definition inherits from our stack class for
strings and adds a new method `print` that prints all the strings on
the stack.

```frag
((typ ocamltop)(name classes/stack.topscript)(part 2))
```

A class can override methods from classes it inherits. For example,
this class creates stacks of integers that double the integers before
they are pushed onto the stack.

```frag
((typ ocamltop)(name classes/stack.topscript)(part 3))
```

The `as super` statement above creates a special object called
`super` which can be used to call superclass methods. Note that
`super` is not a real object and can only be used to call methods.

## Class types

To allow code in a different file or module to inherit from a class
we must expose it and give it a class type. What is the class type?

As an example, let's wrap up our `stack` class in an explicit module
(we'll use explicit modules for illustration, but the process is
similar when we want to define a `.mli` file).  In keeping with the
usual style for modules, we define a type `'a t` to represent the
type of our stacks.

```frag
((typ ocaml)(name classes/class_types_stack.ml)(part 0))
```

We have multiple choices in defining the module type, depending on
how much of the implementation we want to expose.  At one extreme, a
maximally-abstract signature would completely hide the class
definitions.

```frag
((typ ocaml)(name classes/class_types_stack.ml)(part 1))
```

The abstract signature is simple because we ignore the classes.  But
what if we want to include them in the signature, so that other modules
can inherit from the class definitions?  For this, we need to specify
types for the classes, called _class types_.

Class types do not appear in mainstream object-oriented programming languages,
so you may not be familiar with them, but the concept is pretty simple.  A
class type specifies the type of each of the visible parts of the class,
including both fields and methods.  Just like for module types, you don't have
to give a type for everything; anything you omit will be hidden.

```frag
((typ ocaml)(name classes/class_types_stack.ml)(part 2))
```

In this signature, we've chosen to make everything visible.  The
class type for `stack` specifies the types of the field `v`, as well
as the types of each of the methods.

## Open Recursion

Open recursion allows an object's methods to invoke other methods on
the same object. These calls are looked up dynamically allowing a
method in one class to call a method from another class, if both
classes are inherited by the same object. This allows mutually
recursive parts of an object to be defined separately.

This ability to define mutually recursive methods from separate
components is a key feature of classes: achieving similar
functionality with datatypes or modules is much more cumbersome and
verbose.

For example, consider writing recursive functions over a simple
document format. This format is represented as a tree with three
different types of node:

```frag
((typ ocaml)(name classes/doc.ml)(part 0))
```

It is quite easy to write a function that operates by recursively
traversing this data. However, what if you need to write many similar
recursive functions? How can you factor out the common parts of these
functions to avoid repetitive boilerplate?

The simplest way is to use classes and open recursion. For example,
the following class defines objects which fold over the document data:

```frag
((typ ocaml)(name classes/doc.ml)(part 1))
```

The `object (self)` syntax binds `self` to the current object,
allowing the `doc`, `list_item` and `text_item` methods to call each
other.

By inheriting from this class we can create functions which fold over
the document data. For example, the `count_doc` function counts the
number of bold tags in the document that are not within a list:

```frag
((typ ocaml)(name classes/doc.ml)(part 2))
```

Note how the `super` special object is used in `text_item` to call
the `[int] folder` class's `text_item` method to fold over the
children of the `text_item` node.

## Private methods ##

Methods can be declared _private_, which means that they may be
called by subclasses, but they are not visible otherwise (similar to a
_protected_ method in C++).

For example, we may want to include methods in our `folder` class for
handling each of the different cases in `doc` and `text_item`.
However, we may not want to force subclasses of `folder` to expose
these methods as they probably shouldn't be called directly.

```frag
((typ ocaml)(name classes/doc.ml)(part 3))
```

To be precise, the private methods are part of the class type, but
not part of the object type. This means, for example, that the object
`f` has no method `bold`. However, the private methods are available
to subclasses: we can use them to simplify our `counter` class.

```ocaml
# class counter = object
    inherit [int] folder as super

    method list_item acc li = acc

    method private bold acc txt = 
      let acc = super#bold acc txt in
       acc + 1
  end
```

The key property of private methods is that they are visible to
subclasses, but not anywhere else.  If you want the stronger
guarantee that a method is _really_ private, not even accessible in
subclasses, you can use an explicit class type that omits the method.
In the following code, the private methods are explicitly omitted
from the class type of `counter`, and can't be invoked in subclasses
of `counter`.

```ocaml
class counter : object
  method doc : int -> doc -> int
  method list_item : int -> 'b list_item -> int
  method text_item : int -> text_item -> int
end = object
  inherit [int] folder as super

  method list_item acc li = acc

  method private bold acc txt = 
    let acc = super#bold acc txt in
     acc + 1
end
```

### Binary methods ###

A _binary method_ is a method that takes an object of `self` type.
One common example is defining a method for equality. 

```frag
((typ ocamltop)(name classes/binary.topscript)(part 0))
```

We can now test different object instances for equality by using
the `equals` binary method.

```frag
((typ ocamltop)(name classes/binary.topscript)(part 1))
```

This works, but there is a problem lurking here.  The method `equals`
takes an object of the exact type `square` or `circle`.  Because of
this, we can't define a common base class `shape` that also includes
an equality method.

```frag
((typ ocamltop)(name classes/binary.topscript)(part 2))
```

The problem is that a `square` expects to be compared with a `square`,
not an arbitrary shape; similarly for `circle`.

This problem is fundamental.  Many languages solve it either with
narrowing (with dynamic type checking), or by method overloading.
Since OCaml has neither of these, what can we do?

Since the problematic method is equality, one proposal we could consider is is
to just drop it from the base type `shape` and use polymorphic equality
instead.  However, the built-in polymorphic equality has very poor behavior
when applied to objects.

```frag
((typ ocamltop)(name classes/binary.topscript)(part 3))
```

The problem here is that the built-in polymorphic equality compares the method
implementations, not their return values.  The method implementations (the
function values that implement the methods) are different, and so the equality
comparison is false.  There are other reasons not to use the built-in
polymorphic equality, but these false negatives are a showstopper.

If we want to define equality for shapes in general, the remaining solution is
to use the same approach as we described for narrowing.  That is, introduce a
_representation_ type implemented using variants, and implement the comparison
based on the representation type.

```frag
((typ ocamltop)(name classes/binary.topscript)(part 4))
```

The binary method `equals` is now implemented in terms of the concrete
type `shape_repr`.  In fact, the objects are now isomorphic to the
`shape_repr` type.  When using this pattern, you will not be able to
hide the `repr` method, but you can hide the type definition using the
module system.

```frag
((typ ocaml)(name classes/binary_module.ml))
```

## Virtual classes and methods ##

A _virtual_ class is a class where some methods or fields are declared, but not
implemented.  This should not be confused with the word `virtual` as it is used
in C++.  In C++, a `virtual` method uses dynamic dispatch, while regular
non-virtual methods are statically dispatched.  In OCaml, _all_ methods use
dynamic dispatch, but the keyword `virtual` means that the method or field is
not implemented.

To explore this, lets extend our shapes examples to simple interactive
graphics. For this we will use the Async concurrency library and the
[Async_graphics](http://github.com/lpw25/async_graphics/) library, which
provides an Async interface to OCaml's built in Graphics library. Concurrent
programming with Async will be explored later in
[xref](#concurrent-programming-with-async), for now you can safely ignore the
details.

We will extend our `shape` type to include a `draw` method. To
display the shapes we will define a reference to a list of shapes,
and ensure that any shapes in that list will be drawn on the display
at regular intervals. We also define an open_display function to open
a display and ensure that the Async scheduler is running:

```ocaml
open Async.Std
open Async_graphics

type drawable = < draw: unit >

let shapes: drawable list ref = ref []

let repaint () =
  try 
    clear_graph ();
    List.iter ~f:(fun s -> s#draw) !shapes;
    synchronize ()
  with Graphic_failure _ -> ();;

Clock.every (Time.Span.of_sec (1.0 /. 24.0)) repaint;;

let open_display () =
  close_graph ();
  open_graph "";
  auto_synchronize false;
  if not (Scheduler.is_running ()) then ignore (Thread.create Scheduler.go ());;
```

Now let's create classes for making squares and circles. We include
an `on_click` method for adding event handlers to the shapes.

```ocaml
class square w x y = object (self)
  val mutable x: int = x
  method x = x

  val mutable y: int = y
  method y = y

  val mutable width = w
  method width = width

  method draw = fill_rect x y width width

  method private contains x' y' = 
    x <= x' && x' <= x + width &&
      y <= y' && y' <= y + width

  method on_click ?start ?stop f = 
    on_click ?start ?stop 
      (fun {mouse_x;mouse_y} -> 
         if self#contains mouse_x mouse_y then 
           f mouse_x mouse_y)
end

class circle r x y = object
  val mutable x: int = x
  method x = x

  val mutable y: int = y
  method y = y

  val mutable radius = r
  method radius = radius

  method draw = fill_circle x y radius

  method private contains x' y' = 
    let dx = abs (x' - x) in
    let dy = abs (y' - y) in
    let dist = sqrt (Float.of_int ((dx * dx) + (dy * dy))) in
      dist <= (Float.of_int radius)

  method on_click ?start ?stop f = 
    on_click ?start ?stop 
      (fun {mouse_x;mouse_y} -> 
         if self#contains mouse_x mouse_y then 
           f mouse_x mouse_y)
end
```

These classes have a lot in common, and it would be useful to factor
out this common functionality into a superclass. We can easily move
the definitions of `x` and `y` into a superclass, but what about
`on_click`? Its definition depends on `contains` which has a
different definition in each class. The solution is to create a
_virtual_ class. This class will declare a `contains` method, but
leave its definition to the subclasses:

```ocaml
class virtual shape x y = object (self)
  method virtual private contains: int -> int -> bool

  val mutable x: int = x
  method x = x

  val mutable y: int = y
  method y = y

  method on_click ?start ?stop f = 
    on_click ?start ?stop 
      (fun {mouse_x;mouse_y} -> 
         if self#contains mouse_x mouse_y then 
           f mouse_x mouse_y)

  method on_mousedown ?start ?stop f = 
    on_mousedown ?start ?stop 
      (fun {mouse_x;mouse_y} -> 
         if self#contains mouse_x mouse_y then 
           f mouse_x mouse_y)
end
```

Now we can define `square` and `circle` by inheriting from `shape`:

```ocaml
class square w x y = object
  inherit shape x y

  val mutable width = w
  method width = width

  method draw = fill_rect x y width width

  method private contains x' y' = 
    x <= x' && x' <= x + width &&
      y <= y' && y' <= y + width
end

class circle r x y = object
  inherit shape x y

  val mutable radius = r
  method radius = radius

  method draw = fill_circle x y radius

  method private contains x' y' = 
    let dx = abs (x' - x) in
    let dy = abs (y' - y) in
    let dist = sqrt (Float.of_int ((dx * dx) + (dy * dy))) in
      dist <= (Float.of_int radius)
end
```

One way to view a `virtual` class is that it is like a functor, where
the "inputs" are the declared, but not defined, virtual methods and
fields.  The functor application is implemented through inheritance,
when virtual methods are given concrete implementations.

## Initializers

You can execute expressions during the instantiation of a class by
placing them before the object expression or in the initial value of
a field:

```ocaml
# class obj x = 
    let () = printf "Creating obj %d\n" x in
    object 
      val field = printf "Initializing field\n"; x
    end;;
class obj : int -> object val field : int end
# let o = new obj 3;;
Creating obj 3
Initializing field
val o : obj = <obj>
```

However, these expressions are executed before the object has been
created, and cannot refer to the methods of the object. If you need
to use an object's methods during instantiation you can use an
initializer. An initializer is an expression that will be executed
during instantiation but after the object has been created. 

For example, if we wanted to create a `growing_circle` class for
circles that grow when clicked then we could inherit from `circle`
and used the inherited `on_click` to add a handler for click events:

```ocaml
class growing_circle r x y = object (self)
  inherit circle r x y

  initializer
    self#on_click (fun x y -> radius <- radius * 2)
end
```

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
The behavior changed in OCaml 3.10 (previous behavior was hiding.)
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

That's too abstract, so let's give some examples based on our
interactive shapes. We may wish to allow a shape to be dragged by the
mouse. We can define this functionality for any object which has
mutable `x` and `y` fields and an `on_mousedown` method for adding
event handlers:

```ocaml
class virtual draggable = object (self)
  method virtual on_mousedown: 
    ?start:unit Deferred.t -> 
    ?stop:unit Deferred.t -> 
    (int -> int -> unit) -> unit
  val virtual mutable x: int  
  val virtual mutable y: int  

  val mutable dragging = false
  method dragging = dragging

  initializer 
    self#on_mousedown 
      (fun mouse_x mouse_y ->
         let offset_x = x - mouse_x in
         let offset_y = y - mouse_y in
         let mouse_up = Ivar.create () in
         let stop = Ivar.read mouse_up in
           dragging <- true;
           on_mouseup ~stop (fun _ -> Ivar.fill mouse_up (); dragging <- false);
           on_mousemove ~stop
             (fun {mouse_x;mouse_y} -> 
                x <- mouse_x + offset_x;
                y <- mouse_y + offset_y))
end
```

This allows us to create draggable shapes using multiple inheritance.

```ocaml
# class small_square = object inherit square 10 20 20 inherit draggable end;;
class small_square :
  object
    val mutable dragging : bool
    val mutable width : int
    val mutable x : int
    val mutable y : int
    method private contains : int -> int -> bool
    method dragging : bool
    method draw : unit
    method on_click :
      ?start:unit Async.Std.Deferred.t ->
      ?stop:unit Async.Std.Deferred.t -> (int -> int -> unit) -> unit
    method on_mousedown :
      ?start:unit Async.Std.Deferred.t ->
      ?stop:unit Async.Std.Deferred.t -> (int -> int -> unit) -> unit
    method width : int
    method x : int
    method y : int
  end
# shapes := (new small_square :> drawable) :: !shapes;;
- : unit = ()
```

We can also use mixins to create animated shapes. Each animated shape
has a list of update functions to be called during animation. We
create an `animated` mixin to provide this update list and ensure
that the functions in it are called regular intervals when the shape
is animated.

```ocaml
class virtual animated span = object (self)
  method virtual on_click: 
    ?start:unit Deferred.t -> 
    ?stop:unit Deferred.t -> 
    (int -> int -> unit) -> unit
  val mutable updates: (int -> unit) list = []
  val mutable step = 0
  val mutable running = false

  method running = running

  method animate = 
    step <- 0;
    running <- true;
    let stop = Clock.after span >>| fun () -> running <- false in    
      Clock.every ~stop (Time.Span.of_sec (1.0 /. 24.0)) 
        (fun () -> 
           step <- step + 1;
           List.iter ~f:(fun f -> f step) updates)

  initializer 
    self#on_click (fun x y -> if not self#running then self#animate)
end
```

We use initializers to add functions to this update list. For
example, this class will produce circles that move to the right for a
second when clicked:

```ocaml
class my_circle = object
  inherit circle 20 50 50
  inherit animated Time.Span.second
  initializer updates <- [fun _ -> x <- x + 5]
end
```

This initializers can also be added using mixins:

```ocaml
class virtual linear x' y' = object (self)
  val virtual mutable updates: (int -> unit) list
  val virtual mutable x: int
  val virtual mutable y: int

  initializer 
    let update _ = 
      x <- x + x';
      y <- y + y'
    in
      updates <- update :: updates
end

let pi = (atan 1.0) *. 4.0

class virtual harmonic offset x' y' = object (self)
  val virtual mutable updates: (int -> unit) list
  val virtual mutable x: int
  val virtual mutable y: int

  initializer 
    let update step =
      let m = sin (offset +. ((Float.of_int step) *. (pi /. 64.))) in
      let x' = Float.to_int (m *. Float.of_int x') in
      let y' = Float.to_int (m *. Float.of_int y') in
        x <- x + x';
        y <- y + y'
    in
      updates <- update :: updates
end
```

Since the `linear` and `harmonic` mixins are only used for there
side-effects, they can be inherited multiple times within the same
object to produce a variety of different animations.

```ocaml
class my_square x y = object (self)
  inherit square 40 x y
  inherit draggable
  inherit animated (Time.Span.of_int_sec 5)
  inherit linear 5 0
  inherit harmonic 0.0 7 ~-10
end

let my_circle = object
  inherit circle 30 250 250
  inherit animated (Time.Span.minute)
  inherit harmonic 0.0 10 0
  inherit harmonic (pi /. 2.0) 0 10
end

shapes := [(my_circle :> drawable); 
           (new my_square 50 450 :> drawable); 
           (new my_square 50 400 :> drawable)]
```

<note>
<title>Production note</title>

This chapter contains significant external contributions from Leo White.

</note>

