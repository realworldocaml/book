# Classes {#classes}

Programming with objects directly is great for encapsulation, but one of the
main goals of object-oriented programming is code reuse through inheritance.
For inheritance, we need to introduce *classes*. In object-oriented
programming, a class is a "recipe" for creating objects. The recipe can be
changed by adding new methods and fields, or it can be changed by modifying
existing methods. [programming/object-oriented programming (OOP)]{.idx}

## OCaml Classes {#ocaml-classes}

In OCaml, class definitions must be defined as toplevel statements in a
module. The syntax for a class definition uses the keyword `class`:
[classes/basic syntax for]{.idx}

<link rel="import" href="code/classes/istack.mlt" part="1" />

The `class istack : object ... end` result shows that we have created a class
`istack` with *class type*`object ... end`. Like module types, class types
are completely separate from regular OCaml types (e.g., `int`, `string`, and
`list`) and, in particular, should not be confused with object types (e.g.,
`< get : int; .. >`). The class type describes the class itself rather than
the objects that the class creates. This particular class type specifies that
the `istack` class defines a mutable field `v`, a method `pop` that returns
an `int option`, and a method `push` with type `int -> unit`.

To produce an object, classes are instantiated with the keyword `new`:

<link rel="import" href="code/classes/istack.mlt" part="2" />

You may have noticed that the object `s` has been given the type `istack`.
But wait, we've stressed *classes are not types*, so what's up with that? In
fact, what we've said is entirely true: classes and class names *are not*
types. However, for convenience, the definition of the class `istack` also
defines an object type `istack` with the same methods as the class. This type
definition is equivalent to:

<link rel="import" href="code/classes/istack.mlt" part="3" />

Note that this type represents any object with these methods: objects created
using the `istack` class will have this type, but objects with this type may
not have been created by the `istack` class.

## Class Parameters and Polymorphism {#class-parameters-and-polymorphism}

A class definition serves as the *constructor* for the class. In general, a
class definition may have parameters that must be provided as arguments when
the object is created with `new`. [polymorphism/class parameters
and]{.idx}[classes/class parameters and polymorphism]{.idx}

Let's implement a variant of the `istack` class that can hold any values, not
just integers. When defining the class, the type parameters are placed in
square brackets before the class name in the class definition. We also add a
parameter `init` for the initial contents of the stack:

<link rel="import" href="code/classes/stack.mlt" part="0.5" />

Note that the type parameter `['a]` in the definition uses square brackets,
but for other uses of the type they are omitted (or replaced with parentheses
if there is more than one type parameter).

The type annotation on the declaration of `v` is used to constrain type
inference. If we omit this annotation, the type inferred for the class will
be "too polymorphic": `init` could have some type `'b list`:

<link rel="import" href="code/classes/stack.mlt" part="1" />

In general, we need to provide enough constraints so that the compiler will
infer the correct type. We can add type constraints to the parameters, to the
fields, and to the methods. It is a matter of preference how many constraints
to add. You can add type constraints in all three places, but the extra text
may not help clarity. A convenient middle ground is to annotate the fields
and/or class parameters, and add constraints to methods only if necessary.

## Object Types as Interfaces {#object-types-as-interfaces}

We may wish to traverse the elements on our stack. One common style for doing
this in object-oriented languages is to define a class for an `iterator`
object. An iterator provides a generic mechanism to inspect and traverse the
elements of a collection. [elements/traversing with iterator
objects]{.idx}[iterators]{.idx}[objects/object types as
interfaces]{.idx}[interfaces/object types as]{.idx}[classes/object types as
interfaces]{.idx}

There are two common styles for defining abstract interfaces like this. In
Java, an iterator would normally be specified with an interface, which
specifies a set of method types:

<link rel="import" href="code/classes/Iterator.java" />

In languages without interfaces, like C++, the specification would normally
use *abstract* classes to specify the methods without implementing them (C++
uses the "= 0" definition to mean "not implemented"):

<link rel="import" href="code/classes/citerator.cpp" />

OCaml supports both styles. In fact, OCaml is more flexible than these
approaches because an object type can be implemented by any object with the
appropriate methods; it does not have to be specified by the object's class
*a priori*. We'll leave abstract classes for later. Let's demonstrate the
technique using object types.

First, we'll define an object type `iterator` that specifies the methods in
an iterator:

<link rel="import" href="code/classes/iter.mlt" part="0.5" />

Next, we'll define an actual iterator for lists. We can use this to iterate
over the contents of our stack:

<link rel="import" href="code/classes/iter.mlt" part="1" />

Finally, we add a method `iterator` to the `stack` class to produce an
iterator. To do so, we construct a `list_iterator` that refers to the current
contents of the stack:

<link rel="import" href="code/classes/iter.mlt" part="2" />

Now we can build a new stack, push some values to it, and iterate over them:

<link rel="import" href="code/classes/iter.mlt" part="3" />

### Functional Iterators {#functional-iterators}

In practice, most OCaml programmers avoid iterator objects in favor of
functional-style techniques. For example, the alternative `stack` class that
follows takes a function `f` and applies it to each of the elements on the
stack: [functional iterators]{.idx}

<link rel="import" href="code/classes/iter.mlt" part="4" />

What about functional operations like `map` and `fold`? In general, these
methods take a function that produces a value of some other type than the
elements of the set.

For example, a `fold` method for our `['a] stack` class should have type
`('b -> 'a -> 'b) -> 'b -> 'b`, where the `'b` is polymorphic. To express a
polymorphic method type like this, we must use a type quantifier, as shown in
the following example:

<link rel="import" href="code/classes/iter.mlt" part="5" />

The type quantifier `'b.` can be read as "for all `'b`.” Type quantifiers
can only be used *directly after* the method name, which means that method
parameters must be expressed using a `fun` or `function` expression.


## Inheritance {#inheritance}

Inheritance uses an existing class to define a new one. For example, the
following class definition inherits from our `stack` class for strings and
adds a new method `print` that prints all the strings on the stack:
[inheritance]{.idx}[classes/inheritance in]{.idx}

<link rel="import" href="code/classes/stack.mlt" part="2" />

A class can override methods from classes it inherits. For example, this
class creates stacks of integers that double the integers before they are
pushed onto the stack:

<link rel="import" href="code/classes/stack.mlt" part="3" />

The preceding `as super` statement creates a special object called `super`
which can be used to call superclass methods. Note that `super` is not a real
object and can only be used to call methods.

## Class Types {#class-types}

To allow code in a different file or module to inherit from a class, we must
expose it and give it a class type. What is the class type? [classes/class
types]{.idx}

As an example, let's wrap up our `stack` class in an explicit module (we'll
use explicit modules for illustration, but the process is similar when we
want to define a `.mli` file). In keeping with the usual style for modules,
we define a type `'a t` to represent the type of our stacks:

<link rel="import" href="code/classes/class_types_stack.ml" />

We have multiple choices in defining the module type, depending on how much
of the implementation we want to expose. At one extreme, a maximally abstract
signature would completely hide the class definitions:

<link rel="import" href="code/classes/class_types_stack.ml" part="1" />

The abstract signature is simple because we ignore the classes. But what if
we want to include them in the signature so that other modules can inherit
from the class definitions? For this, we need to specify types for the
classes, called *class types*.

Class types do not appear in mainstream object-oriented programming
languages, so you may not be familiar with them, but the concept is pretty
simple. A class type specifies the type of each of the visible parts of the
class, including both fields and methods. Just as with module types, you
don't have to give a type for everything; anything you omit will be hidden:

<link rel="import" href="code/classes/class_types_stack.ml" part="2" />

In this signature, we've chosen to make everything visible. The class type
for `stack` specifies the types of the field `v`, as well as the types of
each of the methods.

## Open Recursion {#open-recursion}

Open recursion allows an object's methods to invoke other methods on the same
object. These calls are looked up dynamically, allowing a method in one class
to call a method from another class, if both classes are inherited by the
same object. This allows mutually recursive parts of an object to be defined
separately. [recursion/open recursion]{.idx}[open
recursion]{.idx}[classes/open recursion in]{.idx}

This ability to define mutually recursive methods from separate components is
a key feature of classes: achieving similar functionality with data types or
modules is much more cumbersome and verbose.

For example, consider writing recursive functions over a simple document
format. This format is represented as a tree with three different types of
node:

<link rel="import" href="code/classes/doc.ml" />

It is quite easy to write a function that operates by recursively traversing
this data. However, what if you need to write many similar recursive
functions? How can you factor out the common parts of these functions to
avoid repetitive boilerplate?

The simplest way is to use classes and open recursion. For example, the
following class defines objects that fold over the document data:

<link rel="import" href="code/classes/doc.ml" part="1" />

The `object (self)` syntax binds `self` to the current object, allowing the
`doc`, `list_item`, and `text_item` methods to call each other.

By inheriting from this class, we can create functions that fold over the
document data. For example, the `count_doc` function counts the number of
bold tags in the document that are not within a list:

<link rel="import" href="code/classes/doc.ml" part="2" />

Note how the `super` special object is used in `text_item` to call the
`[int] folder` class's `text_item` method to fold over the children of the
`text_item` node.

## Private Methods {#private-methods}

Methods can be declared *private*, which means that they may be called by
subclasses, but they are not visible otherwise (similar to a *protected*
method in C++). [methods/private methods]{.idx}[protected
methods]{.idx}[private methods]{.idx}[classes/private methods for]{.idx}

For example, we may want to include methods in our `folder` class for
handling each of the different cases in `doc` and `text_item`. However, we
may not want to force subclasses of `folder` to expose these methods, as they
probably shouldn't be called directly:

<link rel="import" href="code/classes/doc.ml" part="3" />

The final statement that builds the value `f` shows how the instantiation of
a `folder2` object has a type that hides the private methods.

To be precise, the private methods are part of the class type, but not part
of the object type. This means, for example, that the object `f` has no
method `bold`. However, the private methods are available to subclasses: we
can use them to simplify our `counter` class:

<link rel="import" href="code/classes/doc.ml" part="4" />

The key property of private methods is that they are visible to subclasses,
but not anywhere else. If you want the stronger guarantee that a method is
*really* private, not even accessible in subclasses, you can use an explicit
class type that omits the method. In the following code, the private methods
are explicitly omitted from the class type of `counter_with_sig` and can't be
invoked in subclasses of `counter_with_sig`:

<link rel="import" href="code/classes/doc.ml" part="5" />

## Binary Methods {#binary-methods}

A *binary method* is a method that takes an object of `self` type. One common
example is defining a method for equality: [methods/binary
methods]{.idx}[classes/binary methods for]{.idx}[binary methods]{.idx}

<link rel="import" href="code/classes/binary.mlt" part="0.5" />

Note how we can use the type annotation `(self: 'self)` to obtain the type of
the current object.

We can now test different object instances for equality by using the
`equals` binary method:

<link rel="import" href="code/classes/binary.mlt" part="1" />

This works, but there is a problem lurking here. The method `equals` takes an
object of the exact type `square` or `circle`. Because of this, we can't
define a common base class `shape` that also includes an equality method:

<link rel="import" href="code/classes/binary.mlt" part="2" />

The problem is that a `square` expects to be compared with a `square`, not an
arbitrary shape; likewise for `circle`. This problem is fundamental. Many
languages solve it either with narrowing (with dynamic type checking), or by
method overloading. Since OCaml has neither of these, what can we do?
[methods/method overloading]{.idx}[dynamic type
checking]{.idx}[narrowing]{.idx}[polymorphism/polymorphic equality]{.idx}

Since the problematic method is equality, one proposal we could consider is
to just drop it from the base type `shape` and use polymorphic equality
instead. However, the built-in polymorphic equality has very poor behavior
when applied to objects:

<link rel="import" href="code/classes/binary.mlt" part="3" />

The problem here is that two objects are considered equal by the built-in
polymorphic equality if and only if they are physically equal. There are
other reasons not to use the built-in polymorphic equality, but these false
negatives are a showstopper.

If we want to define equality for shapes in general, the remaining solution
is to use the same approach as we described for narrowing. That is, introduce
a *representation* type implemented using variants, and implement the
comparison based on the representation type: [representation types]{.idx}

<link rel="import" href="code/classes/binary.mlt" part="4" />

The binary method `equals` is now implemented in terms of the concrete type
`shape_repr`. When using this pattern, you will not be able to hide the
`repr` method, but you can hide the type definition using the module system:

<link rel="import" href="code/classes/binary_module.ml" />

Note that this solution prevents us from adding new kinds of shapes without
adding new constructors to the `shape_repr` type, which is quite restrictive.
The objects created by these classes are also in one-to-one correspondence
with members of the representation type, making the objects seem somewhat
redundant.

However, equality is quite an extreme instance of a binary method: it needs
access to all the information of the other object. Many other binary methods
need only partial information about the object. For instance, a method that
compares shapes by their sizes:

<link rel="import" href="code/classes/binary_larger.ml" />

In this case, there is no one-to-one correspondence between the objects and
their sizes, and we can still easily define new kinds of shape.

## Virtual Classes and Methods {#virtual-classes-and-methods}

A *virtual* class is a class where some methods or fields are declared but
not implemented. This should not be confused with the word `virtual` as it is
used in C++. A `virtual` method in C++ uses dynamic dispatch, while regular,
nonvirtual methods are statically dispatched. In OCaml, *all* methods use
dynamic dispatch, but the keyword `virtual` means that the method or field is
not implemented. A class containing virtual methods must also be flagged
`virtual` and cannot be directly instantiated (i.e., no object of this class
can be created). [dispatching, dynamic vs. static]{.idx}[static
dispatch]{.idx}[dynamic dispatch]{.idx}[virtual methods]{.idx}[methods/and
virtual classes]{.idx}[virtual classes]{.idx}[classes/virtual classes]{.idx}

To explore this, let's extend our shapes examples to simple, interactive
graphics. We will use the Async concurrency library and the
[Async_graphics](http://github.com/lpw25/async_graphics/) library, which
provides an asynchronous interface to OCaml's built-in Graphics library.
Concurrent programming with Async will be explored later in
[Concurrent Programming With Async](concurrent-programming.html#concurrent-programming-with-async){data-type=xref};
for now you can safely ignore the details. You just need to run
`opam install async_graphics` to get the library installed on your system.

We will give each shape a `draw` method that describes how to draw the shape
on the `Async_graphics` display:

<link rel="import" href="code/classes-async/shapes/shapes.ml" />

### Create Some Simple Shapes {#create-some-simple-shapes}

Now let's add classes for making squares and circles. We include an
`on_click` method for adding event handlers to the shapes: [geometric
shapes]{.idx}

<link rel="import" href="code/classes-async/verbose_shapes.ml" part="1" />

The `square` class is pretty straightforward, and the `circle` class below
also looks very similar:

<link rel="import" href="code/classes-async/verbose_shapes.ml" part="2" />

These classes have a lot in common, and it would be useful to factor out this
common functionality into a superclass. We can easily move the definitions of
`x` and `y` into a superclass, but what about `on_click`? Its definition
depends on `contains`, which has a different definition in each class. The
solution is to create a *virtual* class. This class will declare a `contains`
method but leave its definition to the subclasses.

Here is the more succinct definition, starting with a virtual `shape` class
that implements `on_click` and `on_mousedown`:

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="1" />

Now we can define `square` and `circle` by inheriting from `shape`:

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="2" />

One way to view a `virtual` class is that it is like a functor, where the
"inputs" are the declared—but not defined—virtual methods and fields. The
functor application is implemented through inheritance, when virtual methods
are given concrete implementations.


## Initializers {#initializers}

You can execute expressions during the instantiation of a class by placing
them before the object expression or in the initial value of a field:
[initializers]{.idx}[classes/initializers for]{.idx}

<link rel="import" href="code/classes/initializer.mlt" part="1" />

However, these expressions are executed before the object has been created
and cannot refer to the methods of the object. If you need to use an object's
methods during instantiation, you can use an initializer. An initializer is
an expression that will be executed during instantiation but after the object
has been created.

For example, suppose we wanted to extend our previous shapes module with a
`growing_circle` class for circles that expand when clicked. We could inherit
from `circle` and used the inherited `on_click` to add a handler for click
events:

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="3" />

## Multiple Inheritance {#multiple-inheritance}

When a class inherits from more than one superclass, it is using
*multiple inheritance*. Multiple inheritance extends the variety of ways that
classes can be combined, and it can be quite useful, particularly with
virtual classes. However, it can be tricky to use, particularly when the
inheritance hierarchy is a graph rather than a tree, so it should be used
with care. [multiple inheritance/name resolution
in]{.idx}[inheritance]{.idx}[classes/multiple inheritance in]{.idx}

### How Names Are Resolved {#how-names-are-resolved}

The main trickiness of multiple inheritance is due to naming—what happens
when a method or field with some name is defined in more than one class?

If there is one thing to remember about inheritance in OCaml, it is this:
inheritance is like textual inclusion. If there is more than one definition
for a name, the last definition wins.

For example, consider this class, which inherits from `square` and defines a
new `draw` method that uses `draw_rect` instead of `fill_rect` to draw the
square:

<link rel="import" href="code/classes-async/multiple_inheritance.ml" part=
"1" />

Since the `inherit` declaration comes before the method definition, the new
`draw` method overrides the old one, and the square is drawn using
`draw_rect`. But, what if we had defined `square_outline` as follows?

<link rel="import" href="code/classes-async/multiple_inheritance_wrong.ml" part=
"1" />

Here the `inherit` declaration comes after the method definition, so the
`draw` method from `square` will override the other definition, and the
square will be drawn using `fill_rect`.

To reiterate, to understand what inheritance means, replace each `inherit`
directive with its definition, and take the last definition of each method or
field. Note that the methods and fields added by an inheritance are those
listed in its class type, so private methods that are hidden by the type will
not be included.

### Mixins {#mixins}

When should you use multiple inheritance? If you ask multiple people, you're
likely to get multiple (perhaps heated) answers. Some will argue that
multiple inheritance is overly complicated; others will argue that
inheritance is problematic in general, and one should use object composition
instead. But regardless of who you talk to, you will rarely hear that
multiple inheritance is great and that you should use it widely. [mixin
patterns]{.idx}[multiple inheritance/mixin pattern and]{.idx}

In any case, if you're programming with objects, there's one general pattern
for multiple inheritance that is both useful and reasonably simple: the
*mixin* pattern. Generically, a *mixin* is just a virtual class that
implements a feature based on another one. If you have a class that
implements methods *A*, and you have a mixin *M* that provides methods
*B* from *A*, then you can inherit from *M*—"mixing" it in—to get
features *B*.

That's too abstract, so let's give some examples based on our interactive
shapes. We may wish to allow a shape to be dragged by the mouse. We can
define this functionality for any object that has mutable `x` and `y` fields
and an `on_mousedown` method for adding event handlers:

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="4" />

This allows us to create draggable shapes using multiple inheritance:

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="5" />

We can also use mixins to create animated shapes. Each animated shape has a
list of update functions to be called during animation. We create an
`animated` mixin to provide this update list and ensure that the functions in
it are called regular intervals when the shape is animated:
[animation/creating with mixins]{.idx}

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="6" />

We use initializers to add functions to this update list. For example, this
class will produce circles that move to the right for a second when clicked:

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="7" />

These initializers can also be added using mixins:

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="8" />

Since the `linear` and `harmonic` mixins are only used for their side
effects, they can be inherited multiple times within the same object to
produce a variety of different animations: [linear mixins]{.idx}

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="9" />

### Displaying the Animated Shapes {#displaying-the-animated-shapes}

We finish our shapes module by creating a `main` function to draw some shapes
on the graphical display and running that function using the Async scheduler:
[animation/displaying animated shapes]{.idx}[multiple inheritance/displaying
animated shapes with]{.idx}

<link rel="import" href="code/classes-async/shapes/shapes.ml" part="10" />

Our `main` function creates a list of shapes to be displayed and defines a
`repaint` function that actually draws them on the display. We then open a
graphical display and ask Async to run `repaint` at regular intervals.

Finally, build the binary by linking against the `async_graphics` package,
which will pull in all the other dependencies:

<link rel="import" href="code/classes-async/shapes/dune" />

<link rel="import" href="code/classes-async/shapes/build_shapes.sh" />

When you run the binary, a new graphical window should appear (on Mac OS X,
you will need to install the X11 package first, which you will be prompted
for). Try clicking on the various widgets, and gasp in awe at the
sophisticated animations that unfold as a result.

The graphics library described here is the one built into OCaml and is more
useful as a learning tool than anything else. There are several third-party
libraries that provide more sophisticated bindings to various graphics
subsystems: [js_of_ocaml library]{.idx}[LabIGL library]{.idx}[Lablgtk
library]{.idx}[graphics libraries]{.idx}[libraries/for
graphics]{.idx}[external libraries/for graphics]{.idx}

[Lablgtk](http://lablgtk.forge.ocamlcore.org)
: A strongly typed interface to the GTK widget library.

[LablGL](https://forge.ocamlcore.org/projects/lablgl/)
: An interface between OCaml and OpenGL, a widely supported standard for 3D
  rendering.

[js_of_ocaml](http://ocsigen.org/js_of_ocaml/api/Js)
: Compiles OCaml code to JavaScript and has bindings to WebGL. This is the
  emerging standard for 3D rendering in web browsers.
