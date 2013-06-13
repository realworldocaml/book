# Object-Oriented Programming

We've already seen several tools that OCaml provides for organizing programs,
particularly modules.  In addition, OCaml also supports object-oriented
programming.  There are objects, classes, and their associated types. In this
chapter, we'll introduce you to OCaml objects and subtyping.  In the next
chapter [xref](#classes), we'll introduce you to classes and inheritance.

<note>
<title>What is Object-Oriented Programming?</title>

Object-oriented programming (often shorted to OOP) is a programming style
that encapsulates computation and data within logical *objects*.  Each
object contains some data stored in *fields*, and has
*method* functions that can be invoked against the data within the object.
The code definition behind an object is called a *class*, and objects are
constructed from a class definition by calling a constructor with the
data that the object will use to build itself.

There are five fundamental properties that differentiate OOP from other styles:

* _Abstraction_: the details of the implementation are hidden in the
  object, and the external interface is just the set of publicly-accessible
  methods.
* _Dynamic lookup_: when a message is sent to an object, the method to
  be executed is determined by the implementation of the object, not
  by some static property of the program.  In other words, different
  objects may react to the same message in different ways.
* _Subtyping_: if an object `a` has all the functionality of an object
  `b`, then we may use `a` in any context where `b` is expected.
* _Inheritance_: the definition of one kind of object can be reused
  to produce a new kind of object.  This new definition can override
  some behaviour, but also share code with its parent.
* _Open recursion_: an object's methods can invoke another method in the same
  object using a special variable (often called self). These method calls use
  dynamic lookup, allowing a method defined in one object to invoke methods
  defined in another object that inherits from the first.

Almost every notable modern programming language has been influenced
by OOP, and you'll have run across these terms if you've ever used
C++, Java, C#, Ruby, Python or Javascript.

</note>

## When to use objects ##

You might wonder when to use objects in OCaml, which has a multitude of
alternative mechanisms to express the same concept.  First-class modules are
more expressive (a module can include types, while classes and objects cannot).
Modules, functors, and algebraic data types also offer a wide range of ways to
express program structure.  In fact, many seasoned OCaml programmers rarely use
classes and objects, if at all.

Modules already provide these features in some form, but the main
focus of classes is on code re-use through inheritance and late
binding of methods.  This is a critical property of classes: the
methods that implement an object are determined when the object is
instantiated, a form of _dynamic_ binding.  In the meantime, while
classes are being defined, it is possible (and necessary) to refer to
methods without knowing statically how they will be implemented.

In contrast, modules use static (lexical) scoping.  If you want to
parameterize your module code so that some part of it can be
implemented later, you would write a function or functor.  This is more
explicit, but often more verbose than overriding a method in a class.

In general, a rule of thumb is: use classes and objects in situations where
dynamic binding is a big win, for example if you have many similar variations in
the implementation of a concept.  Two good examples are Xavier Leroy's
[Cryptokit](http://gallium.inria.fr/~xleroy/software.html#cryptokit), which
provides a variety of cryptographic primitives that can be combined in
building-block style, and the [Camlimages](http://cristal.inria.fr/camlimages/)
library which manipulates various graphical file formats.  And, of course, the
use of objects isn't just a programming choice -- it might be driven by design
choices, or because of the need to interact with existing components that are
already object oriented.

In this chapter, we'll introduce you to the basics of object definition and use
in OCaml, and then demonstrate their use with an example using Cryptokit.
We'll return to the more advanced areas of object use later on in the book in
[xref](#object-subtyping-and-inheritance).

## OCaml objects ##

If you already know about object oriented programming in a language like Java
or C++, the OCaml object system may come as a surprise.  Foremost is the
complete separation of objects, and their types, from the class system in
OCaml.  In a language like Java, a class name is also used as the type of
objects created by instantiating it, and the relationships between these object
types correspond to inheritance.  For example, if we implement a class `Deque`
in Java by inheriting from a class `Stack`, we would be allowed to pass a deque
anywhere a stack is expected.

OCaml is entirely different.  Classes are used to construct objects and support
inheritance, but classes are not types.  Instead, objects have _object types_,
and if you want to use objects, you aren't required to use classes at all.
Here's an example of a simple object.

```ocaml
# let r =
  object
    val mutable v = 0
    method get = v
    method set i = v <- i
  end;;
val r : < get : int; set : int -> unit > = <obj>
```

The object has an integer value `v`, a method `get` that returns v,
and a method `set` that updates the value of `v`.

The object type is enclosed in angle brackets `< ... >`, containing
just the types of the methods.  Fields, like `v`, are not part of the
public interface of an object.  All interaction with an object is
through its methods.  The syntax for a method invocation (also called
"sending a message" to the object) uses the `#` character.

```ocaml
# r#get;
- : int = 0
# r#set 17;;
- : unit = ()
# r#get;;
- : int = 17
```

Objects can also be constructed by functions.  If we want to specify
the initial value of the object, we can define a function that takes
the value and returns an object.

```ocaml
# let make i =
  object
    val mutable v = i
    method get = v
    method set y = v <- y
  end;;
val make : 'a -> < get : 'a; set : 'a -> unit > = <fun>
# let r = make 5;;
val r : < get : int; set : int -> unit > = <obj>
# r#get;;
- : int = 5
```

Note that the types of the function `make` and the returned object now
use the polymorphic type `'a`.  When make is invoked on a concrete
value `5`, we get the same object type as before, with type `int` for
the value.

## Object Polymorphism ##

Like polymorphic variants, methods can be used without an explicit type declaration.

```ocaml
# let size p =
    sqrt (p#x ** 2. +. p#y ** 2.);;
val size : < x : float; y : float; .. > -> float = <fun>
# let reset p: unit = 
    p#move 0. 0.;;
val reset : < move : float -> float -> unit; .. > -> unit = <fun>
# let limit p =
    if (size p) > 10. then reset p;;
val limit :
  < move : float -> float -> unit; x : float; y : float; .. > -> unit = <fun>
```

As you can see object types are inferred automatically from the methods that
are invoked on them.

The type system will complain if it sees incompatible uses of the same method:

```ocaml
# let print_coord c = 
    print_string ("(" ^ c#x ^ "," ^ c#y ^ ")");;
val print_coord : < x : string; y : string; .. > -> unit = <fun>
# let print_limited c =
    limit c;
    print_coord c;;
Characters 47-48:
  print_coord c;;
              ^
Error: This expression has type
         < move : float -> float -> unit; x : float; y : float; .. >
       but an expression was expected of type 
         < x : string; y : string; .. >
       Types for method x are incompatible
```

The `..` in the inferred object types are ellipsis, standing for any other
methods.  The type `< x : float; .. >` specifies an object that must have
at least an `x` method, and possibly some others as well. Such object types
are said to be _open_.

We can manually _close_ an object type using a type annotation:

```ocaml
# let size (p: < x: float; y: float >) =
    sqrt (p#x ** 2. +. p#y ** 2.);;
val size : < x : float; y : float > -> float = <fun>
# let p = object 
    method x = 3. 
    method y = 4. 
    method name = "p" 
  end;;
val p : < name : string; x : float; y : float > = <obj>
# size p;;
Characters 7-10:
  size p;;
       ^
Error: This expression has type < name : string; x : float; y : float >
       but an expression was expected of type < x : float; y : float >
       The second object type has no method name
```

<note>
<title>Elisions are polymorphic</title>

The `..` in an open object type is an elision, standing for "possibly more
methods."  It may not be apparent from the syntax, but an elided object type is
actually polymorphic.  If we try to write a type definition, we get an obscure
error.

```ocaml
# type point = < x: float; y: float; .. >;;
Characters 5-39:
  type point = < x: float; y: float; .. >;;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
In type < x : float; y : float; .. > as 'a the variable 'a is unbound
```

A `..` in an object type is called a _row variable_ and this typing scheme is
called _row polymorphism_.  Even though `..` doesn't look like a type variable,
it actually is.  Row polymorphism is also used in polymorphic variant types,
and there is a close relationship between objects and polymorphic variants:
objects are to records what polymorphic variants are to ordinary variants.

</note>

An object of type `< get:int; .. >` can be any object with a method `get:int`,
it doesn't matter how it is implemented.  When the method `#get` is invoked,
the actual method that is run is determined by the object.

```ocaml
# let print_ival i = 
    printf "Int: %d\n" i#get;;
val print_ival : < get : int; .. > -> unit = <fun>
# print_ival (make 5);;
Int: 5
- : unit = ()
# let t = object
    method get = Float.to_int (Time.to_float (Time.now ()))
  end;;
val t : < get : int > = <obj>
# print_ival t;;
Int: 1370650589
- : unit = ()
# print_ival t;;
Int: 1370650592
- : unit = ()
```

## Immutable objects ##

Many people consider object-oriented programming to be intrinsically
imperative, where an object is like a state machine.  Sending a
message to an object causes it to change state, possibly sending
messages to other objects.

Indeed, in many programs, this makes sense, but it is by no means required.
Let's define a function that creates immutable point objects.

```ocaml
# let point (init_x: float) (init_y: float) = object
    val x = init_x
    val y = init_y
    method x = x
    method y = y
    method move new_x new_y = {< x = new_x; y = new_y >}
  end;;
val point : float -> float -> 
  (< move : float -> float -> 'a; x : float; y : float > as 'a) = <fun>
```

A key part of the implementation is the definition of the method `move`.  The
expression `{< ... >}` produces a copy of the current object, with the same
type, and the specified fields updated.  In other words, the `move new_x new_y`
method produces a copy of the object, with `x` replaced by `new_x` and `y`
replaced by `new_y`.  The original object is not modified.

```ocaml
# let p = point 3. 4.;;
val p : < move : float -> float -> 'a; x : float; y : float > as 'a = <obj>
# let q = p#move 3. 5.;;
val q : < move : float -> float -> 'a; x : float; y : float > as 'a = <obj>
# p#y;;
- : float = 4.
# q#y;;
- : float = 5.
```

There are some restriction on the use of the expression `{< ... >}`.
It can be used only within a method body, and only the values of
fields may be updated.  Method implementations are fixed at the time
the object is created, they cannot be changed dynamically.

## When to use objects

You might wonder when to use objects in OCaml, which has a multitude of
alternative mechanisms to express the similar concepts.  First-class modules
are more expressive (a module can include types, while classes and objects
cannot).  Modules, functors, and algebraic data types also offer a wide range
of ways to express program structure.  In fact, many seasoned OCaml programmers
rarely use classes and objects, if at all.

Objects have some advantages over records: they don't require type definitions
and their support for row polymorphism makes them more flexible. However, the
heavy syntax and additional runtime cost means that objects are rarely used in
place of records.

The real benefits of objects come from the class system. Classes support
inheritance and open recursion. Open recursion allows parts of an object to be
defined separately. This works because calls between the methods of an object
are determined when the object is instantiated, a form of _dynamic_
binding. This makes it possible (and necessary) for one method to refer to
other methods in the object without knowing statically how they will be
implemented.

In contrast, modules use static binding.  If you want to parameterize your
module code so that some part of it can be implemented later, you would write a
function or functor.  This is more explicit, but often more verbose than
overriding a method in a class.

In general, a rule of thumb is: use classes and objects in situations where
open recursion is a big win.  Two good examples are Xavier Leroy's
[Cryptokit](http://gallium.inria.fr/~xleroy/software.html#cryptokit), which
provides a variety of cryptographic primitives that can be combined in
building-block style, and the [Camlimages](http://cristal.inria.fr/camlimages/)
library which manipulates various graphical file formats.

We'll introduce you to classes, and examples using open recursion, in
[xref](#classes).

## Subtyping

Subtyping is a central concept in object-oriented programming.  It
governs when an object with one type _A_ can be used in an expression
that expects an object of another type _B_.  When this is true, we say
that _A_ is a _subtype_ of _B_.  Actually, more concretely, subtyping
determines when the coercion operator `e :> t` can be applied.  This
coercion works only if the expression `e` has some type `s` and `s` is
a subtype of `t`.

### Width Subtyping

To explore this, let's define some simple object types for geometric
shapes.  The generic type `shape` has a method to compute the area,
and `square` and `circle` are specific kinds of shape.

```ocaml
type shape = < area : float >;;

type square = < area : float; width : float >;;

let square w = object
  method area = w *. w
  method width = w
end;;

type circle = < area : float; radius : float >;;

let circle r = object
  method area = 3.14 *. r ** 2.0
  method radius = r
end;;
```

A `square` has a method `area` just like a `shape`, and an additional
method `width`.  Still, we expect a `square` to be a `shape`, and it
is.  The coercion `:>` must be explicit.

```ocaml
# let shape w : shape = square w;;
Characters 22-30:
  let shape w : shape = square w;;
                        ^^^^^^^^
Error: This expression has type square but an expression was expected of type shape
       The second object type has no method width
# let shape w : shape = (square w :> shape);;
val shape : float -> shape = <fun>
```

This form of object subtyping is called _width_ subtyping. Width
subtyping means that an object type _A_ is a a subtype of _B_, if _A_
has all of the methods of _B_, and possibly more.  A `square` is a
subtype of `shape` because it implements all of the methods of `shape`
(the `area` method).

### Variance

What about types built from object types? If a `square` is a `shape`, we expect
a `square list` to be a `shape list`. OCaml does indeed allow such coercions:

```ocaml
# let squares: square list = [ square 1.0; square 2.0 ];;
val squares : square list = [<obj>; <obj>]
# let shapes: shape list = (squares :> shape list);;
val shapes : shape list = [<obj>; <obj>]
```

This would not be safe for all types built from objects. For instance, it is
not safe to coerce a `square array` into a `shape array`.

```ocaml
# let square_array: square array = [| square 1.0; square 2.0 |];;
val square_array : square array = [|<obj>; <obj>|]
# let shape_array: shape array = (square_array :> shape array);;
Characters 31-60:
  let shape_array: shape array = (square_array :> shape array);;
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type square array is not a subtype of shape array 
Type square = < area : float; width : float > is not compatible with type
  shape = < area : float > 
The second object type has no method width
```

This is disallowed because it would allow non-square shapes to be inserted into
`square_array`. We call such types _invariant_, while types like `list` are
said to be _covariant_.

For some types the type built from `shape` is a subtype of the one built from
`square`.

```ocaml
# type 'a to_string = 'a -> string;;
type 'a to_string = 'a -> string
# let shape_to_string: shape to_string = 
    fun s -> sprintf "Shape(%F)" s#area;;
val shape_to_string : shape to_string = <fun>
# let square_to_string: square to_string = 
    (shape_to_string :> square to_string);;
val square_to_string : square to_string = <fun>
```

We call such types _contravariant_.

### Depth subtyping

We can also extend subtyping to object types whose methods types are
subtypes. This is commonly called _depth_ subtyping, which, in its most general
form, says that an object type `< m: t1 >` is a subtype of `< m: t2 >` iff `t1`
is a subtype of `t2`.

To explore this, let's start building objects containing shapes by applying our
`make` function to a square and a circle.


```ocaml
# type square_val = < get: square; set: square -> unit >;;
type square_val = < get : square; set : square -> unit >
# let sq_val: square_val = make (square 3.0);;
val sq_val : square_val = <obj>
# type circle_val = < get: circle; set: circle -> unit >;;
type circle_val = < get : circle; set : circle -> unit >
# let circ_val: circle_val = make (circle 2.0);;
val circ_val : circle_val = <obj>
```

If we wanted to write a function that took a list of such objects and found the
total area of their shapes, we might try:

```ocaml
# type shape_val = < get: shape; set: shape -> unit >;;
type shape_val = < get : shape; set : shape -> unit >
let total_area (shape_vals: shape_val list) =
    List.fold ~init:0.0 ~f:(fun t s -> t +. s#get#area) shape_vals;;
val total_area : shape_val list -> float = <fun>
```

However, when we try to apply this function to our objects we get an error:

```ocaml
# total_area [ (sq_val :> shape_val); (circ_val :> shape_val) ];;
Characters 13-34:
  total_area [ (sq_val :> shape_val); (circ_val :> shape_val) ];;
               ^^^^^^^^^^^^^^^^^^^^^
Error: Type square_val = < get : square; set : square -> unit >
       is not a subtype of shape_val = < get : shape; set : shape -> unit > 
       Type shape = < area : float > is not a subtype of
       square = < area : float; width : float > 
```

As you can see, `square_val` and `circle_val` are not subtypes of `shape_val`.
The problem is with the `set` method.  For `shape_val`, the `set` method takes
an arbitrary `shape`.  So if we could coerce a `square_val` to a `shape_val`,
then it would be possible to set sq_val to an arbitrary shape, which would be
an error.

Another way of looking at this is that `< set: 'a -> unit; .. >` is
contravariant in `'a`, so `< set: square -> unit; get: square >` cannot be
subtype of `< set: shape -> unit; get: shape >`.

Still, the `total_area` function should be fine, in principle.  It doesn't call
`set`, so it isn't making that error.  To make it work, we need to use a more
precise type that indicates we are not going to be using the set method.  We
define a type `readonly_shape_val` and confirm that we can coerce the list of
shapes to it.

```ocaml
# type readonly_shape_val = < get : shape >;;
type readonly_shape_val = < get : shape >
# let total_area (shape_vals: readonly_shape_val list) =
    List.fold_left (fun t s -> t +. s#get#area) 0.0 shape_vals;;
val total_area : readonly_shape_val list -> float = <fun>
# total_area [ (sq_val :> readonly_shape_val); (circ_val :> readonly_shape_val) ];;
- : float = 21.5600000000000023
```

Aspects of this section may seem fairly complicated, but it should be pointed
out that this typing _works_, and in the end the type annotations are fairly
minor.  In most typed object-oriented languages, these coercions would simply
not be possible.  For example, in C++, a STL type `list<T>` is invariant in
`T`, it is simply not possible to use `list<square>` where `list<shape>` is
expected (at least safely).  The situation is similar in Java, although Java
supports has an escape hatch that allows the program to fall back to dynamic
typing.  The situation in OCaml is much better; it works, it is statically
checked, and the annotations are pretty simple.

### Narrowing

Narrowing, also called _down casting_, is the ability to coerce an
object to one of its subtypes.  For example, if we have a list of
shapes `shape list`, we might know (for some reason) what the actual
type of each shape is.  Perhaps we know that all objects in the list
have type `square`.  In this case, _narrowing_ would allow the
re-casting of the object from type `shape` to type `square`.  Many
languages support narrowing through dynamic type checking.  For example,
in Java, a coercion `(Square) x` is allowed if the value `x` has type
`Square` or one of its subtypes; otherwise the coercion throws an
exception.

Narrowing is *not permitted* in OCaml.  Period.

Why?  There are two reasonable explanations, one based on a design principle,
and another technical (the technical reason is simple: it is hard to
implement).

The design argument is this: narrowing violates abstraction.  In fact,
with a structural typing system like in OCaml, narrowing would
essentially provide the ability to enumerate the methods in an object.
To check whether an object `obj` has some method `foo : int`, one
would attempt a coercion `(obj :> < foo : int >)`.

More commonly, narrowing leads to poor object-oriented style.
Consider the following Java code, which returns the name of a shape
object.

```
String GetShapeName(Shape s) {
  if (s instanceof Square) {
    return "Square";
  } else if (s instanceof Circle) {
    return "Circle";
  } else {
    return "Other";
  }
}
```

Most programmers would consider this code to be "wrong."  Instead
of performing a case analysis on the type of object, it would be better
to define a method to return the name of the shape.  Instead of
calling `GetShapeName(s)`, we should call `s.Name()` instead.

However, the situation is not always so obvious.  The following code
checks whether an array of shapes looks like a "barbell," composed to
two `Circle` objects separated by a `Line`, where the circles have the
same radius.

```
boolean IsBarBell(Shape[] s) {
  return s.length == 3 && (s[0] instanceof Circle) &&
    (s[1] instanceof Line) && (s[2] instanceof Circle) &&
	((Circle) s[0]).radius() == ((Circle) s[2]).radius();
}
```

In this case, it is much less clear how to augment the `Shape` class
to support this kind of pattern analysis.  It is also not obvious that
object-oriented programming is well-suited for this situation.
Pattern matching seems like a better fit.

```ocaml
let is_bar_bell = function
 | [Circle r1; Line _; Circle r2] when r1 == r2 -> true
 | _ -> false;;
```
 
Regardless, there is a solution if you find yourself in this
situation, which is to augment the classes with variants.  You can
define a method `variant` that injects the actual object into a
variant type.
 
```ocaml
type shape = < variant : repr; area : float>
and circle = < variant : repr; area : float; radius : float >
and line = < variant : repr; area : float; length : float >
and repr =
 | Circle of circle
 | Line of line;;
 
let is_bar_bell = function
 | [s1; s2; s3] ->
   (match s1#variant, s2#variant, s3#variant with
     | Circle c1, Line _, Circle c2 when c1#radius == c2#radius -> true
	 | _ -> false)
 | _ -> false;;
```

This pattern works, but it has drawbacks.  In particular, the
recursive type definition should make it clear that this pattern is
essentially equivalent to using variants, and that objects do not
provide much value here.

### Subtyping vs. Row Polymorphism ###

There is a great deal of overlap between subtyping and row polymorphism. Row
polymorphism is in general preferred over subtyping because it does not require
explicit coercions, and it preserves more type information, allowing functions
like the following:

```ocaml
# let filter_large v =
  if v#get < 10.0 then Some v else None;;
val filter_large : (< get : float; .. > as 'a) -> 'a option = <fun>
```

The return type of this function is built from the open object type of its
argument, preserving any additional methods that it may have. 

```ocaml
# let v = make 5.0;;
val v : < get : float; set : float -> unit > = <obj>
# filter_large v;;
- : < get : float; set : float -> unit > option = Some <obj>
```

Writing a similar function with a closed type and applying it using subtyping
does not preserve the methods of the argument: the returned object is only
known to have a `get` method.

```ocaml
# let filter_large (v: < get: float >) =
  if v#get < 10.0 then Some v else None;;
  val filter_large : < get : float > -> < get : float > option = <fun>
# filter_large (v :> < get: float >);;
- : < get : float > option = Some <obj>
```

However, there are some situations where we cannot use row polymorphism. For
example, lists of heterogeneous elements can not be created using row
polymorphism:

```ocaml
# let hlist: < area: float; ..> list = [square 1.0; circle 3.0];;
Characters 50-60:
  let hlist: < area: float; ..> list = [square 1.0; circle 3.0];;
                                                    ^^^^^^^^^^
Error: This expression has type < area : float; radius : float >
       but an expression was expected of type < area : float; width : float >
       The second object type has no method radius
```

Since row polymorphism is a form of polymorphism, it also does not work well
with references:

```ocaml
# let shape_ref: < area: float; ..> ref = ref (square 4.0);;
val shape_ref : < area : float; width : float > ref = {contents = <obj>}
# shape_ref := circle 2.0;;
Characters 13-23:
  shape_ref := circle 2.0;;
               ^^^^^^^^^^
Error: This expression has type < area : float; radius : float >
       but an expression was expected of type < area : float; width : float >
       The second object type has no method radius
```

In these cases we must use subtyping:

```ocaml
# let hlist: shape list = [(square 1.0 :> shape); (circle 3.0 :> shape)];;
val hlist : shape list = [<obj>; <obj>]
# let shape_ref: shape ref = ref (square 4.0 :> shape);;
val shape_ref : shape ref = {contents = <obj>}
# shape_ref := (circle 2.0 :> shape);;
- : unit = ()
```

<note>
<title>Production note</title>

This chapter contains significant external contributions from Leo White.

</note>

