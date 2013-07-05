# Objects

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
C++, Java, C#, Ruby, Python or JavaScript.

</note>

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
# let s = object
    val mutable v = [0; 2]

    method pop =
      match v with
        hd :: tl -> 
          v <- tl;
          Some hd
      | [] -> None

    method push hd = 
      v <- hd :: v
  end;;
val s : < pop : int option; push : int -> unit > = <obj>
```

The object has an integer list value `v`, a method `pop` that returns the
head of `v`, and a method `push` that adds an integer to the head of
`v`.

The object type is enclosed in angle brackets `< ... >`, containing just the
types of the methods.  Fields, like `v`, are not part of the public
interface of an object.  All interaction with an object is through its methods.
The syntax for a method invocation (also called "sending a message" to the
object) uses the `#` character.

```ocaml
# s#pop;;
- : int option = Some 0
# s#push 4;;
- : unit = ()
# s#pop;;
- : int option = Some 4
```

Objects can also be constructed by functions.  If we want to specify
the initial value of the object, we can define a function that takes
the value and returns an object.

```ocaml
# let stack init = object
    val mutable v = init

    method pop =
      match v with
        hd :: tl -> 
          v <- tl;
          Some hd
      | [] -> None

    method push hd = 
      v <- hd :: v
  end;;
val stack : 'a list -> < pop : 'a option; push : 'a -> unit > = <fun>
# let s = stack [3; 2; 1];;
val s : < pop : int option; push : int -> unit > = <obj>
# s#pop;;
- : int option = Some 3
```

Note that the types of the function `stack` and the returned object now use the
polymorphic type `'a`.  When `stack` is invoked on a concrete value `[3; 2; 1]`, 
we get the same object type as before, with type `int` for the values on the 
stack.

## Object Polymorphism ##

Like polymorphic variants, methods can be used without an explicit type declaration.

```ocaml
# let area sq = sq#width ** 2.;;
val area : < width : float; .. > -> float = <fun>
# let minimize sq : unit = sq#resize 1.;;
val minimize : < resize : float -> unit; .. > -> unit = <fun>
# let limit sq = 
    if (area sq) > 10. then minimize sq;;
val limit : < resize : float -> unit; width : float; .. > -> unit = <fun>
```

As you can see object types are inferred automatically from the methods that
are invoked on them.

The type system will complain if it sees incompatible uses of the same method:

```ocaml
# let toggle sq b : unit = 
    if b then sq#resize `Fullscreen
    else minimize sq;;
Characters 76-78:
    else minimize sq;;
                  ^^
Error: This expression has type < resize : [> `Fullscreen ] -> unit; .. >
       but an expression was expected of type < resize : float -> unit; .. >
       Types for method resize are incompatible
```

The `..` in the inferred object types are ellipsis, standing for any other
methods.  The type `< width : float; .. >` specifies an object that must have
at least an `width` method, and possibly some others as well. Such object types
are said to be _open_.

We can manually _close_ an object type using a type annotation:

```ocaml
# let area_closed (r: < width : float >) = r#width ** 2.;;
val area_closed : < width : float > -> float = <fun>
# let sq = object
    method width = 3. 
    method name = "sq" 
  end;;
val sq : < name : string; width : float > = <obj>
# area_closed sq;;
Characters 12-14:
  area_closed sq;;
              ^^
Error: This expression has type < name : string; width : float >
       but an expression was expected of type < width : float >
       The second object type has no method name
```

<note>
<title>Elisions are polymorphic</title>

The `..` in an open object type is an elision, standing for "possibly more
methods."  It may not be apparent from the syntax, but an elided object type is
actually polymorphic.  If we try to write a type definition, we get an obscure
error.

```ocaml
# type square = < width : float; ..>;;
Characters 5-34:
  type square = < width : float; ..>;;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
In type < width : float; .. > as 'a the variable 'a is unbound
```

A `..` in an object type is called a _row variable_ and this typing scheme is
called _row polymorphism_.  Even though `..` doesn't look like a type variable,
it actually is.  Row polymorphism is also used in polymorphic variant types,
and there is a close relationship between objects and polymorphic variants:
objects are to records what polymorphic variants are to ordinary variants.

</note>

An object of type `< pop : int option; .. >` can be any object with a method
`pop : int option`, it doesn't matter how it is implemented.  When the method
`#pop` is invoked, the actual method that is run is determined by the object.

```ocaml
# let print_pop st = Option.iter ~f:(printf "Popped: %d\n") st#pop;;
val print_pop : < pop : int Core.Std.Option.t; .. > -> unit = <fun>
# print_pop (stack [5;4;3;2;1]);;
Popped: 5
- : unit = ()
# let t = object
    method pop = Some (Float.to_int (Time.to_float (Time.now ())))
  end;;
val t : < pop : int option > = <obj>
# print_pop t;;
Popped: 1372618419
- : unit = ()
```

## Immutable objects ##

Many people consider object-oriented programming to be intrinsically
imperative, where an object is like a state machine.  Sending a
message to an object causes it to change state, possibly sending
messages to other objects.

Indeed, in many programs, this makes sense, but it is by no means required.
Let's define a function that creates immutable stack objects.

```ocaml
# let imm_stack init = object
    val v = init

    method pop =
      match v with
        hd :: tl -> Some (hd, {< v = tl >})
      | [] -> None 

    method push hd = 
      {< v = hd :: v >}
  end;;
val imm_stack : 
  'a list -> (< pop : ('a * 'b) option; push : 'a -> 'b > as 'b) = <fun>
```

The key parts of this implementation are in the `pop` and `push` methods. The
expression `{< ... >}` produces a copy of the current object, with the same
type, and the specified fields updated.  In other words, the `push hd` method
produces a copy of the object, with `v` replaced by `hd :: v`.  The original
object is not modified.

```ocaml
# let s = imm_stack [3; 2; 1];;
val s : < pop : (int * 'a) option; push : int -> 'a > as 'a = <obj>
# let t = s#push 4;;
val t : < pop : (int * 'a) option; push : int -> 'a > as 'a = <obj>
# s#pop;;
- : (int * (< pop : 'a; push : int -> 'b > as 'b)) option as 'a =
Some (3, <obj>)
# t#pop;;
- : (int * (< pop : 'a; push : int -> 'b > as 'b)) option as 'a =
Some (4, <obj>)
```

There are some restriction on the use of the expression `{< ... >}`.
It can be used only within a method body, and only the values of
fields may be updated.  Method implementations are fixed at the time
the object is created, they cannot be changed dynamically.

## When to use objects

You might wonder when to use objects in OCaml, which has a multitude of
alternative mechanisms to express the similar concepts.  First-class modules
are more expressive (a module can include types, while classes and objects
cannot).  Modules, functors, and datatypes also offer a wide range of ways to
express program structure.  In fact, many seasoned OCaml programmers rarely use
classes and objects, if at all.

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

### Width subtyping

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

### Depth subtyping

We can also use _depth_ subtyping with objects. Depth subtyping, in its most
general form, says that an object type `< m: t1 >` is a subtype of `< m: t2 >`
iff `t1` is a subtype of `t2`.

For example, we can create two objects with a `shape` method:

```ocaml
# let coin = object
    method shape = circle 0.5
    method color = "silver"
  end
val coin : < color : string; shape : circle > = <obj>
# let map = object
    method shape = square 1.0
  end
val map : < shape : square > = <obj>
```

Both these objects have a `shape` method whose type is a subtype of the `shape`
type, so they can both be coerced into the object type `< shape : shape >`

```ocaml
# type item = < shape : shape >;;
type item = < shape : shape >
# let items = [ (coin :> item) ; (map :> item) ];;
val items : item list = [<obj>; <obj>]
```

### Polymorphic variant subtyping

Subtyping can also be used to coerce a polymorphic variant into a larger
polymorphic variant type.

```ocaml
# type num = [ `Int of int | `Float of float ];;
type num = [ `Float of float | `Int of int ]
# type const = [ num | `String of string ];;
type const = [ `Float of float | `Int of int | `String of string ]
# let n : num = `Int 3;;
val n : num = `Int 3
# let c : const = (n :> const);;
val c : const = `Int 3
```

### Variance

What about types built from object types? If a `square` is a `shape`, we expect
a `square list` to be a `shape list`. OCaml does indeed allow such coercions:

```ocaml
# let squares: square list = [ square 1.0; square 2.0 ];;
val squares : square list = [<obj>; <obj>]
# let shapes: shape list = (squares :> shape list);;
val shapes : shape list = [<obj>; <obj>]
```

Note that this relies on lists being immutable. It would not be safe to treat a
`square array` as a `shape array` because it would allow you to store
non-square shapes into what should be an array of squares. OCaml recognises
this and does not allow the coercion.

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

We say that `'a list` is _covariant_ (in `'a`), whilst `'a array` is
_invariant_. 

Subtyping function types requires a third class of variance. A function with
type `square -> string` cannot be used with type `shape -> string` because it
expects its argument to be a `square` and would not know what to do with a
`circle`. However, a function with type `shape -> string` _can_ safely be used
with type `square -> string`.

```ocaml
# let shape_to_string: shape -> string = 
    fun s -> sprintf "Shape(%F)" s#area;;
val shape_to_string : shape -> string = <fun>
# let square_to_string: square -> string = 
    (shape_to_string :> square -> string);;
val square_to_string : square -> string = <fun>
```

We say that `'a -> string` is _contravariant_ in `'a`. In general, function types
are contravariant in their arguments and covariant in their results.

<note>
<title>Variance annotations</title>

OCaml works out the variance of a type using that type's definition. 

```ocaml
# module Either = struct
  type ('a, 'b) t = 
      Left of 'a
    | Right of 'b
end;;
module Either : sig type ('a, 'b) t = Left of 'a | Right of 'b end
# let sq : (square, circle) Either.t = Either.Left (square 4.0);;
val sq : (square, circle) Either.t = Either.Left <obj>
# let sh : (shape, shape) Either.t = (sq :> (shape, shape) Either.t);;
val sh : (shape, shape) Either.t = Either.Left <obj>
```

However, if the definition is hidden by a signature then OCaml is forced to
assume that the type is invariant.

```ocaml
# module Either : sig 
    type ('a, 'b) t
    val left: 'a -> ('a, 'b) t
    val right: 'b -> ('a, 'b) t
  end = struct
    type ('a, 'b) t = 
        Left of 'a
      | Right of 'b
    let left x = Left x
    let right x = Right x
  end;;
module Either :
  sig
    type ('a, 'b) t
    val left : 'a -> ('a, 'b) t
    val right : 'b -> ('a, 'b) t
  end
# let sq : (square, circle) Either.t = Either.left (square 4.0);;
val sq : (square, circle) Either.t = <abstr>
# let sh : (shape, shape) Either.t = (sq :> (shape, shape) Either.t);;
Characters 35-66:
  let sh : (shape, shape) Either.t = (sq :> (shape, shape) Either.t);;
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type (square, circle) Either.t is not a subtype of
         (shape, shape) Either.t 
       Type square = < area : float; width : float > is not compatible with type
         shape = < area : float > 
       The second object type has no method width
```

We can fix this by adding _variance annotations_ to the type's parameters in the signature: `+`
for covariance or `-` for contravariance.

```ocaml
# module Either : sig 
    type (+'a, +'b) t
    val left: 'a -> ('a, 'b) t
    val right: 'b -> ('a, 'b) t
  end = struct
    type ('a, 'b) t = 
        Left of 'a
      | Right of 'b
    let left x = Left x
    let right x = Right x
  end;;
module Either :
  sig
    type (+'a, +'b) t
    val left : 'a -> ('a, 'b) t
    val right : 'b -> ('a, 'b) t
  end
# let sq : (square, circle) Either.t = Either.left (square 4.0);;
val sq : (square, circle) Either.t = <abstr>
# let sh : (shape, shape) Either.t = (sq :> (shape, shape) Either.t);;
val sh : (shape, shape) Either.t = <abstr>
```

</note>

For a more concrete example of variance, let's create some stacks containing
shapes by applying our `stack` function to some squares and some circles.


```ocaml
# type 'a stack = < pop: 'a option; push: 'a -> unit >;;
type 'a stack = < pop : 'a option; push : 'a -> unit >
# let square_stack: square stack = stack [square 3.0; square 1.0];;
val square_stack : square stack = <obj>
# let circle_stack: circle stack = stack [circle 2.0; circle 4.0];;
val circle_stack : circle stack = <obj>
```

If we wanted to write a function that took a list of such stacks and found the
total area of their shapes, we might try:

```ocaml
# let total_area (shape_stacks: shape stack list) =
    let stack_area acc st = 
      let rec loop acc =
        match st#pop with
          Some s -> loop (acc +. s#area)
        | None -> acc
      in
        loop acc
    in
      List.fold ~init:0.0 ~f:stack_area shape_stacks;;
val total_area : shape stack list -> float = <fun>
```

However, when we try to apply this function to our objects we get an error:

```ocaml
# total_area [(square_stack :> shape stack); (circle_stack :> shape stack)];;
Characters 12-41:
  total_area [(square_stack :> shape stack); (circle_stack :> shape stack)];;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type square stack = < pop : square option; push : square -> unit >
       is not a subtype of
         shape stack = < pop : shape option; push : shape -> unit > 
       Type shape = < area : float > is not a subtype of
         square = < area : float; width : float > 
```

As you can see, `square stack` and `circle stack` are not subtypes of `shape
stack`.  The problem is with the `push` method.  For `shape stack`, the `push`
method takes an arbitrary `shape`.  So if we could coerce a `square stack` to a
`shape stack`, then it would be possible to push an arbitrary shape onto
`square stack`, which would be an error.

Another way of looking at this is that `< push: 'a -> unit; .. >` is
contravariant in `'a`, so `< push: square -> unit; pop: square option >` cannot be a
subtype of `< push: shape -> unit; pop: shape option >`.

Still, the `total_area` function should be fine, in principle.  It doesn't call
`push`, so it isn't making that error.  To make it work, we need to use a more
precise type that indicates we are not going to be using the set method.  We
define a type `readonly_stack` and confirm that we can coerce the list of
stacks to it.

```ocaml
# type 'a readonly_stack = < pop : 'a option >;;
type 'a readonly_stack = < pop : 'a option >
# let total_area (shape_stacks: shape readonly_stack list) =
    let stack_area acc st = 
      let rec loop acc =
        match st#pop with
          Some s -> loop (acc +. s#area)
        | None -> acc
      in
        loop acc
    in
      List.fold ~init:0.0 ~f:stack_area shape_stacks;;
val total_area : shape readonly_stack list -> float = <fun>
# total_area [(square_stack :> shape readonly_stack); (circle_stack :> shape readonly_stack)];;
- : float = 72.8000000000000114
```

Aspects of this section may seem fairly complicated, but it should be pointed
out that this typing _works_, and in the end the type annotations are fairly
minor.  In most typed object-oriented languages, these coercions would simply
not be possible.  For example, in C++, a STL type `list<T>` is invariant in
`T`, it is simply not possible to use `list<square>` where `list<shape>` is
expected (at least safely).  The situation is similar in Java, although Java
has an escape hatch that allows the program to fall back to dynamic typing.
The situation in OCaml is much better; it works, it is statically checked, and
the annotations are pretty simple.

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
boolean IsBarbell(Shape[] s) {
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
let is_barbell = function
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
 
let is_barbell = function
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
# let remove_large l = 
    List.filter ~f:(fun s -> s#area < 10.0) l;;
val remove_large :
  (< area : float; .. > as 'a) Core.Std.List.t -> 'a Core.Std.List.t = <fun>
```

The return type of this function is built from the open object type of its
argument, preserving any additional methods that it may have. 

```ocaml
# let squares : < area : float; width : float > list = 
    [square 3.0; square 4.0; square 2.0];;
val squares : < area : float; width : float > list = [<obj>; <obj>; <obj>]
# remove_large squares;;
- : < area : float; width : float > Core.Std.List.t = [<obj>; <obj>]
```

Writing a similar function with a closed type and applying it using subtyping
does not preserve the methods of the argument: the returned object is only
known to have an `area` method.

```ocaml
# let remove_large (l: < area : float > list) = 
    List.filter ~f:(fun s -> s#area < 10.0) l;;
val remove_large : < area : float > list -> < area : float > Core.Std.List.t =
  <fun>
# remove_large (squares :> < area : float > list );;
- : < area : float > Core.Std.List.t = [<obj>; <obj>]
```

However, there are some situations where we cannot use row polymorphism. For
example, lists of heterogeneous elements can not be created using row
polymorphism:

```ocaml
# let hlist: < area: float; ..> list = [square 1.0; circle 3.0];;
Characters 50-60:
  let hlist: < area: float; ..> list = [square 1.0; circle 3.0];;
                                                    ^^^^^^^^^^
Error: This expression has type circle but an expression was expected of type
         square
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
Error: This expression has type circle but an expression was expected of type
         < area : float; width : float >
       The first object type has no method width
```

In both these cases we must use subtyping:

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

