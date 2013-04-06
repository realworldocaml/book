Object Subtyping and Inheritance
================================

## Subtyping

Subtyping is a central concept in object-oriented programming.  It
governs when an object with one type _A_ can be used in an expression
that expects an object of another type _B_.  When this is true, we say
that _A_ is a _subtype_ of _B_.  Actually, more concretely, subtyping
determines when the coercion operator `e :> t` can be applied.  This
coercion works only if the expression `e` has some type `s` and `s` is
a subtype of `t`.

To explore this, let's define some simple classes for geometric
shapes.  The generic type `shape` has a method to compute the area,
and a `square` is a specific kind of shape.

```ocaml
type shape = < area : float >;;

class square w =
object (self : 'self)
  method area = self#width *. self#width
  method width = w
end;;
```

A `square` has a method `area` just like a `shape`, and an additional
method `width`.  Still, we expect a `square` to be a `shape`, and it
is.  The coercion `:>` must be explicit.

```ocaml
# let new_square x : shape = new square x;;
Characters 27-39:
  let new_square x : shape = new square x;;
                             ^^^^^^^^^^^^
Error: This expression has type square but an expression was expected of type shape
The second object type has no method width
# let new_square x : shape = (new square x :> shape);;
val new_square : float -> shape = <fun>
```

What are the rules for subtyping?  In general, object subtyping has
two general forms, called _width_ and _depth_ subtyping.  Width
subtyping means that an object type _A_ is a a subtype of _B_, if _A_
has all of the methods of _B_, and possibly more.  A `square` is a
subtype of `shape` because it implements all of the methods of `shape`
(the `area` method).

The subtyping rules are purely technical, they have no relation to
object semantics.  We can define a class `rectangle` that has all of
the methods of a `square`, so it is a subtype of square and can be
used wherever a `square` is expected.

```ocaml
# class rectangle h w =
  object (self : 'self)
     inherit square w
     method area = self#width *. self#height
     method height = h
  end;;
# let square_rectangle h w : square = (new rectangle h w :> square);;
val square_rectangle : float -> float -> square = <fun>
```

This may seem absurd, but this concept is expressible in all
object-oriented languages.  The contradiction is semantic -- we know
that in the real world, not all rectangles are squares; but in the
programming world, rectangles have all of the features of squares
(according to our definition), so they can be used just like squares.
Suffice it to say that it is usually better to avoid such apparent
contradictions.

Next, let's take a seemingly tiny step forward, and start building
collections of shapes.  It is easy enough to define a `slist` of
squares.

```ocaml
# let squares =
     let l = SList.make () in
     l#insert (new square 1.0);
     l#insert (new square 2.0);
     l;;
val squares : square slist = <obj>
```

We can also define a function to calculate the total area of a list of
shapes.  There is no reason to restrict this to squares, it should
work for any list of shapes with type `shape slist`.  The problem is
that doing so raises some serious typing questions -- can a `square
slist` be passed to a function that expects a `shape slist`?  If we
try it, the compiler produces a verbose error message.

```ocaml
# let total_area (l : shape slist) : float =
     let total = ref 0.0 in
     let it = l#iterator in
     while it#has_value do
        total := !total +. it#get#area;
	    it#next
     done;
     !total;;
val total_area : shape slist -> float = <fun>
# total_area squares;;
Characters 11-18:
  total_area squares;;
             ^^^^^^^
Error: This expression has type
         square slist =
           < insert : square -> unit; is_empty : bool;
             iterator : square iterator >
       but an expression was expected of type
         shape slist =
           < insert : shape -> unit; is_empty : bool;
             iterator : shape iterator >
       Type square = < area : float; width : float >
       is not compatible with type shape = < area : float > 
       The second object type has no method width
```

It might seem tempting to give up at this point, especially because
the subtyping is not even true -- the type `square slist` is not a
subtype of `shape slist`.  The problem is with the `insert` method.
For `shape slist`, the `insert` method takes an arbitrary `shape` and
inserts it into the list.  So if we could coerce a `square slist` to a
`shape slist`, then it would be possible to insert an arbitrary shape
into the list, which would be an error.

### Using more precise types to address subtyping problems ###

Still, the `total_area` function should be fine, in principle.  It
doesn't call `insert`, so it isn't making that error.  To make it
work, we need to use a more precise type that indicates we are not
going to be mutating the list.  We define a type
`readonly_shape_slist` and confirm that we can coerce the list of
squares.

```ocaml
# type readonly_shape_slist = < iterator : shape iterator >;;
type readonly_shape_slist = < iterator : shape iterator >
# (squares :> readonly_shape_slist);;
- : readonly_shape_slist = <obj>
# let total_area (l : readonly_shape_slist) : float = ...;;
val total_area : readonly_shape_slist -> float = <fun>
#   total_area (squares :> readonly_shape_slist);;
- : float = 5.
```

Why does this work, why is a `square slist` a subtype of
`readonly_shape_slist`.  The reasoning is in two steps.  First, the
easy part is width subtyping: we can drop the other methods to see
that `square slist` is a subtype of `< iterator : square iterator >`.
The next step is to use _depth_ subtyping, which, in its general form,
says that an object type `< m : t1 >` is a subtype of a type `< m :
t2>` iff `t1` is a subtype of `t2`.  In other words, instead of
reasoning about the number of methods in a type (the width), the
number of methods is fixed, and we look within the method types
themselves (the "depth").

In this particular case, depth subtyping on the `iterator` method
requires that `square iterator` be a subtype of `shape iterator`.
Expanding the type definition for the type `iterator`, we again invoke
depth subtyping, and we need to show that the type `< get : square >`
is a subtype of `<get : shape >`, which follows because `square` is a
subtype of `shape`.

This reasoning may seem fairly long and complicated, but it should be
pointed out that this typing _works_, and in the end the type
annotations are fairly minor.  In most typed object-oriented
languages, the coercion would simply not be possible.  For example, in
C++, a STL type `slist<T>` is invariant in `T`, it is simply not
possible to use `slist<square>` where `slist<shape>` is expected (at
least safely).  The situation is similar in Java, although Java
supports has an escape hatch that allows the program to fall back to
dynamic typing.  The situation in OCaml is much better; it works, it
is statically checked, and the annotations are pretty simple.

### Using elided types to address subtyping problems ###

Before we move to the next topic, there is one more thing to address.
The typing we gave above, using `readonly_shape_slist`, requires that
the caller perform an explicit coercion before calling the
`total_area` function.  We would like to give a better type that
avoids the coercion.

A solution is to use an elided type.  Instead of `shape`, we can use
the elided type `< area : float; .. >`.  In fact, once we do this, it
also becomes possible to use the `slist` type.

```ocaml
# let total_area (l : < area : float; .. > slist) : float = ...;;
val total_area : < area : float; .. > slist -> float = <fun>
# total_area squares;;
- : float = 5.
```

This works, and it removes the need for explicit coercions.  This type
is still fairly simple, but it does have the drawback that the
programmer needs to remember that the types `< area : float; ..>` and
`shape` are related.

OCaml supports an abbreviation in this case, but it works only for
classes, not object types.  The type expression `# classname` is an
abbreviation for an elided type containing all of the methods in the
named class, and more.  Since `shape` is an object type, we can't
write `#shape`.  However, if a
class definition is available, this
abbreviation can be useful.  The following definition is exactly
equivalent to the preceeding.

```ocaml
# class cshape = object method area = 0.0 end;;
class cshape : object method area : float end
# let total_area (l : #cshape list) : float = ...;;
val total_area : #cshape slist -> float = <fun>
# total_area squares;;
- : float = 5.
```

### Narrowing ###

Narrowing, also called _down casting_, is the ability to coerce an
object to one of its subtypes.  For example, if we have a list of
shapes `shape slist`, we might know (for some reason) what the actual
type of each shape is.  Perhaps we know that all objects in the list
have type `square`.  In this case, _narrowing_ would allow the
re-casting of the object from type `shape` to type `square`.  Many
languages support narrowing through dynamic type checking.  For example,
in Java, a coercion `(Square) x` is allowed if the value `x` has type
`Square` or one of its subtypes; otherwise the coercion throws an
exception.

Narrowing is *not permitted* in OCaml.  Period.

Why?  There are two reasonable explanations, one based on a design
principle, and another technical (the technical reason is
simple: it is hard to implement).

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

## Multiple inheritance ##

When a class inherits from more than one superclass, it is using
_multiple inheritance_.  Multiple inheritance extends the variety of
ways in which classes can be combined, and it can be quite useful,
particularly with virtual classes.  However, it can be tricky to use,
particularly when the inheritance hierarchy is a graph rather than a
tree, so it should be used with care.

### How names are resolved ###

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

### Mixins ###

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


  
