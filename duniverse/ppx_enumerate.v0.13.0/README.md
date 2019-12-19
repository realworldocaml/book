ppx_enumerate
=============

Generate a list containing all values of a finite type.

`ppx_enumerate` is a ppx rewriter which generates a definition for the
list of all values of a type with (for a type which only has finitely
many values).

Basic Usage
-----------

The basic usage is simply to add "[@@deriving enumerate]" after the type
definition.  For example:

```ocaml
type t =
  | Foo
  | Bar of bool
  | Baz of [`A | `B of unit option]
  [@@deriving enumerate]
```

will produce a value `val all : t list`, whose value is equal to

```ocaml
[ Foo; Bar true; Bar false; Baz `A; Baz (`B None); Baz (`B Some ()) ]
```

in some order (that is, there is no guarantee about the order of the list).

Polymorphic types
-----------------

In a similar fashion as sexplib, using '[@@deriving enumerate]' on polymorphic
types produces a function for [all].  For example,

```ocaml
type 'a t =
  | Foo
  | Bar of 'a option
  [@@deriving enumerate]
```

will produce a value `val all : 'a list -> 'a t list`, whose value is
semantically equal to

```ocaml
fun all_of_a -> Foo :: Bar None :: List.map all_of_a ~f:(fun x -> Bar (Some x))
```

Types not named `t`
-------------------

If the type is not named `t`, then the enumeration is called
`all_of_<type_name>` instead of `all`.

Records and Tuples
------------------

Product types are supported as well as sum types.  For example,

```ocaml
type t =
  { foo : [`A | `B]
  ; bar : [`C | `D]
  } [@@deriving enumerate]
```

produces a `val all : t list` whose value is equal (up to order) to:

```ocaml
[ { foo = `A; bar = `C }; { foo = `A; bar = `D };
  { foo = `B; bar = `C }; { foo = `B; bar = `D };
]
```

Tuples and variants with multiple arguments are similarly supported.

Overriding the `all` value
---------------------------

Just like with sexplib, it can sometimes be useful to provide a custom
value of `all`.  For example, you might define a type of bounded
integers:

```ocaml
module Small_int : sig
  type t = private int [@@deriving enumerate]
  val create_exn : int -> t
end = struct
  type t = int
  let limit = 100
  let create_exn i = if i < 0 || i >= limit then failwith "out of bounds"; i
  let all = List.init limit ~f:(fun i -> i)
end
```

You could then use `Small_int.t` as normal with other types using
`[@@deriving enumerate]`:

```ocaml
type t =
  | Foo
  | Bar of Small_int.t option
  [@@deriving enumerate]
```

Using `all` without defining a type name
----------------------------------------

You don't have to define a type name to be able to create the list of
values of a type. You do it for any type expression by using the `all`
quotation. For example:

```ocaml
[%all: bool * bool]
```

which will evaluate to:

```ocaml
[ (true, true); (true, false); (false, false); (false, true) ]
```

Known issues
------------

Using `all` for polymorphic variants with duplicated constructors leads
to duplicate values in the resulting lists:

```ocaml
type t = [ `A ] [@@deriving enumerate]
let () = assert ([%all: [ t | t ] ] = [ `A; `A ])
```
