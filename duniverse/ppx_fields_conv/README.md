ppx_fields_conv
===============

Generation of accessor and iteration functions for ocaml records.

`ppx_fields_conv` is a ppx rewriter that can be used to define first
class values representing record fields, and additional routines, to
get and set record fields, iterate and fold over all fields of a
record and create new record values.

Basic Usage
-----------

If you define a type as follows:

<!--BEGIN type_decl-->
```ocaml
type t = {
  dir : [ `Buy | `Sell ];
  quantity : int;
  price : float;
  mutable cancelled : bool;
} [@@deriving fields]
```
<!--END-->

then code will be generated for functions of the following type:

<!--BEGIN generated_sig-->
```ocaml
(* getters *)
val cancelled : t -> bool
val price     : t -> float
val quantity  : t -> int
val dir       : t -> [ `Buy | `Sell ]

(* setters *)
val set_cancelled : t -> bool -> unit

(* higher order fields and functions over all fields *)
module Fields : sig

  val names : string list

  val cancelled : (t, bool            ) Field.t
  val price     : (t, float           ) Field.t
  val quantity  : (t, int             ) Field.t
  val dir       : (t, [ `Buy | `Sell ]) Field.t

  val create
    :  dir:[ `Buy | `Sell ]
    -> quantity  : int
    -> price     : float
    -> cancelled : bool
    -> t

  val make_creator
    :  dir:      ((t, [ `Buy | `Sell ]) Field.t -> 'a -> ('arg -> [ `Buy | `Sell ]) * 'b)
    -> quantity: ((t, int             ) Field.t -> 'b -> ('arg -> int             ) * 'c)
    -> price:    ((t, float           ) Field.t -> 'c -> ('arg -> float           ) * 'd)
    -> cancelled:((t, bool            ) Field.t -> 'd -> ('arg -> bool            ) * 'e)
    -> 'a -> ('arg -> t) * 'e

  val fold
    :  init:'a
    -> dir      :('a -> (t, [ `Buy | `Sell ]) Field.t -> 'b)
    -> quantity :('b -> (t, int             ) Field.t -> 'c)
    -> price    :('c -> (t, float           ) Field.t -> 'd)
    -> cancelled:('d -> (t, bool            ) Field.t -> 'e)
    -> 'e

  val map
    :  dir      :((t, [ `Buy | `Sell ]) Field.t -> [ `Buy | `Sell ])
    -> quantity :((t, int             ) Field.t -> int)
    -> price    :((t, float           ) Field.t -> float)
    -> cancelled:((t, bool            ) Field.t -> bool)
    -> t

  val iter
    :  dir      :((t, [ `Buy | `Sell ]) Field.t -> unit)
    -> quantity :((t, int             ) Field.t -> unit)
    -> price    :((t, float           ) Field.t -> unit)
    -> cancelled:((t, bool            ) Field.t -> unit)
    -> unit

  val for_all
    :  dir      :((t, [ `Buy | `Sell ]) Field.t -> bool)
    -> quantity :((t, int             ) Field.t -> bool)
    -> price    :((t, float           ) Field.t -> bool)
    -> cancelled:((t, bool            ) Field.t -> bool)
    -> bool

  val exists
    :  dir      :((t, [ `Buy | `Sell ]) Field.t -> bool)
    -> quantity :((t, int             ) Field.t -> bool)
    -> price    :((t, float           ) Field.t -> bool)
    -> cancelled:((t, bool            ) Field.t -> bool)
    -> bool

  val to_list
    :  dir      :((t, [ `Buy | `Sell ]) Field.t -> 'a)
    -> quantity :((t, int             ) Field.t -> 'a)
    -> price    :((t, float           ) Field.t -> 'a)
    -> cancelled:((t, bool            ) Field.t -> 'a)
    -> 'a list

  val map_poly : ([< `Read | `Set_and_create ], t, 'a) Field.user -> 'a list

  (** Functions that take a record directly *)
  module Direct : sig

      val fold
        :  t
        -> init:'a
        -> dir      :('a -> (t, [ `Buy | `Sell ]) Field.t -> t -> [ `Buy | `Sell ] -> 'b)
        -> quantity :('b -> (t, int             ) Field.t -> t -> int              -> 'c)
        -> price    :('c -> (t, float           ) Field.t -> t -> float            -> 'd)
        -> cancelled:('d -> (t, bool            ) Field.t -> t -> bool             -> 'e)
        -> 'e

      val map
        :  t
        -> dir      :((t, [ `Buy | `Sell ]) Field.t -> t -> [ `Buy | `Sell ] -> [ `Buy | `Sell ])
        -> quantity :((t, int             ) Field.t -> t -> int              -> int)
        -> price    :((t, float           ) Field.t -> t -> float            -> float)
        -> cancelled:((t, bool            ) Field.t -> t -> bool             -> bool)
        -> t

      val iter
        :  t
        -> dir      :((t, [ `Buy | `Sell ]) Field.t -> t -> [ `Buy | `Sell ] -> unit)
        -> quantity :((t, int             ) Field.t -> t -> int              -> unit)
        -> price    :((t, float           ) Field.t -> t -> float            -> unit)
        -> cancelled:((t, bool            ) Field.t -> t -> bool             -> unit)
        -> unit

      val for_all
        :  t
        -> dir      :((t, [ `Buy | `Sell ]) Field.t -> t -> [ `Buy | `Sell ] -> bool)
        -> quantity :((t, int             ) Field.t -> t -> int              -> bool)
        -> price    :((t, float           ) Field.t -> t -> float            -> bool)
        -> cancelled:((t, bool            ) Field.t -> t -> bool             -> bool)
        -> bool

      val exists
        :  t
        -> dir      :((t, [ `Buy | `Sell ]) Field.t -> t -> [ `Buy | `Sell ] -> bool)
        -> quantity :((t, int             ) Field.t -> t -> int              -> bool)
        -> price    :((t, float           ) Field.t -> t -> float            -> bool)
        -> cancelled:((t, bool            ) Field.t -> t -> bool             -> bool)
        -> bool

      val to_list
        :  t
        -> dir      :((t, [ `Buy | `Sell ]) Field.t -> t -> [ `Buy | `Sell ] -> 'a)
        -> quantity :((t, int             ) Field.t -> t -> int              -> 'a)
        -> price    :((t, float           ) Field.t -> t -> float            -> 'a)
        -> cancelled:((t, bool            ) Field.t -> t -> bool             -> 'a)
        -> 'a list
        
      val set_all_mutable_fields : t -> cancelled:bool -> unit
    end

end
```
<!--END-->

Use of `[@@deriving fields]` in an .mli will extend the signature for
functions with the above types; In an .ml, definitions will be
generated.

Field.t is defined in Fieldslib, including:

```ocaml
type ('perm, 'record, 'field) t_with_perm

type ('record, 'field) t = ([ `Read | `Set_and_create], 'record, 'field) t_with_perm

val name : (_, _, _) t_with_perm -> string
val get  : (_, 'r, 'a) t_with_perm -> 'r -> 'a
```

Functions over all fields
-------------------------

Use of the generated functions together with `Fieldslib` allow us to
define functions over t which check exhaustiveness w.r.t record
fields, avoiding common semantic errors which can occur when a record
is extended with new fields but we forget to update functions.

For example if you are writing a custom equality operator to ignore
small price differences:

```ocaml
let ( = ) a b : bool =
  let use op = fun field ->
    op (Field.get field a) (Field.get field b)
  in
  let price_equal p1 p2 = Float.abs (p1 -. p2) < 0.001 in
  Fields.for_all
    ~dir:(use (=)) ~quantity:(use (=))
    ~price:(use price_equal) ~cancelled:(use (=))
;;
```

A type error would occur if you were to add a new field and not change
the definition of `( = )`:

```ocaml
type t = {
  dir : [ `Buy | `Sell ];
  quantity : int;
  price : float;
  mutable cancelled : bool;
  symbol : string;
} [@@deriving fields]

...
Error: This expression has type
         symbol:(([< `Read | `Set_and_create ], t, string) Field.t_with_perm ->
                 bool) ->
         bool
       but an expression was expected of type bool
```

Or similarly you could use `fold` to create `to_string` function:

```ocaml
let to_string t =
  let conv to_s = fun acc f ->
    (sprintf "%s: %s" (Field.name f) (to_s (Field.get f t))) :: acc
  in
  let fs =
    Fields.fold ~init:[]
      ~dir:(conv (function `Buy -> "Buy" | `Sell -> "Sell"))
      ~quantity:(conv Int.to_string)
      ~price:(conv Float.to_string)
      ~cancelled:(conv Bool.to_string)
  in
  String.concat fs ~sep:", "
;;
```

Addition of a new field would cause a type error reminding you to
update the definition of `to_string`.
