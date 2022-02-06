(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type construction *)

[@@@warning "-9"]

exception IncompleteType
exception ModifyingSealedType of string
exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type incomplete_size = { mutable isize: int }

type structured_spec = { size: int; align: int; }

type 'a structspec =
    Incomplete of incomplete_size
  | Complete of structured_spec

type abstract_type = {
  aname : string;
  asize : int;
  aalignment : int;
}

type _ ocaml_type =
  String     : string ocaml_type
| Bytes      : bytes ocaml_type
| FloatArray : float array ocaml_type

type _ typ =
    Void            :                       unit typ
  | Primitive       : 'a Ctypes_primitive_types.prim -> 'a typ
  | Pointer         : 'a typ             -> 'a ptr typ
  | Funptr          : 'a fn              -> 'a static_funptr typ
  | Struct          : 'a structure_type  -> 'a structure typ
  | Union           : 'a union_type      -> 'a union typ
  | Abstract        : abstract_type      -> 'a abstract typ
  | View            : ('a, 'b) view      -> 'a typ
  | Array           : 'a typ * int       -> 'a carray typ
  | Bigarray        : (_, 'a, _) Ctypes_bigarray.t
                                         -> 'a typ
  | OCaml           : 'a ocaml_type      -> 'a ocaml typ
and 'a carray = { astart : 'a ptr; alength : int }
and ('a, 'kind) structured = { structured : ('a, 'kind) structured ptr } [@@unboxed]
and 'a union = ('a, [`Union]) structured
and 'a structure = ('a, [`Struct]) structured
and 'a abstract = ('a, [`Abstract]) structured
and (_, _) pointer =
  CPointer : (Obj.t option,'a typ) Ctypes_ptr.Fat.t -> ('a, [`C]) pointer
| OCamlRef : int * 'a * 'a ocaml_type -> ('a, [`OCaml]) pointer
and 'a ptr = ('a, [`C]) pointer
and 'a ocaml = ('a, [`OCaml]) pointer
and 'a static_funptr =
  Static_funptr : (Obj.t option, 'a fn) Ctypes_ptr.Fat.t -> 'a static_funptr
and ('a, 'b) view = {
  read : 'b -> 'a;
  write : 'a -> 'b;
  format_typ: ((Format.formatter -> unit) -> Format.formatter -> unit) option;
  format: (Format.formatter -> 'a -> unit) option;
  ty: 'b typ;
}
and ('a, 's) field = {
  ftype: 'a typ;
  foffset: int;
  fname: string;
}
and 'a structure_type = {
  tag: string;
  mutable spec: 'a structspec;
  (* fields are in reverse order iff the struct type is incomplete *)
  mutable fields : 'a structure boxed_field list;
}
and 'a union_type = {
  utag: string;
  mutable uspec: structured_spec option;
  (* fields are in reverse order iff the union type is incomplete *)
  mutable ufields : 'a union boxed_field list;
}
and 's boxed_field = BoxedField : ('a, 's) field -> 's boxed_field
and _ fn =
  | Returns  : 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn

type _ bigarray_class =
  Genarray :
  < element: 'a;
    layout: 'l;
    dims: int array;
    ba_repr: 'b;
    bigarray: ('a, 'b, 'l) Bigarray.Genarray.t;
    carray: 'a carray > bigarray_class
| Array1 :
  < element: 'a;
    layout: 'l;
    dims: int;
    ba_repr: 'b;
    bigarray: ('a, 'b, 'l) Bigarray.Array1.t;
    carray: 'a carray > bigarray_class
| Array2 :
  < element: 'a;
    layout: 'l;
    dims: int * int;
    ba_repr: 'b;
    bigarray: ('a, 'b, 'l) Bigarray.Array2.t;
    carray: 'a carray carray > bigarray_class
| Array3 :
  < element: 'a;
    layout: 'l;
    dims: int * int * int;
    ba_repr: 'b;
    bigarray: ('a, 'b, 'l) Bigarray.Array3.t;
    carray: 'a carray carray carray > bigarray_class

type boxed_typ = BoxedType : 'a typ -> boxed_typ

let rec sizeof : type a. a typ -> int = function
    Void                           -> raise IncompleteType
  | Primitive p                    -> Ctypes_primitives.sizeof p
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { spec = Complete
      { size } }                   -> size
  | Union { uspec = None }         -> raise IncompleteType
  | Union { uspec = Some { size } }
                                   -> size
  | Array (t, i)                   -> i * sizeof t
  | Bigarray ba                    -> Ctypes_bigarray.sizeof ba
  | Abstract { asize }             -> asize
  | Pointer _                      -> Ctypes_primitives.pointer_size
  | Funptr _                       -> Ctypes_primitives.pointer_size
  | OCaml _                        -> raise IncompleteType
  | View { ty }                    -> sizeof ty

let rec alignment : type a. a typ -> int = function
    Void                             -> raise IncompleteType
  | Primitive p                      -> Ctypes_primitives.alignment p
  | Struct { spec = Incomplete _ }   -> raise IncompleteType
  | Struct { spec = Complete
      { align } }                    -> align
  | Union { uspec = None }           -> raise IncompleteType
  | Union { uspec = Some { align } } -> align
  | Array (t, _)                     -> alignment t
  | Bigarray ba                      -> Ctypes_bigarray.alignment ba
  | Abstract { aalignment }          -> aalignment
  | Pointer _                        -> Ctypes_primitives.pointer_alignment
  | Funptr _                         -> Ctypes_primitives.pointer_alignment
  | OCaml _                          -> raise IncompleteType
  | View { ty }                      -> alignment ty

let rec passable : type a. a typ -> bool = function
    Void                           -> true
  | Primitive _                    -> true
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { spec = Complete _ }   -> true
  | Union  { uspec = None }        -> raise IncompleteType
  | Union  { uspec = Some _ }      -> true
  | Array _                        -> false
  | Bigarray _                     -> false
  | Pointer _                      -> true
  | Funptr _                       -> true
  | Abstract _                     -> false
  | OCaml _                        -> true
  | View { ty }                    -> passable ty

(* Whether a value resides in OCaml-managed memory.
   Values that reside in OCaml memory cannot be accessed
   when the runtime lock is not held. *)
let rec ocaml_value : type a. a typ -> bool = function
    Void        -> false
  | Primitive _ -> false
  | Struct _    -> false
  | Union _     -> false
  | Array _     -> false
  | Bigarray _  -> false
  | Pointer _   -> false
  | Funptr _    -> false
  | Abstract _  -> false
  | OCaml _     -> true
  | View { ty } -> ocaml_value ty

let rec has_ocaml_argument : type a. a fn -> bool = function
    Returns _ -> false
  | Function (t, _) when ocaml_value t -> true
  | Function (_, t) -> has_ocaml_argument t

let void = Void
let char = Primitive Ctypes_primitive_types.Char
let schar = Primitive Ctypes_primitive_types.Schar
let float = Primitive Ctypes_primitive_types.Float
let double = Primitive Ctypes_primitive_types.Double
let ldouble = Primitive Ctypes_primitive_types.LDouble
let complex32 = Primitive Ctypes_primitive_types.Complex32
let complex64 = Primitive Ctypes_primitive_types.Complex64
let complexld = Primitive Ctypes_primitive_types.Complexld
let short = Primitive Ctypes_primitive_types.Short
let int = Primitive Ctypes_primitive_types.Int
let sint = Primitive Ctypes_primitive_types.Sint
let long = Primitive Ctypes_primitive_types.Long
let llong = Primitive Ctypes_primitive_types.Llong
let nativeint = Primitive Ctypes_primitive_types.Nativeint
let int8_t = Primitive Ctypes_primitive_types.Int8_t
let int16_t = Primitive Ctypes_primitive_types.Int16_t
let int32_t = Primitive Ctypes_primitive_types.Int32_t
let int64_t = Primitive Ctypes_primitive_types.Int64_t
let camlint = Primitive Ctypes_primitive_types.Camlint
let uchar = Primitive Ctypes_primitive_types.Uchar
let bool = Primitive Ctypes_primitive_types.Bool
let uint8_t = Primitive Ctypes_primitive_types.Uint8_t
let uint16_t = Primitive Ctypes_primitive_types.Uint16_t
let uint32_t = Primitive Ctypes_primitive_types.Uint32_t
let uint64_t = Primitive Ctypes_primitive_types.Uint64_t
let size_t = Primitive Ctypes_primitive_types.Size_t
let ushort = Primitive Ctypes_primitive_types.Ushort
let uint = Primitive Ctypes_primitive_types.Uint
let ulong = Primitive Ctypes_primitive_types.Ulong
let ullong = Primitive Ctypes_primitive_types.Ullong
let array i t = Array (t, i)
let ocaml_string = OCaml String
let ocaml_bytes = OCaml Bytes
let ocaml_float_array = OCaml FloatArray
let ptr t = Pointer t
let ( @->) f t =
  if not (passable f) then
    raise (Unsupported "Unsupported argument type")
  else
    Function (f, t)
let abstract ~name ~size ~alignment =
  Abstract { aname = name; asize = size; aalignment = alignment }
let view ?format_typ ?format ~read ~write ty =
  View { read; write; format_typ; format; ty }
let id v = v
let typedef old name =
  view ~format_typ:(fun k fmt -> Format.fprintf fmt "%s%t" name k)
    ~read:id ~write:id old

let bigarray_ : type a b c d e l.
  < element: a;
    layout: l;
    dims: b;
    ba_repr: c;
    bigarray: d;
    carray: e > bigarray_class -> 
   b -> (a, c) Bigarray.kind -> l Bigarray.layout -> d typ =
  fun spec dims kind l -> match spec with
  | Genarray -> Bigarray (Ctypes_bigarray.bigarray dims kind l)
  | Array1 -> Bigarray (Ctypes_bigarray.bigarray1 dims kind l)
  | Array2 -> let d1, d2 = dims in
              Bigarray (Ctypes_bigarray.bigarray2 d1 d2 kind l)
  | Array3 -> let d1, d2, d3 = dims in
              Bigarray (Ctypes_bigarray.bigarray3 d1 d2 d3 kind l)

let bigarray spec c k = bigarray_ spec c k Bigarray.c_layout
let fortran_bigarray spec c k = bigarray_ spec c k Bigarray.fortran_layout

let returning v =
  if not (passable v) then
    raise (Unsupported "Unsupported return type")
  else
    Returns v
let static_funptr fn = Funptr fn

let structure tag =
  Struct { spec = Incomplete { isize = 0 }; tag; fields = [] }

let union utag = Union { utag; uspec = None; ufields = [] }

let offsetof { foffset } = foffset
let field_type { ftype } = ftype
let field_name { fname } = fname

(* This corresponds to the enum in ctypes_primitives.h *)
type arithmetic =
    Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float
  | Double
