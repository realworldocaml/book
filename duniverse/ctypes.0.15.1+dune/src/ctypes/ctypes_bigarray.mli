(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** {2 Types} *)

type ('a, 'b, 'l) t
(** The type of bigarray values of particular sizes.  A value of type
    [(a, b, l) t] can be used to read and write values of type [b].  *)

(** {3 Type constructors} *)

val bigarray : int array -> ('a, 'b) Bigarray.kind -> 'l Bigarray.layout ->
  ('a, ('a, 'b, 'l) Bigarray.Genarray.t, 'l) t
(** Create a {!t} value for the {!Bigarray.Genarray.t} type. *)

val bigarray1 : int -> ('a, 'b) Bigarray.kind -> 'l Bigarray.layout ->
  ('a, ('a, 'b, 'l) Bigarray.Array1.t, 'l) t
(** Create a {!t} value for the {!Bigarray.Array1.t} type. *)

val bigarray2 : int -> int -> ('a, 'b) Bigarray.kind -> 'l Bigarray.layout ->
  ('a, ('a, 'b, 'l) Bigarray.Array2.t, 'l) t
(** Create a {!t} value for the {!Bigarray.Array2.t} type. *)

val bigarray3 : int -> int -> int -> ('a, 'b) Bigarray.kind -> 'l Bigarray.layout ->
  ('a, ('a, 'b, 'l) Bigarray.Array3.t, 'l) t
(** Create a {!t} value for the {!Bigarray.Array3.t} type. *)

val prim_of_kind : ('a, _) Bigarray.kind -> 'a Ctypes_primitive_types.prim
(** Create a {!Ctypes_ptr.Types.ctype} for a {!Bigarray.kind}. *)

(** {3 Type eliminators} *)

val sizeof : (_, _, _) t -> int
(** Compute the size of a bigarray type. *)

val alignment : (_, _, _) t -> int
(** Compute the alignment of a bigarray type. *)

val element_type : ('a, _, _) t -> 'a Ctypes_primitive_types.prim
(** Compute the element type of a bigarray type. *)

val dimensions : (_, _, _) t -> int array
(** Compute the dimensions of a bigarray type. *)

val type_expression : ('a, 'b, 'l) t -> ([> `Appl of string list * 'c list
                                         |  `Ident of string list ] as 'c)
(** Compute a type expression that denotes a bigarray type. *)

(** {2 Values} *)

val unsafe_address : 'a -> Ctypes_ptr.voidp
(** Return the address of a bigarray value.  This function is unsafe because
    it dissociates the raw address of the C array from the OCaml object that
    manages the lifetime of the array.  If the caller does not hold a
    reference to the OCaml object then the array might be freed, invalidating
    the address. *)

val view : (_, 'a, _) t -> _ Ctypes_ptr.Fat.t -> 'a
(** [view b ptr] creates a bigarray view onto existing memory.

    If [ptr] references an OCaml object then [view] will ensure that
    that object is not collected before the bigarray returned by
    [view]. *)
