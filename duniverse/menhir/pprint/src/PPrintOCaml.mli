(**************************************************************************)
(*                                                                        *)
(*  PPrint                                                                *)
(*                                                                        *)
(*  François Pottier, Inria Paris                                         *)
(*  Nicolas Pouillard                                                     *)
(*                                                                        *)
(*  Copyright 2007-2019 Inria. All rights reserved. This file is          *)
(*  distributed under the terms of the GNU Library General Public         *)
(*  License, with an exception, as described in the file LICENSE.         *)
(**************************************************************************)

(** A set of functions that construct representations of OCaml values. *)

(** The string representations produced by these functions are supposed to be
    accepted by the OCaml parser as valid values. *)

(** The signature of this module is compatible with that expected by
    the [camlp4] generator [Camlp4RepresentationGenerator]. *)

(** These functions do {i not} distinguish between mutable and immutable
    values. They do {i not} recognize sharing, and do {i not} incorporate a
    protection against cyclic values. *)

type constructor = string
type type_name = string
type record_field = string
type tag = int

(** A representation of a value is a [PPrint] document. *)
type representation =
    PPrintEngine.document

(** [variant _ dc _ args] is a description of a constructed value whose data
    constructor is [dc] and whose arguments are [args]. The other two
    parameters are presently unused. *)
val variant : type_name -> constructor -> tag -> representation list -> representation

(** [record _ fields] is a description of a record value whose fields are
    [fields]. The other parameter is presently unused. *)
val record : type_name -> (record_field * representation) list -> representation

(** [tuple args] is a description of a tuple value whose components are [args]. *)
val tuple : representation list -> representation

(** [string s] is a representation of the string [s]. *)
val string : string -> representation

(** [int i] is a representation of the integer [i]. *)
val int : int -> representation

(** [int32 i] is a representation of the 32-bit integer [i]. *)
val int32 : int32 -> representation

(** [int64 i] is a representation of the 64-bit integer [i]. *)
val int64 : int64 -> representation

(** [nativeint i] is a representation of the native integer [i]. *)
val nativeint : nativeint -> representation

(** [float f] is a representation of the floating-point number [f]. *)
val float : float -> representation

(** [char c] is a representation of the character [c]. *)
val char : char -> representation

(** [bool b] is a representation of the Boolenan value [b]. *)
val bool : bool -> representation

(** [option f o] is a representation of the option [o], where the
    representation of the element, if present, is computed by the function
    [f]. *)
val option : ('a -> representation) -> 'a option -> representation

(** [list f xs] is a representation of the list [xs], where the
    representation of each element is computed by the function [f].
    If the whole list fits on a single line, then it is printed on
    a single line; otherwise each element is printed on a separate
    line. *)
val list : ('a -> representation) -> 'a list -> representation

(** [flowing_list f xs] is a representation of the list [xs], where the
    representation of each element is computed by the function [f]. As
    many elements are possible are printed on each line. *)
val flowing_list : ('a -> representation) -> 'a list -> representation

(** [array f xs] is a representation of the array [xs], where the
    representation of each element is computed by the function [f].
    If the whole array fits on a single line, then it is printed on
    a single line; otherwise each element is printed on a separate
    line. *)
val array : ('a -> representation) -> 'a array -> representation

(** [flowing_array f xs] is a representation of the array [xs], where
    the representation of each element is computed by the function [f].
    As many elements are possible are printed on each line. *)
val flowing_array : ('a -> representation) -> 'a array -> representation

(** [ref r] is a representation of the reference [r], where the
    representation of the content is computed by the function [f]. *)
val ref : ('a -> representation) -> 'a ref -> representation

(** [unknown t _] is a representation of an unknown value of type [t]. *)
val unknown : type_name -> 'a -> representation
