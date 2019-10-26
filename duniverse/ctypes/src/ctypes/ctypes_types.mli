(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Signed
open Unsigned


(** Abstract interface to C object type descriptions *)
module type TYPE =
sig
  (** {2:types Values representing C types} *)

  type 'a typ
  (** The type of values representing C types.  There are two types associated
      with each [typ] value: the C type used to store and pass values, and the
      corresponding OCaml type.  The type parameter indicates the OCaml type, so a
      value of type [t typ] is used to read and write OCaml values of type [t].
      There are various uses of [typ] values, including

      - constructing function types for binding native functions using
      {!Foreign.foreign}

      - constructing pointers for reading and writing locations in C-managed
      storage using {!ptr}

      - describing the fields of structured types built with {!structure} and
      {!union}.
  *)

  (** {3 The void type} *)

  val void  : unit typ
  (** Value representing the C void type.  Void values appear in OCaml as the
      unit type, so using void in an argument or result type specification
      produces a function which accepts or returns unit.

      Dereferencing a pointer to void is an error, as in C, and will raise
      {!IncompleteType}.
  *)

  (** {3 Scalar types}

      The scalar types consist of the {!arithmetic_types} and the {!pointer_types}.
  *)

  (** {4:arithmetic_types Arithmetic types}

      The arithmetic types consist of the signed and unsigned integer types
      (including character types) and the floating types.  There are values
      representing both exact-width integer types (of 8, 16, 32 and 64 bits) and
      types whose size depend on the platform (signed and unsigned short, int, long,
      long long).

  *)

  val char : char typ
  (** Value representing the C type [char]. *)

  (** {5 Signed integer types} *)

  val schar : int typ
  (** Value representing the C type [signed char]. *)

  val short : int typ
  (** Value representing the C type ([signed]) [short]. *)

  val int   : int typ
  (** Value representing the C type ([signed]) [int]. *)

  val long  : long typ
  (** Value representing the C type ([signed]) [long]. *)

  val llong  : llong typ
  (** Value representing the C type ([signed]) [long long]. *)

  val nativeint : nativeint typ
  (** Value representing the C type ([signed]) [int]. *)

  val int8_t : int typ
  (** Value representing an 8-bit signed integer C type. *)

  val int16_t : int typ
  (** Value representing a 16-bit signed integer C type. *)

  val int32_t : int32 typ
  (** Value representing a 32-bit signed integer C type. *)

  val int64_t : int64 typ
  (** Value representing a 64-bit signed integer C type. *)

  module Intptr : Signed.S
  val intptr_t : Intptr.t typ
  (** Value representing the C type [intptr_t]. *)

  module Ptrdiff : Signed.S
  val ptrdiff_t : Ptrdiff.t typ
  (** Value representing the C type [ptrdiff_t]. *)

  val camlint : int typ
  (** Value representing an integer type with the same storage requirements as
      an OCaml [int]. *)

  (** {5 Unsigned integer types} *)

  val uchar : uchar typ
  (** Value representing the C type [unsigned char]. *)

  val bool : bool typ
  (** Value representing the C type [bool]. *)

  val uint8_t : uint8 typ
  (** Value representing an 8-bit unsigned integer C type. *)

  val uint16_t : uint16 typ
  (** Value representing a 16-bit unsigned integer C type. *)

  val uint32_t : uint32 typ
  (** Value representing a 32-bit unsigned integer C type. *)

  val uint64_t : uint64 typ
  (** Value representing a 64-bit unsigned integer C type. *)

  val size_t : size_t typ
  (** Value representing the C type [size_t], an alias for one of the unsigned
      integer types.  The actual size and alignment requirements for [size_t]
      vary between platforms. *)

  val ushort : ushort typ
  (** Value representing the C type [unsigned short]. *)

  val sint : sint typ
  (** Value representing the C type [int]. *)

  val uint : uint typ
  (** Value representing the C type [unsigned int]. *)

  val ulong : ulong typ
  (** Value representing the C type [unsigned long]. *)

  val ullong : ullong typ
  (** Value representing the C type [unsigned long long]. *)

  module Uintptr : Unsigned.S
  val uintptr_t : Uintptr.t typ
  (** Value representing the C type [uintptr_t]. *)

  (** {5 Floating types} *)

  val float : float typ
  (** Value representing the C single-precision [float] type. *)

  val double : float typ
  (** Value representing the C type [double]. *)

  val ldouble : LDouble.t typ
  (** Value representing the C type [long double]. *)

  (** {5 Complex types} *)

  val complex32 : Complex.t typ
  (** Value representing the C99 single-precision [float complex] type. *)

  val complex64 : Complex.t typ
  (** Value representing the C99 double-precision [double complex] type. *)

  val complexld : ComplexL.t typ
  (** Value representing the C99 long-double-precision [long double complex] type. *)

  (** {4:pointer_types Pointer types} *)

  (** {5 C-compatible pointers} *)

  val ptr : 'a typ -> 'a Ctypes_static.ptr typ
  (** Construct a pointer type from an existing type (called the {i reference
      type}).  *)

  val ptr_opt : 'a typ -> 'a Ctypes_static.ptr option typ
  (** Construct a pointer type from an existing type (called the {i reference
      type}).  This behaves like {!ptr}, except that null pointers appear in OCaml
      as [None]. *)

  val string : string typ
  (** A high-level representation of the string type.

      On the C side this behaves like [char *]; on the OCaml side values read
      and written using {!string} are simply native OCaml strings.

      To avoid problems with the garbage collector, values passed using
      {!string} are copied into immovable C-managed storage before being passed
      to C.

      When the memory is not owned by the C code, -- i.e. when creating or
      initializing a struct in OCaml before passing it to C -- then the
      {!string} view isn't a good choice, because there's no way to manage the
      lifetime of the C copy of the generated OCaml string.
  *)

  val string_opt : string option typ
  (** A high-level representation of the string type.  This behaves like {!string},
      except that null pointers appear in OCaml as [None].
  *)

  (** {5 OCaml pointers} *)

  val ocaml_string : string Ctypes_static.ocaml typ
  (** Value representing the directly mapped storage of an OCaml string. *)

  val ocaml_bytes : Bytes.t Ctypes_static.ocaml typ
  (** Value representing the directly mapped storage of an OCaml byte array. *)

  (** {3 Array types} *)

  (** {4 C array types} *)

  val array : int -> 'a typ -> 'a Ctypes_static.carray typ
  (** Construct a sized array type from a length and an existing type (called
      the {i element type}). *)

  (** {4 Bigarray types} *)

  val bigarray :
    < element: 'a;
      layout: Bigarray.c_layout;
      ba_repr: 'b;
      dims: 'dims;
      bigarray: 'bigarray;
      carray: _ > Ctypes_static.bigarray_class ->
     'dims -> ('a, 'b) Bigarray.kind -> 'bigarray typ
  (** Construct a sized C-layout bigarray type representation from a bigarray
      class, the dimensions, and the {!Bigarray.kind}. *)

  val fortran_bigarray :
    < element: 'a;
      layout: Bigarray.fortran_layout;
      ba_repr: 'b;
      dims: 'dims;
      bigarray: 'bigarray;
      carray: _ > Ctypes_static.bigarray_class ->
     'dims -> ('a, 'b) Bigarray.kind -> 'bigarray typ
  (** Construct a sized Fortran-layout bigarray type representation from a
      bigarray class, the dimensions, and the {!Bigarray.kind}. *)

  val typ_of_bigarray_kind : ('a, 'b) Bigarray.kind -> 'a typ
  (** [typ_of_bigarray_kind k] is the type corresponding to the Bigarray kind
      [k]. *)

  (** {3 Struct and union types} *)

  type ('a, 't) field

  val structure : string -> 's Ctypes_static.structure typ
  (** Construct a new structure type.  The type value returned is incomplete and
      can be updated using {!field} until it is passed to {!seal}, at which point
      the set of fields is fixed.

      The type (['_s structure typ]) of the expression returned by the call
      [structure tag] includes a weak type variable, which can be explicitly
      instantiated to ensure that the OCaml values representing different C
      structure types have incompatible types.  Typical usage is as follows:

      [type tagname]

      [let tagname : tagname structure typ = structure "tagname"]
  *)

  val union : string -> 's Ctypes_static.union typ
  (** Construct a new union type.  This behaves analogously to {!structure};
      fields are added with {!field}. *)

  val field : 't typ -> string -> 'a typ ->
    ('a, (('s, [<`Struct | `Union]) Ctypes_static.structured as 't)) field
  (** [field ty label ty'] adds a field of type [ty'] with label [label] to the
      structure or union type [ty] and returns a field value that can be used to
      read and write the field in structure or union instances (e.g. using
      {!getf} and {!setf}).

      Attempting to add a field to a union type that has been sealed with [seal]
      is an error, and will raise {!ModifyingSealedType}. *)

  val seal : (_, [< `Struct | `Union]) Ctypes_static.structured typ -> unit
  (** [seal t] completes the struct or union type [t] so that no further fields
      can be added.  Struct and union types must be sealed before they can be used
      in a way that involves their size or alignment; see the documentation for
      {!IncompleteType} for further details.  *)

  (** {3 View types} *)

  val view : ?format_typ:((Format.formatter -> unit) -> Format.formatter -> unit) ->
             ?format:(Format.formatter -> 'b -> unit) ->
             read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ
  (** [view ~read:r ~write:w t] creates a C type representation [t'] which
      behaves like [t] except that values read using [t'] are subsequently
      transformed using the function [r] and values written using [t'] are first
      transformed using the function [w].

      For example, given suitable definitions of [string_of_char_ptr] and
      [char_ptr_of_string], the type representation

      [view ~read:string_of_char_ptr ~write:char_ptr_of_string (ptr char)]

      can be used to pass OCaml strings directly to and from bound C functions,
      or to read and write string members in structs and arrays.  (In fact, the
      {!string} type representation is defined in exactly this way.)

      The optional argument [format_typ] is used by the {!Ctypes.format_typ} and
      {!string_of_typ} functions to print the type at the top level and
      elsewhere.  If [format_typ] is not supplied the printer for [t] is used
      instead.

      The optional argument [format] is used by the {!Ctypes.format}
      and {!string_of} functions to print the values. If [format_val]
      is not supplied the printer for [t] is used instead.

  *)

  val typedef : 'a typ -> string -> 'a typ
  (** [typedef t name] creates a C type representation [t'] which
      is equivalent to [t] except its name is printed as [name].

      This is useful when generating C stubs involving "anonymous" types, for
      example: [typedef struct { int f } typedef_name;]
  *)

  (** {3 Abstract types} *)

  val abstract : name:string -> size:int -> alignment:int -> 'a Ctypes_static.abstract typ
  (** Create an abstract type specification from the size and alignment
      requirements for the type. *)

  (** {3 Injection of concrete types} *)

  val lift_typ : 'a Ctypes_static.typ -> 'a typ
  (** [lift_typ t] turns a concrete type representation into an abstract type
      representation.

      For example, retrieving struct layout from C involves working with an
      abstract representation of types which do not support operations such as
      [sizeof].  The [lift_typ] function makes it possible to use concrete
      type representations wherever such abstract type representations are
      needed. *)

  (** {3 Function types} *)
  (** Abstract interface to C function type descriptions *)

  type 'a fn = 'a Ctypes_static.fn
  (** The type of values representing C function types.  A value of type [t fn]
      can be used to bind to C functions and to describe type of OCaml functions
      passed to C. *)

  val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn
  (** Construct a function type from a type and an existing function type.  This
      corresponds to prepending a parameter to a C function parameter list.  For
      example,

      [int @-> ptr void @-> returning float]

      describes a function type that accepts two arguments -- an integer and a
      pointer to void -- and returns a float.
  *)

  val returning : 'a typ -> 'a fn
  (** Give the return type of a C function.  Note that [returning] is intended
      to be used together with {!(@->)}; see the documentation for {!(@->)} for an
      example. *)

  (** {3 Function pointer types} *)
  type 'a static_funptr = 'a Ctypes_static.static_funptr
  (** The type of values representing C function pointer types. *)

  val static_funptr : 'a fn -> 'a Ctypes_static.static_funptr typ
  (** Construct a function pointer type from an existing function type
      (called the {i reference type}).  *)
end
