(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** The core ctypes module.

    The main points of interest are the set of functions for describing C
    types (see {!types}) and the set of functions for accessing C values (see
    {!values}).  The {!Foreign.foreign} function uses C type descriptions
    to bind external C values.
*)

(** {4:pointer_types Pointer types} *)

type ('a, 'b) pointer = ('a, 'b) Ctypes_static.pointer
(** The type of pointer values. A value of type [('a, [`C]) pointer] contains
    a C-compatible pointer, and a value of type [('a, [`OCaml]) pointer]
    contains a pointer to a value that can be moved by OCaml runtime. *)

(** {4 C-compatible pointers} *)

type 'a ptr = ('a, [`C]) pointer
(** The type of C-compatible pointer values.  A value of type [t ptr] can be
    used to read and write values of type [t] at particular addresses. *)

type 'a ocaml = 'a Ctypes_static.ocaml
(** The type of pointer values pointing directly into OCaml values.
    {b Pointers of this type should never be captured by external code}.
    In particular, functions accepting ['a ocaml] pointers must not invoke
    any OCaml code. *)

(** {4 C array types} *)

type 'a carray = 'a Ctypes_static.carray
(** The type of C array values.  A value of type [t carray] can be used to read
    and write array objects in C-managed storage. *)

(** {4 Bigarray types} *)

type 'a bigarray_class = 'a Ctypes_static.bigarray_class
(** The type of Bigarray classes.  There are four instances, one for each of
    the Bigarray submodules. *)

val genarray :
  < element: 'a;
    layout: 'l;
    ba_repr: 'b;
    bigarray: ('a, 'b, 'l) Bigarray.Genarray.t;
    carray: 'a carray;
    dims: int array > bigarray_class
(** The class of {!Bigarray.Genarray.t} values *)

val array1 :
  < element: 'a;
    layout: 'l;
    ba_repr: 'b;
    bigarray: ('a, 'b, 'l) Bigarray.Array1.t;
    carray: 'a carray;
    dims: int > bigarray_class
(** The class of {!Bigarray.Array1.t} values *)

val array2 :
  < element: 'a;
    layout: 'l;
    ba_repr: 'b;
    bigarray: ('a, 'b, 'l) Bigarray.Array2.t;
    carray: 'a carray carray;
    dims: int * int > bigarray_class
(** The class of {!Bigarray.Array2.t} values *)

val array3 :
  < element: 'a;
    layout: 'l;
    ba_repr: 'b;
    bigarray: ('a, 'b, 'l) Bigarray.Array3.t;
    carray: 'a carray carray carray;
    dims: int * int * int > bigarray_class
(** The class of {!Bigarray.Array3.t} values *)

(** {3 Struct and union types} *)

type ('a, 'kind) structured = ('a, 'kind) Ctypes_static.structured
(** The base type of values representing C struct and union types.  The
    ['kind] parameter is a polymorphic variant type indicating whether the type
    represents a struct ([`Struct]) or a union ([`Union]). *)

type 'a structure = ('a, [`Struct]) structured
(** The type of values representing C struct types. *)

type 'a union = ('a, [`Union]) structured
(** The type of values representing C union types. *)

type ('a, 't) field = ('a, 't) Ctypes_static.field
(** The type of values representing C struct or union members (called "fields"
    here).  A value of type [(a, s) field] represents a field of type [a] in a
    struct or union of type [s]. *)

type 'a abstract = 'a Ctypes_static.abstract
(** The type of abstract values.  The purpose of the [abstract] type is to
    represent values whose type varies from platform to platform.

    For example, the type [pthread_t] is a pointer on some platforms, an
    integer on other platforms, and a struct on a third set of platforms.  One
    way to deal with this kind of situation is to have
    possibly-platform-specific code which interrogates the C type in some way
    to help determine an appropriate representation.  Another way is to use
    [abstract], leaving the representation opaque.

    (Note, however, that although [pthread_t] is a convenient example, since
    the type used to implement it varies significantly across platforms, it's
    not actually a good match for [abstract], since values of type [pthread_t]
    are passed and returned by value.) *)

include Ctypes_types.TYPE
 with type 'a typ = 'a Ctypes_static.typ
  and type ('a, 's) field := ('a, 's) field

(** {3 Operations on types} *)

val sizeof : 'a typ -> int
(** [sizeof t] computes the size in bytes of the type [t].  The exception
    {!IncompleteType} is raised if [t] is incomplete. *)

val alignment : 'a typ -> int
(** [alignment t] computes the alignment requirements of the type [t].  The
    exception {!IncompleteType} is raised if [t] is incomplete. *)

val format_typ : ?name:string -> Format.formatter -> 'a typ -> unit
(** Pretty-print a C representation of the type to the specified formatter. *)

val format_fn : ?name:string -> Format.formatter -> 'a fn -> unit
(** Pretty-print a C representation of the function type to the specified
    formatter. *)

val string_of_typ : ?name:string -> 'a typ -> string
(** Return a C representation of the type. *)

val string_of_fn : ?name:string -> 'a fn -> string
(** Return a C representation of the function type. *)

(** {2:values Values representing C values} *)

val format : 'a typ -> Format.formatter -> 'a -> unit
(** Pretty-print a representation of the C value to the specified formatter. *)

val string_of : 'a typ -> 'a -> string
(** Return a string representation of the C value. *)

(** {3 Pointer values} *)

val null : unit ptr
(** A null pointer. *)

val (!@) : 'a ptr -> 'a
(** [!@ p] dereferences the pointer [p].  If the reference type is a scalar
    type then dereferencing constructs a new value.  If the reference type is
    an aggregate type then dereferencing returns a value that references the
    memory pointed to by [p]. *)

val (<-@) : 'a ptr -> 'a -> unit
(** [p <-@ v] writes the value [v] to the address [p]. *)

val (+@) : ('a, 'b) pointer -> int -> ('a, 'b) pointer
(** If [p] is a pointer to an array element then [p +@ n] computes the
    address of the [n]th next element. *)

val (-@) : ('a, 'b) pointer -> int -> ('a, 'b) pointer
(** If [p] is a pointer to an array element then [p -@ n] computes the address
    of the nth previous element. *)

val ptr_diff : ('a, 'b) pointer -> ('a, 'b) pointer -> int
(** [ptr_diff p q] computes [q - p].  As in C, both [p] and [q] must point
    into the same array, and the result value is the difference of the
    subscripts of the two array elements. *)

val from_voidp : 'a typ -> unit ptr -> 'a ptr
(** Conversion from [void *]. *)

val to_voidp : _ ptr -> unit ptr
(** Conversion to [void *]. *)

val allocate : ?finalise:('a ptr -> unit) -> 'a typ -> 'a -> 'a ptr
(** [allocate t v] allocates a fresh value of type [t], initialises it
    with [v] and returns its address.  The argument [?finalise], if
    present, will be called just before the memory is freed.  The value
    will be automatically freed after no references to the pointer
    remain within the calling OCaml program. *)

val allocate_n : ?finalise:('a ptr -> unit) -> 'a typ -> count:int -> 'a ptr
(** [allocate_n t ~count:n] allocates a fresh array with element type
    [t] and length [n], and returns its address.  The argument
    [?finalise], if present, will be called just before the memory is
    freed.  The array will be automatically freed after no references
    to the pointer remain within the calling OCaml program.  The
    memory is allocated with libc's [calloc] and is guaranteed to be
    zero-filled.  *)

val ptr_compare : 'a ptr -> 'a ptr -> int
(** If [p] and [q] are pointers to elements [i] and [j] of the same array then
    [ptr_compare p q] compares the indexes of the elements.  The result is
    negative if [i] is less than [j], positive if [i] is greater than [j], and
    zero if [i] and [j] are equal. *)

val is_null : 'a ptr -> bool
(** [is_null p] is true when [p] is a null pointer. *)

val reference_type : 'a ptr -> 'a typ
(** Retrieve the reference type of a pointer. *)

val ptr_of_raw_address : nativeint -> unit ptr
(** Convert the numeric representation of an address to a pointer *)

val funptr_of_raw_address : nativeint -> (unit -> unit) Ctypes_static.static_funptr
(** Convert the numeric representation of an address to a function pointer *)

val raw_address_of_ptr : unit ptr -> nativeint
(** [raw_address_of_ptr p] returns the numeric representation of p.

    Note that the return value remains valid only as long as the pointed-to
    object is alive.  If [p] is a managed object (e.g. a value returned by
    {!make}) then unless the caller retains a reference to [p], the object may
    be collected, invalidating the returned address. *)

val string_from_ptr : char ptr -> length:int -> string
(** [string_from_ptr p ~length] creates a string initialized with the [length]
    characters at address [p].

    Raise [Invalid_argument "Ctypes.string_from_ptr"] if [length] is
    negative. *)

val ocaml_string_start : string -> string ocaml
(** [ocaml_string_start s] allows to pass a pointer to the contents of an OCaml
    string directly to a C function. *)

val ocaml_bytes_start : bytes -> bytes ocaml
(** [ocaml_bytes_start s] allows to pass a pointer to the contents of an OCaml
    byte array directly to a C function. *)

(** {3 Array values} *)

(** {4 C array values} *)

module CArray :
sig
  type 'a t = 'a carray

  val get : 'a t -> int -> 'a
  (** [get a n] returns the [n]th element of the zero-indexed array [a].  The
      semantics for non-scalar types are non-copying, as for {!(!@)}.

      If you rebind the [CArray] module to [Array] then you can also use the
      syntax [a.(n)] instead of [Array.get a n].

      Raise [Invalid_argument "index out of bounds"] if [n] is outside of the
      range [0] to [(CArray.length a - 1)]. *)

  val set : 'a t -> int -> 'a -> unit
  (** [set a n v] overwrites the [n]th element of the zero-indexed array [a]
      with [v].

      If you rebind the [CArray] module to [Array] then you can also use the
      [a.(n) <- v] syntax instead of [Array.set a n v].

      Raise [Invalid_argument "index out of bounds"] if [n] is outside of the
      range [0] to [(CArray.length a - 1)]. *)

  val unsafe_get : 'a t -> int -> 'a
  (** [unsafe_get a n] behaves like [get a n] except that the check that [n]
      between [0] and [(CArray.length a - 1)] is not performed. *)

  val unsafe_set : 'a t -> int -> 'a -> unit
  (** [unsafe_set a n v] behaves like [set a n v] except that the check that
      [n] between [0] and [(CArray.length a - 1)] is not performed. *)

  val of_string : string -> char t
  (** [of_string s] builds an array of the same length as [s], and writes
      the elements of [s] to the corresponding elements of the array. *)

  val of_list : 'a typ -> 'a list -> 'a t
  (** [of_list t l] builds an array of type [t] of the same length as [l], and
      writes the elements of [l] to the corresponding elements of the array. *)

  val to_list : 'a t -> 'a list
  (** [to_list a] builds a list of the same length as [a] such that each
      element of the list is the result of reading the corresponding element of
      [a]. *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f a] is analogous to [Array.iter f a]: it applies [f] in turn to
      all the elements of [a]. *)

  val map : 'b typ -> ('a -> 'b) -> 'a t -> 'b t
  (** [map t f a] is analogous to [Array.map f a]: it creates a new array with
      element type [t] whose elements are obtained by applying [f] to the
      elements of [a]. *)

  val mapi : 'b typ -> (int -> 'a -> 'b) -> 'a t -> 'b t
  (** [mapi] behaves like {!Array.mapi}, except that it also passes the
      index of each element as the first argument to [f] and the element
      itself as the second argument. *)

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [CArray.fold_left (@) x a] computes 
         [(((x @ a.(0)) @ a.(1)) ...) @ a.(n-1)]
       where [n] is the length of the array [a]. *)

  val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
  (** [CArray.fold_right f a x] computes
         [a.(0) @ (a.(1) @ ( ... (a.(n-1) @ x) ...))]
       where [n] is the length of the array [a]. *)

  val length : 'a t -> int
  (** Return the number of elements of the given array. *)

  val start : 'a t -> 'a ptr
  (** Return the address of the first element of the given array. *)

  val from_ptr : 'a ptr -> int -> 'a t
  (** [from_ptr p n] creates an [n]-length array reference to the memory at
      address [p]. *)

  val make : ?finalise:('a t -> unit) -> 'a typ -> ?initial:'a -> int -> 'a t
  (** [make t n] creates an [n]-length array of type [t].  If the optional
      argument [?initial] is supplied, it indicates a value that should be
      used to initialise every element of the array.  The argument [?finalise],
      if present, will be called just before the memory is freed. *)

  val copy : 'a t -> 'a t
  (** [copy a] creates a fresh array with the same elements as [a]. *)

  val sub : 'a t -> pos:int -> length:int -> 'a t
  (** [sub a ~pos ~length] creates a fresh array of length [length] containing
      the elements [a.(pos)] to [a.(pos + length - 1)] of [a].

      Raise [Invalid_argument "CArray.sub"] if [pos] and [length] do not
      designate a valid subarray of [a]. *)

  val element_type : 'a t -> 'a typ
  (** Retrieve the element type of an array. *)
end
(** Operations on C arrays. *)

(** {4 Bigarray values} *)

val bigarray_start : < element: 'a;
                       layout: 'l;
                       ba_repr: _;
                       bigarray: 'b;
                       carray: _;
                       dims: _ > bigarray_class -> 'b -> 'a ptr
(** Return the address of the first element of the given Bigarray value. *)

val bigarray_of_ptr : < element: 'a;
                        layout: Bigarray.c_layout;
                        ba_repr: 'f;
                        bigarray: 'b;
                        carray: _;
                        dims: 'i > bigarray_class ->
    'i -> ('a, 'f) Bigarray.kind -> 'a ptr -> 'b
(** [bigarray_of_ptr c dims k p] converts the C pointer [p] to a C-layout
    bigarray value.  No copy is made; the bigarray references the memory
    pointed to by [p]. *)

val fortran_bigarray_of_ptr : < element: 'a;
                                layout: Bigarray.fortran_layout;
                                ba_repr: 'f;
                                bigarray: 'b;
                                carray: _;
                                dims: 'i > bigarray_class ->
    'i -> ('a, 'f) Bigarray.kind -> 'a ptr -> 'b
(** [fortran_bigarray_of_ptr c dims k p] converts the C pointer [p] to a
    Fortran-layout bigarray value.  No copy is made; the bigarray references
    the memory pointed to by [p]. *)

val array_of_bigarray : < element: _;
                          layout: Bigarray.c_layout;
                          ba_repr: _;
                          bigarray: 'b;
                          carray: 'c;
                          dims: _ > bigarray_class -> 'b -> 'c
(** [array_of_bigarray c b] converts the bigarray value [b] to a value of type
    {!CArray.t}.  No copy is made; the result occupies the same memory as
    [b]. *)

(** Convert a Bigarray value to a C array. *)

val bigarray_of_array : < element: 'a;
                          layout: Bigarray.c_layout;
                          ba_repr: 'f;
                          bigarray: 'b;
                          carray: 'c carray;
                          dims: 'i > bigarray_class ->
    ('a, 'f) Bigarray.kind -> 'c carray -> 'b
(** [bigarray_of_array c k a] converts the {!CArray.t} value [a] to a
    C-layout bigarray value.  No copy is made; the result occupies the
    same memory as [a]. *)

(** {3 Struct and union values} *)

val make : ?finalise:('s -> unit) -> ((_, _) structured as 's) typ -> 's
(** Allocate a fresh, uninitialised structure or union value.  The argument
    [?finalise], if present, will be called just before the underlying memory is
    freed. *)

val setf : ((_, _) structured as 's) -> ('a, 's) field -> 'a -> unit
(** [setf s f v] overwrites the value of the field [f] in the structure or
    union [s] with [v]. *)

val getf : ((_, _) structured as 's) -> ('a, 's) field -> 'a
(** [getf s f] retrieves the value of the field [f] in the structure or union
    [s].  The semantics for non-scalar types are non-copying, as for
    {!(!@)}.*)

val (@.) : ((_, _) structured as 's) -> ('a, 's) field -> 'a ptr
(** [s @. f] computes the address of the field [f] in the structure or union
    value [s]. *)

val (|->) : ((_, _) structured as 's) ptr -> ('a, 's) field -> 'a ptr
(** [p |-> f] computes the address of the field [f] in the structure or union
    value pointed to by [p]. *)

val offsetof : (_, _ structure) field -> int
(** [offsetof f] returns the offset, in bytes, of the field [f] from the
    beginning of the associated struct type. *)

val field_type : ('a, _) field -> 'a typ
(** [field_type f] returns the type of the field [f]. *)

val field_name : (_, _) field -> string
(** [field_name f] returns the name of the field [f]. *)

val addr : ((_, _) structured as 's) -> 's ptr
(** [addr s] returns the address of the structure or union [s]. *)

(** {3 Coercions} *)

val coerce : 'a typ -> 'b typ -> 'a -> 'b
(** [coerce t1 t2] returns a coercion function between the types represented
    by [t1] and [t2].  If [t1] cannot be coerced to [t2], [coerce] raises
    {!Uncoercible}.

    The following coercions are currently supported:

     - All function and object pointer types are intercoercible.
     - Any type may be coerced to {!void}
     - There is a coercion between a {!view} and another type [t] (in either
       direction) if there is a coercion between the representation type
       underlying the view and [t].
     - Coercion is transitive: if [t1] is coercible to [t2] and [t2] is
       coercible to [t3], then [t1] is directly coercible to [t3].

    The set of supported coercions is subject to change.  Future versions of
    ctypes may both add new types of coercion and restrict the existing
    coercions. *)

val coerce_fn : 'a fn -> 'b fn -> 'a -> 'b
(** [coerce_fn f1 f2] returns a coercion function between the function
    types represented by [f1] and [f2].  If [f1] cannot be coerced to
    [f2], [coerce_fn] raises {!Uncoercible}.

    A function type [f1] may be coerced to another function type [f2]
    if all of the following hold:

      - the C types described by [f1] and [f2] have the same arity

      - each argument of [f2] may be coerced to the corresponding
        argument of [f1]

      - the return type of [f1] may be coerced to the return type of [f2]

    The set of supported coercions is subject to change.  Future versions of
    ctypes may both add new types of coercion and restrict the existing
    coercions. *)


(** {2 binding interfaces}

    Foreign function binding interface.

    The {!Foreign} and {!Cstubs} modules provide concrete implementations. *)
module type FOREIGN =
sig
  type 'a fn
  type 'a return
  val (@->) : 'a typ -> 'b fn -> ('a -> 'b) fn
  val returning : 'a typ -> 'a return fn

  type 'a result
  val foreign : string -> ('a -> 'b) fn -> ('a -> 'b) result
  val foreign_value : string -> 'a typ -> 'a ptr result
end

(** Foreign types binding interface.

    The {!Cstubs} module builds concrete implementations. *)
module type TYPE =
sig
  include Ctypes_types.TYPE

  type 'a const
  val constant : string -> 'a typ -> 'a const
  (** [constant name typ] retrieves the value of the compile-time constant
      [name] of type [typ].  It can be used to retrieve enum constants,
      #defined values and other integer constant expressions.

      The type [typ] must be either an integer type such as [bool], [char],
      [int], [uint8], etc., or a view (or perhaps multiple views) where the
      underlying type is an integer type.

      When the value of the constant cannot be represented in the type there
      will typically be a diagnostic from either the C compiler or the OCaml
      compiler.  For example, gcc will say

         warning: overflow in implicit constant conversion *)

  val enum : string -> ?typedef:bool ->
    ?unexpected:(int64 -> 'a) -> ('a * int64 const) list -> 'a typ
  (** [enum name ?unexpected alist] builds a type representation for the
      enum named [name].  The size and alignment are retrieved so that the
      resulting type can be used everywhere an integer type can be used: as
      an array element or struct member, as an argument or return value,
      etc.

      The value [alist] is an association list of OCaml values and values
      retrieved by the [constant] function.  For example, to expose the enum

        enum letters \{ A, B, C = 10, D \}; 

      you might first retrieve the values of the enumeration constants:

      {[
        let a = constant "A" int64_t
        and b = constant "B" int64_t
        and c = constant "C" int64_t
        and d = constant "D" int64_t
      ]}

      and then build the enumeration type

      {[
        let letters = enum "letters" [
           `A, a;
           `B, b;
           `C, c;
           `D, d;
        ] ~unexpected:(fun i -> `E i)
      ]}

      The [unexpected] function specifies the value to return in the case
      that some unexpected value is encountered -- for example, if a
      function with the return type 'enum letters' actually returns the
      value [-1].

      The optional flag [typedef] specifies whether the first argument,
      [name], indicates an tag or an alias.  If [typedef] is [false] (the
      default) then [name] is treated as an enumeration tag:

        [enum letters { ... }]

      If [typedef] is [true] then [name] is instead treated as an alias:

        [typedef enum { ... } letters] *)
end

(** {2:roots Registration of OCaml values as roots} *)
module Root :
sig
  val create : 'a -> unit ptr
  (** [create v] allocates storage for the address of the OCaml value [v],
      registers the storage as a root, and returns its address. *)

  val get : unit ptr -> 'a
  (** [get p] retrieves the OCaml value whose address is stored at [p]. *)

  val set : unit ptr -> 'a -> unit
  (** [set p v] updates the OCaml value stored as a root at [p]. *)

  val release : unit ptr -> unit
  (** [release p] unregsiters the root [p]. *)
end

(** {2 Exceptions} *)

exception Unsupported of string
(** An attempt was made to use a feature not currently supported by ctypes.
    In practice this refers to attempts to use an union, array or abstract
    type as an argument or return type of a function. *)

exception ModifyingSealedType of string
(** An attempt was made to modify a sealed struct or union type
    description.  *)

exception IncompleteType
(** An attempt was made to compute the size or alignment of an incomplete
    type.

    The incomplete types are struct and union types that have not been sealed,
    and the void type.

    It is not permitted to compute the size or alignment requirements of an
    incomplete type, to use it as a struct or union member, to read or write a
    value of the type through a pointer or to use it as the referenced type in
    pointer arithmetic.  Additionally, incomplete struct and union types
    cannot be used as argument or return types.
*)

type uncoercible_info
exception Uncoercible of uncoercible_info
(** An attempt was made to coerce between uncoercible types.  *)
