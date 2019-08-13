open Std_internal

(** A computation is the type of an operation that can be applied to various different
    kind of types.  It is expressed as a type with one parameter:

    type 'a computation

    Examples of computation:

    type sexp_of_t = ('a -> Sexp.t) computation

    The term [generic] is used to refer to a specific implementation of a computation
    whose concrete implementation is programmed using the type representation of values.

    For example, when one uses [with sexp] as a way to implement the [sexp_of_t]
    computation, the technique used is code generation at compile time.  Another approach
    is to define a generic function [sexp_of_t] that inspects the representation of the
    type at runtime.

    This module offers an abstraction over type rep in order to implement generics in a
    efficient way.

    Provided from a user enough pieces of implementation regarding a particular
    computation, this module returns essentially the following function:

    (** main function : get the computation from the typerep *)
    val of_typerep : 'a Typerep.t -> [ `generic of 'a computation ]

    that allows one to get the generic computation operating on a given type ['a].
*)

module Variant_and_record_intf : (module type of Variant_and_record_intf)

module Helper (A : Variant_and_record_intf.S) (B : Variant_and_record_intf.S) : sig
  type map = { map : 'a. 'a A.t -> 'a B.t }
  val map_variant : map -> 'a A.Variant.t -> 'a B.Variant.t
  val map_record : map -> 'a A.Record.t -> 'a B.Record.t
end

module type Named = sig
  type 'a computation
  module Context : sig
    (**
       Mutable context used to memorize some info during the traversal of a typerep.
       A new context is created before starting to enter the toplevel of a typerep.
       Then it is passed to all [init] calls that happen during the traversal of it.
       The user of the generic functor is free to stuff there whatever context needs to be
       available while creating a new value of type ['a Named.t]
    *)
    type t
    val create : unit -> t
  end

  (**
     Work in progress representation of a computation. This is mostly used to handle
     recursive types. While building a computation on a recursive type, one needs to have
     some computation available for the location where the type appears recursively.
     [init] will be called once on each new type_name met during the traversal of a type.
     Each time the same type is encountered again, [get_wip_computation] will be called.
     At the end of the traversal of that particular type, [set_final_computation] will be
     called, offering as a way to "close" the wip representation.  ['a t] can be mutable
     (and is likely to be in practice).

     After a [set_final_computation] is performed and return a final computation C for a
     type_name, C will be memoized and returned for each further occurrences of the same
     type_name inside the typerep, going further on.
  *)
  type 'a t
  val init : Context.t -> 'a Typename.t -> 'a t
  val get_wip_computation : 'a t -> 'a computation
  val set_final_computation : 'a t -> 'a computation -> 'a computation

  (**
     It might be interesting to inline some computation for a few typerep if they appear
     several times within a typerep. This parameters will allow one to
     tweak the sharing between multiple occurences of the same typename.
     [share = true] means no inlining.

     Note that not sharing recursive types will lead the [of_typerep] function to loop
     forever. Be careful when setting this.

     An example where it is not suitable to share everything for example is
     typestruct. The typestruct of an int is a simple constructor called [Int], naming it
     once and using the name to refere to it later within the typestruct does not lead to
     a shorter typestruct, and is in fact less readable. The benefit of the sharing
     depends on the computation, its memory and building costs.
  *)
  val share : _ Typerep.t -> bool
end

module type Computation = sig
  type 'a t

  include Variant_and_record_intf.S with type 'a t := 'a t

  val int : int t
  val int32 : int32 t
  val int64 : int64 t
  val nativeint : nativeint t
  val char : char t
  val float : float t
  val string : string t
  val bytes : bytes t
  val bool : bool t
  val unit : unit t
  val option : 'a t -> 'a option t
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
  val lazy_t : 'a t -> 'a lazy_t t
  val ref_ : 'a t -> 'a ref t
  val function_ : 'a t -> 'b t -> ('a -> 'b) t
  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  val record : 'a Record.t -> 'a t
  val variant : 'a Variant.t -> 'a t

  module Named : Named with type 'a computation := 'a t
end

(**
   Not all computations are arrow types. For example:

     ['a computation = Type_hash.t]

   However, arrow types computation such as [of_sexp], [sexp_of], [json_of], etc.  are
   such a standard case that is seems reasonable to share this extra layer of functor for
   it to build the [Named] module.
*)
module Make_named_for_closure (X : sig
  type 'a input
  type 'a output
  type 'a t = 'a input -> 'a output
end) : Named with type 'a computation := 'a X.t

module Ident : sig
  (**
     Runtime identifier for a generic computation. This is essentially a string whose
     purpose is to give reasonable error messages in case the dependency requirements for
     a generic are not met at runtime.

     The field called [required] is needed in order to build a generic computation module.
     It is used to establish a set up that would explicitly list all the computation that
     are required by an other computation to work.

     Generic computations are a way to build dynamically some operations on types. It is
     possible to build computation on top of each other. This ident type will be the key
     to talk about other computations at the point of setting up the dependencies.
  *)
  type t
end

module type S = sig

  type 'a t
  type 'a computation = 'a t

  val ident : Ident.t

  (** generic_ident * typename or info *)
  exception Not_implemented of string * string

  (** register mechanism to customize the behavior of this generic *)
  include Type_generic_intf.S with type 'a t := 'a t

  (**
     Extending an existing generic for a particular type name

     The use of first class modules there is essentially because we cannot talk about a
     variable of kind * -> k
     val register1 : 'a 't Typerep.t -> ('a computation -> 'a 't computation) -> unit
     ...
  *)
  val register0 : (module S) -> unit
  val register1 : (module S1) -> unit
  val register2 : (module S2) -> unit
  val register3 : (module S3) -> unit
  val register4 : (module S4) -> unit
  val register5 : (module S5) -> unit

  (**
     special less scary type when the type has no parameters. this is equivalent as
     using register0
  *)
  val register : 'a Typerep.t -> 'a computation -> unit

  (** main function : compute the generic computation from the typerep *)
  val of_typerep : 'a Typerep.t -> [ `generic of 'a computation ]

  (** exported to build a computation on top of a previous one *)
  module Computation : Computation with type 'a t = 'a t
end

(**
   The [name] is used for debug information only in case of Broken_dependency.
   The [required] is to handle dependencies between generics at runtime.
   Example:
   if [X] is the module given to build a generic computation [G] that depends on three
   other computation [A,B,C] then X.required shall be [ A.ident ; B.ident ; C.ident ]
*)
module Make (X : sig
  type 'a t
  val name : string
  val required : Ident.t list
  include Computation
  with type 'a t := 'a t
end) : S with type 'a t = 'a X.t
