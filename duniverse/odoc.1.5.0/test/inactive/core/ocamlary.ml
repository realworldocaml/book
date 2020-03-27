(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *                    Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** An interface with all of the module system features *)

module type Empty = sig type t end

(** An ambiguous, misnamed module type *)
module type MissingComment = sig type t end

(** A plain, empty module. *)
module Empty = struct end

(** A plain module alias. *)
module EmptyAlias = Empty

(** A plain, empty module signature. *)
module type EmptySig = sig end

(** A plain, empty module signature alias. *)
module type EmptySigAlias = EmptySig

(** A plain module of a signature. *)
module ModuleWithSignature = struct end

(** A plain module with an alias signature. *)
module ModuleWithSignatureAlias = struct end

(** has type "one" *)
module One = struct type one end

(** There's a module in this signature. *)
module type SigForMod = sig
  module Inner : sig
    module type Empty = sig end
  end
end

module type SuperSig = sig
  module type SubSigA = sig
    (** {3:SubSig A Labeled Section Header Inside of a Signature} *)

    type t

    module SubSigAMod : sig
      type sub_sig_a_mod
    end
  end
  module type SubSigB = sig
    (** {3:SubSig Another Labeled Section Header Inside of a Signature} *)

    type t
  end
  module type EmptySig = sig
    type not_actually_empty
  end
  module type One = sig type two end
  module type SuperSig = sig end
end

(** {!Buffer.t} *)
module Buffer = struct
  let f _ = ()
end

(** Unary exception constructor *)
exception Kaboom of unit

(** Binary exception constructor *)
exception Kablam of unit * unit

(** Unary exception constructor over binary tuple *)
exception Kapow  of (unit * unit)

(** {!EmptySig} is general but {!module:EmptySig} is a module and
    {!exception:EmptySig} is this exception. *)
exception EmptySig

(** {!exception:EmptySigAlias} is this exception. *)
exception EmptySigAlias

(** {!a_function} is general but {!type:a_function} is this type and
    {!val:a_function} is the value below. *)
type ('a,'b) a_function = 'a -> 'b

(**
   @param x the [x] coordinate
   @return the [y] coordinate
*)
let a_function ~x = x

let fun_fun_fun _int_fun = (fun () -> ())

let fun_maybe ?yes:_ () = 0

(** @raise Not_found That's all it does *)
let not_found () = raise Not_found

(** @see < http://ocaml.org/ > The OCaml Web site *)
let ocaml_org = "http://ocaml.org/"

(** @see 'some_file' The file called [some_file] *)
let some_file = "some_file"

(** @see "some_doc" The document called [some_doc] *)
let some_doc = "some_doc"

(**
   This value was introduced in the Mesozoic era.
   @since mesozoic
*)
let since_mesozoic = ()

(**
   This value has had changes in 1.0.0, 1.1.0, and 1.2.0.
   @before 1.0.0 before 1.0.0
   @before 1.1.0 before 1.1.0
   @version 1.2.0
*)
let changing = ()

(** This value has a custom tag [foo].
    @foo the body of the custom [foo] tag
*)
let with_foo = ()

(** {3 Some Operators } *)

let ( ~- ) = ()
let ( ! )  = ()
let ( @ )  = ()
let ( $ )  = ()
let ( % )  = ()
let ( ^ )  = ()
let ( & )  = ()
let ( * )  = ()
let ( - )  = ()
let ( + )  = ()
let ( < )  = ()
let ( > )  = ()
let ( -? ) = ()
let ( / )  = ()
let ( -| ) = ()
let ( := ) = ()
let ( = )  = ()

let (land) = ()

(** {3 Advanced Module Stuff} *)

(** This comment is for [CollectionModule]. *)
module CollectionModule = struct
  (** This comment is for [collection]. *)
  type collection
  type element

  (** This comment is for [InnerModuleA]. *)
  module InnerModuleA = struct
    (** This comment is for [t]. *)
    type t = collection

    (** This comment is for [InnerModuleA']. *)
    module InnerModuleA' = struct
      (** This comment is for [t]. *)
      type t = (unit,unit) a_function
    end

    (** This comment is for [InnerModuleTypeA']. *)
    module type InnerModuleTypeA' = sig
      (** This comment is for [t]. *)
      type t = InnerModuleA'.t
    end
  end

  (** This comment is for [InnerModuleTypeA]. *)
  module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
end

(** module type of *)
module type COLLECTION = module type of CollectionModule

module Recollection(C : COLLECTION) :
  COLLECTION with type collection = C.element list and type element = C.collection = struct
  type collection = C.element list
  type element = C.collection

  (** This comment is for [InnerModuleA]. *)
  module InnerModuleA = struct
    (** This comment is for [t]. *)
    type t = collection

    (** This comment is for [InnerModuleA']. *)
    module InnerModuleA' = struct
      (** This comment is for [t]. *)
      type t = (unit,unit) a_function
    end

    (** This comment is for [InnerModuleTypeA']. *)
    module type InnerModuleTypeA' = sig
      (** This comment is for [t]. *)
      type t = InnerModuleA'.t
    end
  end

  (** This comment is for [InnerModuleTypeA]. *)
  module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
end

module type MMM = sig module C : COLLECTION end

module type RECOLLECTION = MMM with module C = Recollection(CollectionModule)

module type RecollectionModule = sig
  include module type of Recollection(CollectionModule)
end

module type A = sig
  type t
  module Q : COLLECTION
end

module type B = sig
  type t
  module Q : COLLECTION
end

module type C = sig
  include A
  include B with type t := t and module Q := Q
end

(*
(** This comment is for [Functor]. *)
module Functor(EmptyAlias : EmptySigAlias) = struct
  (** This comment is for [FunctorInner]. *)
  module FunctorInner = EmptyAlias
end
*)

(** This comment is for [FunctorTypeOf]. *)
module FunctorTypeOf(Collection : module type of CollectionModule) = struct
  (** This comment is for [t]. *)
  type t = Collection.collection
end

(** This comment is for [IncludeModuleType]. *)
module type IncludeModuleType = sig
  (** This comment is for [include EmptySigAlias]. *)
  include EmptySigAlias
end

module type ToInclude = sig
  module IncludedA : sig
    type t
  end
  module type IncludedB = sig
    type s
  end
end

module IncludedA = struct
  type t
end

module type IncludedB = sig
  type s
end

(** {3 Advanced Type Stuff} *)

(** This comment is for [record]. *)
type record = {
  field1 : int; (** This comment is for [field1]. *)
  field2 : int; (** This comment is for [field2]. *)
}
(** This comment is also for [record]. *)

type mutable_record = {
  mutable a : int; (** [a] is first and mutable *)
  b : unit; (** [b] is second and immutable *)
  mutable c : int; (** [c] is third and mutable *)
}

type universe_record = {
  nihilate : 'a. 'a -> unit;
}

(** This comment is for [variant]. *)
type variant =
| TagA (** This comment is for [TagA]. *)
| ConstrB of int (** This comment is for [ConstrB]. *)
| ConstrC of int * int (** This comment is for binary [ConstrC]. *)
| ConstrD of (int * int)
(** This comment is for unary [ConstrD] of binary tuple. *)
(** This comment is also for [variant]. *)

(** This comment is for [poly_variant]. *)
type poly_variant = [
| `TagA (** This comment is for [`TagA]. *)
| `ConstrB of int (** This comment is for [`ConstrB]. *)
]
(** Wow! It was a polymorphic variant! *)

(** This comment is for [full_gadt]. *)
type (_,_) full_gadt =
| Tag : (unit,unit) full_gadt
| First : 'a -> ('a,unit) full_gadt
| Second : 'a -> (unit,'a) full_gadt
| Exist : 'a * 'b -> ('b, unit) full_gadt
(** Wow! It was a GADT! *)

(** This comment is for [partial_gadt]. *)
type 'a partial_gadt =
| AscribeTag : 'a partial_gadt
| OfTag of 'a partial_gadt
| ExistGadtTag : ('a -> 'b) -> 'a partial_gadt
(** Wow! It was a mixed GADT! *)

(** This comment is for [record_arg_gadt]. *)
type _ record_arg_gadt =
  | With_rec : { foo : int } -> unit record_arg_gadt
  | With_poly_rec : { bar : 'a. 'a -> 'a } -> ('a -> 'a) record_arg_gadt (** *)
(** Wow! It was a GADT with record arguments *)

(** This comment is for [alias]. *)
type alias = variant

(** This comment is for [tuple]. *)
type tuple = (alias * alias) * alias * (alias * alias)

(** This comment is for [variant_alias]. *)
type variant_alias = variant =
| TagA
| ConstrB of int
| ConstrC of int * int
| ConstrD of (int * int)

(** This comment is for [record_alias]. *)
type record_alias = record = {
  field1 : int;
  field2 : int;
}

(** This comment is for [poly_variant_union]. *)
type poly_variant_union = [
| poly_variant
| `TagC
]

type 'a poly_poly_variant = [
| `TagA of 'a
]

type ('a,'b) bin_poly_poly_variant = [
| `TagA of 'a
| `ConstrB of 'b
]

(* TODO: figure out how to spec a conjunctive type
type amb_poly_variant = [
| unit poly_poly_variant
| (int,unit) bin_poly_poly_variant
| `TagC
]
*)

type 'a open_poly_variant  = [> `TagA ] as 'a

type 'a open_poly_variant2 = [> `ConstrB of int ] as 'a

type 'a open_poly_variant_alias = 'a open_poly_variant open_poly_variant2

type 'a poly_fun = ([> `ConstrB of int ] as 'a) -> 'a

type 'a poly_fun_constraint = 'a -> 'a constraint 'a = [> `TagA ]

type 'a closed_poly_variant = [< `One | `Two ] as 'a

type 'a clopen_poly_variant =
[< `One | `Two of int | `Three > `Two `Three] as 'a

type nested_poly_variant = [
| `A
| `B of [
  | `B1
  | `B2
]
| `C
| `D of [
  | `D1 of [
    `D1a
  ]
]
]

(** This comment is for [full_gadt_alias]. *)
type ('a,'b) full_gadt_alias = ('a,'b) full_gadt =
| Tag : (unit,unit) full_gadt_alias
| First : 'a -> ('a,unit) full_gadt_alias
| Second : 'a -> (unit,'a) full_gadt_alias
| Exist : 'a * 'b -> ('b, unit) full_gadt_alias

(** This comment is for [partial_gadt_alias]. *)
type 'a partial_gadt_alias = 'a partial_gadt =
| AscribeTag : 'a partial_gadt_alias
| OfTag of 'a partial_gadt_alias
| ExistGadtTag : ('a -> 'b) -> 'a partial_gadt_alias

(** This comment is for {!exn_arrow}. *)
exception Exn_arrow : unit -> exn

(** This comment is for {!mutual_constr_a} and {!mutual_constr_b}. *)
type mutual_constr_a =
| A
| B_ish of mutual_constr_b
and mutual_constr_b =
| B
| A_ish of mutual_constr_a

type rec_obj = < f : int; g : unit -> unit; h : rec_obj >

type 'a open_obj = < f : int; g : unit -> unit; .. > as 'a

type 'a oof = (< a : unit; .. > as 'a) -> 'a

type 'a any_obj = < .. > as 'a

type empty_obj = < >

type one_meth = < meth: unit >

(** A mystery wrapped in an ellipsis *)
type ext = ..

type ext += ExtA
type ext += ExtB
type ext +=
| ExtC of unit
| ExtD of ext
type ext += ExtE

type ext += private ExtF

type 'a poly_ext = ..
(** 'a poly_ext *)

type 'b poly_ext += Foo of 'b | Bar of 'b * 'b
(** 'b poly_ext *)

type 'c poly_ext += Quux of 'c

module ExtMod = struct
  type t = ..

  type t += Leisureforce
end

type ExtMod.t += ZzzTop0
(** It's got the rock *)

type ExtMod.t += ZzzTop of unit
(** and it packs a unit. *)

(** Rotate keys on my mark... *)
external launch_missiles : unit -> unit = "tetris"

(** A brown paper package tied up with string*)
type my_mod = (module COLLECTION)

class empty_class = object val x = 0 end

class one_method_class = object
  method go = ()
end

class two_method_class = object
  method one = new one_method_class
  method undo = ()
end

class ['a] param_class x = object
  method v : 'a = x
end


type my_unit_object = unit param_class

type 'a my_unit_class = unit #param_class as 'a

(* Test resolution of dependently typed modules *)
module Dep1 = struct

  module type S = sig
    class c : object
      method m : int
    end
  end

  module X = struct
    module Y = struct
      class c = object
        method m = 4
      end
    end
  end

end

module Dep2 (Arg : sig module type S module X : sig module Y : S end end) =
    struct
      module A = Arg.X
      module B = A.Y
    end

type dep1 = Dep2(Dep1).B.c;;

module Dep3 = struct type a end

module Dep4 = struct
  module type T = sig type b end
  module type S = sig
    module X : T
    module Y : sig end
  end
  module X = struct type b end
end

module Dep5 (Arg : sig
                   module type T
                   module type S = sig
                     module X : T
                     module Y : sig end
                   end
                   module X : T
              end) = struct
      module Z : Arg.S with module Y = Dep3 = struct
        module X = Arg.X
        module Y = Dep3
      end
  end

type dep2 = Dep5(Dep4).Z.X.b

type dep3 = Dep5(Dep4).Z.Y.a

module Dep6 = struct
  module type S = sig type d end
  module type T = sig
    module type R = S
    module Y : R
  end
  module X = struct
    module type R = S
    module Y = struct type d end
  end
end

module Dep7 (Arg : sig
                   module type S
                   module type T = sig
                     module type R = S
                     module Y : R
                   end
                   module X : T
            end) = struct
      module M = Arg.X
    end

type dep4 = Dep7(Dep6).M.Y.d;;

module Dep8 = struct
  module type T = sig type t end
end

module Dep9(X : sig module type T end) = X

module type Dep10 = Dep9(Dep8).T with type t = int

module Dep11 = struct
  module type S = sig
    class c : object
      method m : int
    end
  end
end

module Dep12 =
  functor (Arg : sig module type S end) -> struct
      module type T = Arg.S
end

module Dep13 = struct
  class c = object
    method m = 4
  end
end

type dep5 = Dep13.c

module type With1 = sig
  module M : sig
    module type S
  end
  module N : M.S
end

module With2 = struct
  module type S = sig type t end
end

module With3 = struct
  module M = With2
  module N = struct
    type t = int
  end
end

type with1 = With3.N.t

module With4 = struct
  module N = struct
    type t = int
  end
end

type with2 = With4.N.t

module With5 = struct
  module type S = sig type t end
  module N = struct type t = float end
end

module With6 = struct
  module type T = sig
    module M : sig
      module type S
      module N : S
    end
  end
end

module With7 (X : sig module type T end) = X

module type With8 = With7(With6).T with module M = With5 and type M.N.t = With5.N.t

module With9 = struct
  module type S = sig type t end
end

module With10 = struct
  module type T = sig
    module M : sig
      module type S
    end
    module N : M.S
  end
end

module type With11 = With7(With10).T with module M = With9 and type N.t = int

module type NestedInclude1 = sig

  module type NestedInclude2 = sig type nested_include end

end

module type NestedInclude2 = sig
  type nested_include
end

type nested_include = int

module DoubleInclude1 = struct
  module DoubleInclude2 = struct
    type double_include
  end
end

module DoubleInclude3 = struct
  include DoubleInclude1
end

include DoubleInclude3.DoubleInclude2

module IncludeInclude1 = struct
  module type IncludeInclude2 = sig
    type include_include
  end
end

include IncludeInclude1
type include_include

module Caml_list = List

module CanonicalTest = struct
  module Base__List = struct
    type 'a t = 'a list

    let id x = x
  end

  module Base__ = struct
    (** @canonical Ocamlary.CanonicalTest.Base.List *)
    module List = Base__List
  end

  module Base = struct
    module List = Base__.List
  end

  module Base__Tests = struct
    module C = struct
      include Base__.List
    end

    open Base__

    module L = List

    let foo (l : int L.t) : float L.t =
      Caml_list.map float_of_int l

    (** This is just {!List.id}, or rather {!L.id} *)
    let bar (l : 'a List.t) : 'a List.t =
      L.id l

    (** Just seeing if {!Base__.List.t} ([Base__.List.t]) gets rewriten to
        {!Base.List.t} ([Base.List.t]) *)
    let baz (_ : 'a Base__.List.t) = ()
  end

  module List_modif = struct
    include Base.List
  end
end

let test _ = ()
(** Some ref to {!CanonicalTest.Base__Tests.C.t} and {!CanonicalTest.Base__Tests.D.id}.
    But also to {!CanonicalTest.Base__.List} and {!CanonicalTest.Base__.List.t} *)

(** {1 Aliases again} *)

module Aliases = struct
  (** Let's imitate jst's layout. *)

  module Foo__A = struct
    type t

    let id t = t
  end

  module Foo__B = struct
    type t

    let id t = t
  end

  module Foo__C = struct
    type t

    let id t = t
  end

  module Foo__D = struct
    type t

    let id t = t
  end

  module Foo__E = struct
    type t

    let id t = t
  end

  module Foo__ = struct

    (** @canonical Ocamlary.Aliases.Foo.A *)
    module A = Foo__A

    (** @canonical Ocamlary.Aliases.Foo.B *)
    module B = Foo__B

    (** @canonical Ocamlary.Aliases.Foo.C *)
    module C = Foo__C

    (** @canonical Ocamlary.Aliases.Foo.D *)
    module D = Foo__D

    module E = Foo__E
  end

  module Foo = struct
    open Foo__

    module A = A
    module B = B
    module C = C
    module D = D

    module E = E
  end

  module A' = Foo.A

  type tata = Foo.A.t
  type tbtb = Foo__.B.t
  type tete = Foo__.E.t
  type tata' = A'.t
  type tete2 = Foo.E.t

  module Std = struct
    module A = Foo.A
    module B = Foo.B
    module C = Foo.C
    module D = Foo.D
    module E = Foo.E
  end

  type stde = Std.E.t

  (** {3 include of Foo}

      Just for giggle, let's see what happens when we include {!Foo}. *)

  include Foo (** @inline *)

  type testa = A.t

  (** And also, let's refer to {!A.t} and {!Foo.B.id} *)

  module P1 = struct
    (** @canonical Ocamlary.Aliases.P2.Z *)
    module Y = struct
      type t

      let id x = x
    end
  end

  module P2 = struct
    module Z = P1.Y
  end

  module X1 = P1.Y
  module X2 = P2.Z

  type p1 = X1.t
  type p2 = X2.t
end

(** {1 New reference syntax} *)

module type M = sig
  type t
end

module M = struct
  type t
end

(** Here goes:
    - [{!M.t}] : {!M.t}
    - [{!module-M.t}] : {!module-M.t}
    - [{!module-type-M.t}] : {!module-type-M.t} *)

module Only_a_module = struct
  type t
end

(** Some here should fail:
    - [{!Only_a_module.t}] : {!Only_a_module.t}
    - [{!module-Only_a_module.t}] : {!module-Only_a_module.t}
    - [{!module-type-Only_a_module.t}] : {!module-type-Only_a_module.t} *)
