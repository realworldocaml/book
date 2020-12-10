(** @TxtAttribute *)

val v1 : int
(** @ValueDeclaration *)

(** @ValueDeclaration *)
val v2 : int
(** @ValueDeclaration *)

type t1 = int
(** @TypeDeclaration *)

(** @TypeDeclaration *)
type t2 = A (** @ConstructorDeclaration *)
        | B (** @ConstructorDeclaration *)

type t3 = [
| `A
| `B
]

(** @TypeDeclaration *)
type t4 = {
  a : int; (** @LabelDeclaration *)
  b : bool; (** @LabelDeclaration *)
}

type te = ..
(** @TypeDeclaration *)

(** @TypeExtension *)
type te += A (** @Extension *)
         | B (** @Extension *)

(** @ModuleTypeDeclaration *)
module type Mt = sig
  (** @TxtAttribute *)

  type t = int
  (** @TypeDeclaration *)
end

(** @ModuleDeclaration *)
module M : sig
  (** @TxtAttribute *)

  (** @ModuleDeclaration *)
  module N : sig
    (** @TxtAttribute *)

    type t = int
    (** @TypeDeclaration *)
  end
end

exception Kaboom
(** @Exception *)

(**/**)
(** @Hidden *)
(**/**)

(** @Functor *)
module F : functor (M : Map.OrderedType) -> Map.S

(** @IncludeDescription *)
include sig
  (** @TxtAttribute *)

  type t = int
  (** @TypeDeclaration *)
end

(** @Class *)
class empty_class : object
  method go : unit
  (** @Method *)
end
