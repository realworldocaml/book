type abstract
(** Some {e documentation}. *)

type alias = int

type private_ = private int

type 'a constructor = 'a

type arrow = int -> int

type higher_order = (int -> int) -> int

type labeled = l:int -> int

type optional = ?l:int -> int

type labeled_higher_order = (l:int -> int) -> (?l:int -> int) -> int

type pair = int * int

type parens_dropped = (int * int)

type triple = int * int * int

type nested_pair = (int * int) * int

type instance = int constructor

type variant_e = {a : int}
type variant =
  | A
  | B of int
  | C (** foo *)
  | D (** {e bar} *)
  | E of variant_e

type variant_c = {a: int}
type _ gadt =
  | A : int gadt
  | B : int -> string gadt
  | C : variant_c -> unit gadt

type degenerate_gadt =
  | A : degenerate_gadt

type private_variant = private A

type record = {
  a : int;
  mutable b : int;
  c : int; (** foo *)
  d : int; (** {e bar} *)
  e : 'a. 'a;
}

(* 4.02 doesn't preserve doc comments on polymorphic variant constructors, but
   they should be restored if 4.02 support is dropped, or if creating a test
   that won't run on 4.02. *)
type polymorphic_variant = [
  | `A
  | `B of int
  | `C of int * unit
  | `D
]

type polymorphic_variant_extension = [
  | polymorphic_variant (** {not e} shown *)
  | `E
]

type nested_polymorphic_variant = [
  | `A of [ `B | `C ]
]

type private_extenion = private [> polymorphic_variant ]

type object_ = <
  a : int;
  b : int; (** foo *)
  c : int; (** {e bar} *)
>

module type X = sig type t type u end

type module_ = (module X)

type module_substitution = (module X with type t = int and type u = unit)

type +'a covariant

type -'a contravariant

type _ bivariant = int

type ('a, 'b) binary

type using_binary = (int, int) binary

type 'custom name

type 'a constrained = 'a constraint 'a = int

type 'a exact_variant = 'a constraint 'a = [ `A | `B of int ]

type 'a lower_variant = 'a constraint 'a = [> `A | `B of int ]

type 'a any_variant = 'a constraint 'a = [> ]

type 'a upper_variant = 'a constraint 'a = [< `A | `B of int ]

type 'a named_variant = 'a constraint 'a = [< polymorphic_variant ]

type 'a exact_object = 'a constraint 'a = <a : int; b : int>

type 'a lower_object = 'a constraint 'a = <a : int; b : int; ..>

type 'a poly_object = 'a constraint 'a = <a : 'a. 'a>

type ('a, 'b) double_constrained = 'a * 'b
  constraint 'a = int
  constraint 'b = unit

type as_ = (int as 'a) * 'a

type extensible = ..

type extensible += Extension | Another_extension

type mutually = A of recursive
and recursive = B of mutually

(* Not a type, but analogous to extensions. *)
exception Foo of int * int
