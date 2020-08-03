(* These tests are run on only the most recent version of the compiler that is
   explicitly supported by odoc. This allows us to test doc generation for new
   language features. *)

module type S = sig end

module type S1 = S -> S

type variant =
  | A
  | B of int
  | C (** foo *)
  | D (** {e bar} *)
  | E of {a : int}

type _ gadt =
  | A : int gadt
  | B : int -> string gadt (** foo *)
  | C : {a : int} -> unit gadt

type polymorphic_variant = [
  | `A
  | `B of int
  | `C (** foo *)
  | `D (** bar *)
]

type nonrec nonrec_ = int


(* Conjunctive types: dune compilation scheme exposes a bug in old
   versions of the compiler *)
type empty_conj= X: [< `X of & 'a & int * float  ] -> empty_conj
type conj = X: [< `X of int & [< `B of int & float ] ] -> conj
val empty_conj: [< `X of & 'a & int * float  ]
val conj : [< `X of int & [< `B of int & float ] ]

module Z : sig
  module Y : sig
    module X : sig
      type 'a t
    end
  end
end

module X : sig
  module L := Z.Y
  type t = int L.X.t
  type u := int
  type v = u L.X.t
end



