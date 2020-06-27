(** Applicatives model computations in which values computed by subcomputations cannot
    affect what subsequent computations will take place.

    Relative to monads, this restriction takes power away from the user of the interface
    and gives it to the implementation.  In particular, because the structure of the
    entire computation is known, one can augment its definition with some description of
    that structure.

    For more information, see:

    {v
      Applicative Programming with Effects.
      Conor McBride and Ross Paterson.
      Journal of Functional Programming 18:1 (2008), pages 1-13.
      http://staff.city.ac.uk/~ross/papers/Applicative.pdf
    v} *)

open! Import

module type Basic = sig
  type 'a t

  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t

  (** The following identities ought to hold for every Applicative (for some value of =):

      - identity:     [return Fn.id <*> t = t]
      - composition:  [return Fn.compose <*> tf <*> tg <*> tx = tf <*> (tg <*> tx)]
      - homomorphism: [return f <*> return x = return (f x)]
      - interchange:  [tf <*> return x = return (fun f -> f x) <*> tf]

      Note: <*> is the infix notation for apply. *)

  (** The [map] argument to [Applicative.Make] says how to implement the applicative's
      [map] function.  [`Define_using_apply] means to define [map t ~f = return f <*> t].
      [`Custom] overrides the default implementation, presumably with something more
      efficient.

      Some other functions returned by [Applicative.Make] are defined in terms of [map],
      so passing in a more efficient [map] will improve their efficiency as well. *)
  val map : [ `Define_using_apply | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
end

module type Basic_using_map2 = sig
  type 'a t

  val return : 'a -> 'a t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
end

module type Applicative_infix = sig
  type 'a t


  (** same as [apply] *)
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( <* ) : 'a t -> unit t -> 'a t
  val ( *> ) : unit t -> 'a t -> 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
end

module type For_let_syntax = sig
  type 'a t

  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t

  include Applicative_infix with type 'a t := 'a t
end

module type S = sig
  include For_let_syntax

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t

  module Applicative_infix : Applicative_infix with type 'a t := 'a t
end

module type Let_syntax = sig
  type 'a t

  module Open_on_rhs_intf : sig
    module type S
  end

  module Let_syntax : sig
    val return : 'a -> 'a t

    include Applicative_infix with type 'a t := 'a t

    module Let_syntax : sig
      val return : 'a -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val both : 'a t -> 'b t -> ('a * 'b) t

      module Open_on_rhs : Open_on_rhs_intf.S
    end
  end
end

module type Basic2 = sig
  type ('a, 'e) t

  val return : 'a -> ('a, _) t
  val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  val map : [ `Define_using_apply | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
end

module type Basic2_using_map2 = sig
  type ('a, 'e) t

  val return : 'a -> ('a, _) t
  val map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
  val map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
end

module type Applicative_infix2 = sig
  type ('a, 'e) t

  val ( <*> ) : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  val ( <* ) : ('a, 'e) t -> (unit, 'e) t -> ('a, 'e) t
  val ( *> ) : (unit, 'e) t -> ('a, 'e) t -> ('a, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
end

module type For_let_syntax2 = sig
  type ('a, 'e) t

  val return : 'a -> ('a, _) t
  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

  include Applicative_infix2 with type ('a, 'e) t := ('a, 'e) t
end

module type S2 = sig
  include For_let_syntax2

  val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  val map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t

  val map3
    :  ('a, 'e) t
    -> ('b, 'e) t
    -> ('c, 'e) t
    -> f:('a -> 'b -> 'c -> 'd)
    -> ('d, 'e) t

  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t

  module Applicative_infix : Applicative_infix2 with type ('a, 'e) t := ('a, 'e) t
end

module type Let_syntax2 = sig
  type ('a, 'e) t

  module Open_on_rhs_intf : sig
    module type S
  end

  module Let_syntax : sig
    val return : 'a -> ('a, _) t

    include Applicative_infix2 with type ('a, 'e) t := ('a, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, _) t
      val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      module Open_on_rhs : Open_on_rhs_intf.S
    end
  end
end

module type Basic3 = sig
  type ('a, 'd, 'e) t

  val return : 'a -> ('a, _, _) t
  val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t

  val map
    : [ `Define_using_apply
      | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
      ]
end

module type Basic3_using_map2 = sig
  type ('a, 'd, 'e) t

  val return : 'a -> ('a, _, _) t
  val map2 : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'd, 'e) t

  val map
    : [ `Define_using_map2 | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
end

module type Applicative_infix3 = sig
  type ('a, 'd, 'e) t

  val ( <*> ) : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  val ( <* ) : ('a, 'd, 'e) t -> (unit, 'd, 'e) t -> ('a, 'd, 'e) t
  val ( *> ) : (unit, 'd, 'e) t -> ('a, 'd, 'e) t -> ('a, 'd, 'e) t
  val ( >>| ) : ('a, 'd, 'e) t -> ('a -> 'b) -> ('b, 'd, 'e) t
end

module type For_let_syntax3 = sig
  type ('a, 'd, 'e) t

  val return : 'a -> ('a, _, _) t
  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
  val both : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('a * 'b, 'd, 'e) t

  include Applicative_infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
end

module type S3 = sig
  include For_let_syntax3

  val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  val map2 : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'd, 'e) t

  val map3
    :  ('a, 'd, 'e) t
    -> ('b, 'd, 'e) t
    -> ('c, 'd, 'e) t
    -> f:('a -> 'b -> 'c -> 'result)
    -> ('result, 'd, 'e) t

  val all : ('a, 'd, 'e) t list -> ('a list, 'd, 'e) t
  val all_unit : (unit, 'd, 'e) t list -> (unit, 'd, 'e) t

  module Applicative_infix :
    Applicative_infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
end

module type Let_syntax3 = sig
  type ('a, 'd, 'e) t

  module Open_on_rhs_intf : sig
    module type S
  end

  module Let_syntax : sig
    val return : 'a -> ('a, _, _) t

    include Applicative_infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, _, _) t
      val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
      val both : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('a * 'b, 'd, 'e) t

      module Open_on_rhs : Open_on_rhs_intf.S
    end
  end
end

module type Applicative = sig
  module type Applicative_infix = Applicative_infix
  module type Applicative_infix2 = Applicative_infix2
  module type Applicative_infix3 = Applicative_infix3
  module type Basic = Basic
  module type Basic2 = Basic2
  module type Basic3 = Basic3
  module type Basic_using_map2 = Basic_using_map2
  module type Basic2_using_map2 = Basic2_using_map2
  module type Basic3_using_map2 = Basic3_using_map2
  module type Let_syntax = Let_syntax
  module type Let_syntax2 = Let_syntax2
  module type Let_syntax3 = Let_syntax3
  module type S = S
  module type S2 = S2
  module type S3 = S3

  module S2_to_S (X : S2) : S with type 'a t = ('a, unit) X.t
  module S_to_S2 (X : S) : S2 with type ('a, 'e) t = 'a X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'd) t = ('a, 'd, unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'd, 'e) t = ('a, 'd) X.t
  module Make (X : Basic) : S with type 'a t := 'a X.t
  module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t
  module Make3 (X : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t

  module Make_let_syntax
      (X : For_let_syntax) (Intf : sig
                              module type S
                            end)
      (Impl : Intf.S) :
    Let_syntax with type 'a t := 'a X.t with module Open_on_rhs_intf := Intf

  module Make_let_syntax2
      (X : For_let_syntax2) (Intf : sig
                               module type S
                             end)
      (Impl : Intf.S) :
    Let_syntax2 with type ('a, 'e) t := ('a, 'e) X.t with module Open_on_rhs_intf := Intf

  module Make_let_syntax3
      (X : For_let_syntax3) (Intf : sig
                               module type S
                             end)
      (Impl : Intf.S) :
    Let_syntax3
    with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t
    with module Open_on_rhs_intf := Intf

  module Make_using_map2 (X : Basic_using_map2) : S with type 'a t := 'a X.t

  module Make2_using_map2 (X : Basic2_using_map2) :
    S2 with type ('a, 'e) t := ('a, 'e) X.t

  module Make3_using_map2 (X : Basic3_using_map2) :
    S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t

  (** The following functors give a sense of what Applicatives one can define.

      Of these, [Of_monad] is likely the most useful.  The others are mostly didactic. *)

  (** Every monad is Applicative via:

      {[
        let apply mf mx =
          mf >>= fun f ->
          mx >>| fun x ->
          f x
      ]} *)
  module Of_monad (M : Monad.S) : S with type 'a t := 'a M.t

  module Of_monad2 (M : Monad.S2) : S2 with type ('a, 'e) t := ('a, 'e) M.t
  module Compose (F : S) (G : S) : S with type 'a t = 'a F.t G.t
  module Pair (F : S) (G : S) : S with type 'a t = 'a F.t * 'a G.t
end
