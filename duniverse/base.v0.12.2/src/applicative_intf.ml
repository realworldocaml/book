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
  val map : [`Define_using_apply | `Custom of ('a t -> f:('a -> 'b) -> 'b t)]
end

module type Basic_using_map2 = sig
  type 'a t
  val return : 'a -> 'a t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val map : [`Define_using_map2 | `Custom of ('a t -> f:('a -> 'b) -> 'b t)]
end

module type Applicative_infix = sig
  type 'a t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t (** same as [apply] *)

  val ( <*  ) : 'a t -> unit t -> 'a t
  val (  *> ) : unit t -> 'a t -> 'a t

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

  val all_ignore : unit t list -> unit t [@@deprecated "[since 2018-02] Use [all_unit]"]

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

(** Argument lists and associated N-ary map and apply functions. *)
module type Args = sig

  type 'a arg (** the underlying applicative *)

  (** ['f] is the type of a function that consumes the list of arguments and returns an
      ['r]. *)
  type ('f, 'r) t

  (** the empty argument list **)
  val nil : ('r, 'r) t

  (** prepend an argument *)
  val cons : 'a arg -> ('f, 'r) t -> ('a -> 'f, 'r) t

  (** infix operator for [cons] *)
  val (@>) : 'a arg -> ('f, 'r) t -> ('a -> 'f, 'r) t

  (** Transform argument values in some way.  For example, one can label a function
      argument like so:

      {[
        step ~f:(fun f x -> f ~foo:x) : ('a -> 'r1, 'r2) t -> (foo:'a -> 'r1, 'r2) t
      ]} *)
  val step : ('f1, 'r) t -> f:('f2 -> 'f1) -> ('f2, 'r) t

  (** The preferred way to factor out an [Args] sub-sequence:

      {[
        let args =
          Foo.Args.(
            bar "A"
            (* TODO: factor out the common baz qux sub-sequence *)
            @> baz "B"
            @> qux "C"
            @> zap "D"
            @> nil
          )
      ]}

      is to write a function that prepends the sub-sequence:

      {[
        let baz_qux remaining_args =
          Foo.Args.(
            baz "B"
            @> qux "C"
            @> remaining_args
          )
      ]}

      and splice it back into the original sequence using [@@] so that things line up
      nicely:

      {[
        let args =
          Foo.Args.(
            bar "A"
            @> baz_qux
            @@ zap "D"
            @> nil
          )
      ]} *)

  val mapN : f:'f -> ('f, 'r) t -> 'r arg

  val applyN : 'f arg -> ('f, 'r) t -> 'r arg

end
[@@deprecated "[since 2018-09] Use [ppx_let] instead."]

module type Basic2 = sig
  type ('a, 'e) t
  val return : 'a -> ('a, _) t
  val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  val map : [`Define_using_apply | `Custom of (('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t)]
end

module type Basic2_using_map2 = sig
  type ('a, 'e) t
  val return : 'a -> ('a, _) t
  val map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
  val map : [`Define_using_map2 | `Custom of (('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t)]
end

module type S2 = sig
  type ('a, 'e) t

  val return : 'a -> ('a, _) t

  val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t

  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t

  val map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t

  val map3
    :  ('a, 'e) t
    -> ('b, 'e) t
    -> ('c, 'e) t
    -> f:('a -> 'b -> 'c -> 'd)
    -> ('d, 'e) t

  val all : ('a, 'e) t list -> ('a list, 'e) t

  val all_unit : (unit, 'e) t list -> (unit, 'e) t

  val all_ignore : (unit, 'e) t list -> (unit, 'e) t
  [@@deprecated "[since 2018-02] Use [all_unit]"]

  val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

  module Applicative_infix : sig
    val ( <*> ) : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
    val ( <*  ) : ('a, 'e) t -> (unit, 'e) t -> ('a, 'e) t
    val (  *> ) : (unit, 'e) t -> ('a, 'e) t -> ('a, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end

  include module type of Applicative_infix
end

module type Args2 = sig
  type ('a, 'e) arg

  type ('f, 'r, 'e) t

  val nil : ('r, 'r, _) t

  val cons : ('a, 'e) arg -> ('f, 'r, 'e) t -> ('a -> 'f, 'r, 'e) t
  val (@>) : ('a, 'e) arg -> ('f, 'r, 'e) t -> ('a -> 'f, 'r, 'e) t

  val step : ('f1, 'r, 'e) t -> f:('f2 -> 'f1) -> ('f2, 'r, 'e) t

  val mapN : f:'f -> ('f, 'r, 'e) t -> ('r, 'e) arg
  val applyN : ('f, 'e) arg -> ('f, 'r, 'e) t -> ('r, 'e) arg
end
[@@deprecated "[since 2018-09] Use [ppx_let] instead."]

module type Applicative = sig

  module type Applicative_infix = Applicative_infix
  module type Args              = Args
  [@@warning "-3"]
  [@@deprecated "[since 2018-09] Use [ppx_let] instead."]
  module type Args2             = Args2
  [@@warning "-3"]
  [@@deprecated "[since 2018-09] Use [ppx_let] instead."]
  module type Basic             = Basic
  module type Basic2            = Basic2
  module type Basic2_using_map2 = Basic2_using_map2
  module type Basic_using_map2  = Basic_using_map2
  module type Let_syntax        = Let_syntax
  module type S                 = S
  module type S2                = S2

  module Args_to_Args2 (X : Args) :
    Args2
    with type ('a, 'e) arg = 'a X.arg
    with type ('f, 'r, 'e) t = ('f, 'r) X.t
      [@@warning "-3"]

  module S2_to_S (X : S2) : S with type 'a t = ('a, unit) X.t

  module S_to_S2 (X : S) : S2 with type ('a, 'e) t = 'a X.t

  module Make  (X : Basic ) : S  with type  'a      t :=  'a      X.t
  module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t

  module Make_let_syntax
      (X : For_let_syntax)
      (Intf : sig module type S end)
      (Impl : Intf.S)
    : Let_syntax with type 'a t := 'a X.t
      with module Open_on_rhs_intf := Intf

  module Make_using_map2  (X : Basic_using_map2 ) : S  with type  'a      t :=  'a      X.t
  module Make2_using_map2 (X : Basic2_using_map2) : S2 with type ('a, 'e) t := ('a, 'e) X.t

  module Make_args  (X : S ) : Args  with type  'a      arg :=  'a      X.t [@@warning "-3"]
  [@@deprecated "[since 2018-09] Use [ppx_let] instead."]
  module Make_args2 (X : S2) : Args2 with type ('a, 'e) arg := ('a, 'e) X.t [@@warning "-3"]
  [@@deprecated "[since 2018-09] Use [ppx_let] instead."]

  (** The following functors give a sense of what Applicatives one can define.

      Of these, [Of_monad] is likely the most useful.  The others are mostly didactic. *)

  (** Every monad is Applicative via:

      {[
        let apply mf mx =
          mf >>= fun f ->
          mx >>| fun x ->
          f x
      ]} *)
  module Of_monad (M : Monad.S)   : S with type 'a t := 'a M.t
  module Compose  (F : S) (G : S) : S with type 'a t =  'a F.t G.t
  module Pair     (F : S) (G : S) : S with type 'a t =  'a F.t * 'a G.t

end
