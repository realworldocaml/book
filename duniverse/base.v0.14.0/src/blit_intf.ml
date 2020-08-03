(** Standard type for [blit] functions, and reusable code for validating [blit]
    arguments. *)

open! Import

(** If [blit : (src, dst) blit], then [blit ~src ~src_pos ~len ~dst ~dst_pos] blits [len]
    values from [src] starting at position [src_pos] to [dst] at position [dst_pos].
    Furthermore, [blit] raises if [src_pos], [len], and [dst_pos] don't specify valid
    slices of [src] and [dst]. *)
type ('src, 'dst) blit =
  src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit

(** [blito] is like [blit], except that the [src_pos], [src_len], and [dst_pos] are
    optional (hence the "o" in "blito").  Also, we use [src_len] rather than [len] as a
    reminder that if [src_len] isn't supplied, then the default is to take the slice
    running from [src_pos] to the end of [src]. *)
type ('src, 'dst) blito =
  src:'src
  -> ?src_pos:int (** default is [0] *)
  -> ?src_len:int (** default is [length src - src_pos] *)
  -> dst:'dst
  -> ?dst_pos:int (** default is [0] *)
  -> unit
  -> unit

(** If [sub : (src, dst) sub], then [sub ~src ~pos ~len] returns a sequence of type [dst]
    containing [len] characters of [src] starting at [pos].

    [subo] is like [sub], except [pos] and [len] are optional. *)
type ('src, 'dst) sub = 'src -> pos:int -> len:int -> 'dst

type ('src, 'dst) subo =
  ?pos:int (** default is [0] *)
  -> ?len:int (** default is [length src - pos] *)
  -> 'src
  -> 'dst

(*_ These are not implemented less-general-in-terms-of-more-general because odoc produces
  unreadable documentation in that case, with or without [inline] on [include]. *)

module type S = sig
  type t

  val blit : (t, t) blit
  val blito : (t, t) blito
  val unsafe_blit : (t, t) blit
  val sub : (t, t) sub
  val subo : (t, t) subo
end

module type S1 = sig
  type 'a t

  val blit : ('a t, 'a t) blit
  val blito : ('a t, 'a t) blito
  val unsafe_blit : ('a t, 'a t) blit
  val sub : ('a t, 'a t) sub
  val subo : ('a t, 'a t) subo
end

module type S_distinct = sig
  type src
  type dst

  val blit : (src, dst) blit
  val blito : (src, dst) blito
  val unsafe_blit : (src, dst) blit
  val sub : (src, dst) sub
  val subo : (src, dst) subo
end

module type S1_distinct = sig
  type 'a src
  type 'a dst

  val blit : (_ src, _ dst) blit
  val blito : (_ src, _ dst) blito
  val unsafe_blit : (_ src, _ dst) blit
  val sub : (_ src, _ dst) sub
  val subo : (_ src, _ dst) subo
end

module type S_to_string = sig
  type t

  val sub : (t, string) sub
  val subo : (t, string) subo
end

(** Users of modules matching the blit signatures [S], [S1], and [S1_distinct] only need
    to understand the code above.  The code below is only for those that need to implement
    modules that match those signatures. *)

module type Sequence = sig
  type t

  val length : t -> int
end

type 'a poly = 'a

module type Sequence1 = sig
  type 'a t

  (** [Make1*] guarantees to only call [create_like ~len t] with [len > 0] if [length t >
      0]. *)
  val create_like : len:int -> 'a t -> 'a t

  val length : _ t -> int
  val unsafe_blit : ('a t, 'a t) blit
end

module type Blit = sig
  type nonrec ('src, 'dst) blit = ('src, 'dst) blit
  type nonrec ('src, 'dst) blito = ('src, 'dst) blito
  type nonrec ('src, 'dst) sub = ('src, 'dst) sub
  type nonrec ('src, 'dst) subo = ('src, 'dst) subo

  module type S = S
  module type S1 = S1
  module type S_distinct = S_distinct
  module type S1_distinct = S1_distinct
  module type S_to_string = S_to_string
  module type Sequence = Sequence
  module type Sequence1 = Sequence1

  (** There are various [Make*] functors that turn an [unsafe_blit] function into a [blit]
      function.  The functors differ in whether the sequence type is monomorphic or
      polymorphic, and whether the src and dst types are distinct or are the same.

      The blit functions make sure the slices are valid and then call [unsafe_blit].  They
      guarantee at a call [unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len] that:

      {[
        len > 0
        && src_pos >= 0
        && src_pos + len <= get_src_len src
        && dst_pos >= 0
        && dst_pos + len <= get_dst_len dst
      ]}

      The [Make*] functors also automatically create unit tests. *)

  (** [Make] is for blitting between two values of the same monomorphic type. *)
  module Make (Sequence : sig
      include Sequence

      val create : len:int -> t
      val unsafe_blit : (t, t) blit
    end) : S with type t := Sequence.t

  (** [Make_distinct] is for blitting between values of distinct monomorphic types. *)
  module Make_distinct
      (Src : Sequence) (Dst : sig
                          include Sequence

                          val create : len:int -> t
                          val unsafe_blit : (Src.t, t) blit
                        end) : S_distinct with type src := Src.t with type dst := Dst.t

  module Make_to_string (T : sig
      type t
    end)
      (To_bytes : S_distinct with type src := T.t with type dst := bytes) :
    S_to_string with type t := T.t

  (** [Make1] is for blitting between two values of the same polymorphic type. *)
  module Make1 (Sequence : Sequence1) : S1 with type 'a t := 'a Sequence.t

  (** [Make1_generic] is for blitting between two values of the same container type that's
      not fully polymorphic (in the sense of Container.Generic). *)
  module Make1_generic (Sequence : Sequence1) : S1 with type 'a t := 'a Sequence.t
end
