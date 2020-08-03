(** runtime representation of the name of type ['a].
    Useful for representing types with a nominal notion of equality *)

type 'a t
type 'a typename = 'a t

val create : ?name:string -> unit -> 'a t
val static : unit t

(** nominal type equality test *)
val same : _ t -> _ t -> bool
val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t

(** a runtime representation of fully applied type ['a] *)
module Key : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

val key : 'a t -> Key.t

(** an untyped runtime representation of non applied type *)
module Uid : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val name : t -> string
end

val uid : 'a t -> Uid.t
val name : 'a t -> string

module type S0 = sig
  type t
  val typename_of_t : t typename
end

module type S1 = sig
  type 'a t
  val typename_of_t : 'a typename -> 'a t typename
end

module type S2 = sig
  type ('a, 'b) t
  val typename_of_t :
    'a typename
    -> 'b typename
    -> ('a, 'b) t typename
end

module type S3 = sig
  type ('a, 'b, 'c) t
  val typename_of_t :
    'a typename
    -> 'b typename
    -> 'c typename
    -> ('a, 'b, 'c) t typename
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t
  val typename_of_t :
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> ('a, 'b, 'c, 'd) t typename
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val typename_of_t :
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> 'e typename
    -> ('a, 'b, 'c, 'd, 'e) t typename
end

module Make0(X : Named_intf.S0) : S0
  with type t := X.t
module Make1(X : Named_intf.S1) : S1
  with type 'a t := 'a X.t
module Make2(X : Named_intf.S2) : S2
  with type ('a, 'b) t := ('a, 'b) X.t
module Make3(X : Named_intf.S3) : S3
  with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t
module Make4(X : Named_intf.S4) : S4
  with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) X.t
module Make5(X : Named_intf.S5) : S5
  with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) X.t

module Table(X : sig
  type 'a t
end) : sig
  type t
  val create : int -> t
  val mem : t -> 'a typename -> bool
  val set : t -> 'a typename -> 'a X.t -> unit
  val find : t -> 'a typename -> 'a X.t option
end

(* witness of equality between non applied types *)

module Same_witness_exn_1 (A : S1) (B : S1) : sig
  type t = { eq : 'a. (
    'a A.t,
    'a B.t
  ) Type_equal.t }
  val witness : t
end

module Same_witness_exn_2 (A : S2) (B : S2) : sig
  type t = { eq : 'a 'b. (
    ('a, 'b) A.t,
    ('a, 'b) B.t
  ) Type_equal.t }
  val witness : t
end

module Same_witness_exn_3 (A : S3) (B : S3) : sig
  type t = { eq : 'a 'b 'c. (
    ('a, 'b, 'c) A.t,
    ('a, 'b, 'c) B.t
  ) Type_equal.t }
  val witness : t
end

module Same_witness_exn_4 (A : S4) (B : S4) : sig
  type t = { eq : 'a 'b 'c 'd. (
    ('a, 'b, 'c, 'd) A.t,
    ('a, 'b, 'c, 'd) B.t
  ) Type_equal.t }
  val witness : t
end

module Same_witness_exn_5 (A : S5) (B : S5) : sig
  type t = { eq : 'a 'b 'c 'd 'e. (
    ('a, 'b, 'c, 'd, 'e) A.t,
    ('a, 'b, 'c, 'd, 'e) B.t
  ) Type_equal.t }
  val witness : t
end
