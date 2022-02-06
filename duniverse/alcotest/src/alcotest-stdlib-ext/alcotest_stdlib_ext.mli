(** This module exists for internal Alcotest use only. It provides no stability
    guarantee. *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

module Fun : sig
  val id : 'a -> 'a
end

module Int : sig
  module Set : Set.S with type elt = int
end

module String : sig
  include module type of Astring.String

  val length_utf8 : string -> int
  (** Get the length of a string in UTF-8 characters and malformed segments. *)

  val prefix_utf8 : int -> string -> string
  (** [prefix_utf8 n s] is the prefix of [s] containing [n] UTF-8 characters (or
      [s] if it contains fewer than [n] UTF-8 characters). *)
end

module List : sig
  include module type of List

  type 'a t = 'a list

  val rev_head : int -> 'a list -> 'a list
  (** Reverse a list, taking at most the first n elements of the original list. *)

  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result
  val init : int -> (int -> 'a) -> 'a list
end

module Result : sig
  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
end

module Option : sig
  val map : ('a -> 'b) -> 'a option -> 'b option
  val is_some : _ option -> bool
  val get_exn : 'a option -> 'a
  val value : default:'a -> 'a option -> 'a
  val ( || ) : 'a option -> 'a option -> 'a option
end

module Cmdliner_syntax : sig
  open Cmdliner

  val ( let+ ) : 'a Term.t -> ('a -> 'b) -> 'b Term.t
  val ( and+ ) : 'a Term.t -> 'b Term.t -> ('a * 'b) Term.t
  val ( >>| ) : 'a Term.t -> ('a -> 'b) -> 'b Term.t
end
