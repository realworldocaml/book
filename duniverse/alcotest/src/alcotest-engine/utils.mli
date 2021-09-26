val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

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

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result
  val init : int -> (int -> 'a) -> 'a list
end

module Result : sig
  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
end

module Option : sig
  val is_some : _ option -> bool
  val get_exn : 'a option -> 'a
  val value : default:'a -> 'a option -> 'a
  val ( || ) : 'a option -> 'a option -> 'a option
end
