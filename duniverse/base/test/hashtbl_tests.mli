open! Base

module type Hashtbl_for_testing = sig
  include Hashtbl.Accessors with type 'key key = 'key
  include Invariant.S2 with type ('key, 'data) t := ('key, 'data) t

  val create_poly : ?size:int -> unit -> ('key, 'data) t

  val of_alist_poly_exn : ('key * 'data) list -> ('key, 'data) t
  val of_alist_poly_or_error : ('key * 'data) list -> ('key, 'data) t Or_error.t
end

module Make (Hashtbl : Hashtbl_for_testing) : sig end
