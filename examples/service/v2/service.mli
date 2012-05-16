open Core.Std

(** The module type for a service. *)
module type S = sig
  type t
  type config with sexp

  val create         : config -> t
  val reconfigure    : t -> config -> unit
  val name           : t -> string
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

module Configured : sig
  type t
  val create :
    (module S with type config = 'a) -> 'a -> t
end


(** A handler for dispatching queries to one of many services. *)
module Handler : sig
  type t
  val create : Configured.t list -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end
