open Core.Std

(** The module type for a service. *)
module type S = sig
  type t

  val name           : string
  val create         : unit -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

(** A handler for dispatching queries to one of many services. *)
module Handler : sig
  type t
  val create : (module S) list -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end
