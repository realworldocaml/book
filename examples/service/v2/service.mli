open Core.Std

(** The module type for a service. *)
module type S = sig
  type t
  type config with sexp

  val create         : config -> t
  val name           : string
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

type 'a t = (module S with type config = 'a)

(** Bundles multiple services together *)
module Bundle : sig
  type 'a service = 'a t
  type t

  val create         : unit -> t
  val register       : t -> 'a service -> 'a -> unit Or_error.t
  val unregister     : t -> string -> unit
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end
