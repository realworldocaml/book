module type Service : sig
  type t
  val name : string
  val create : unit -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end
