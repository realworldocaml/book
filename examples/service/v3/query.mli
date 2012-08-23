module Type : sig
  type t with sexp
  val uconstr : t Univ.Constr.t
end

(* Module type for a query specification *)
module Spec : sig
  module type S = sig
    module Query : Type
    module Response : Type
    type t

    val name : string
    val description : t -> string
    val examples : (query * response) list
  end

  type ('a,'b) t = (module S with type query = 'a
                             and  type response = 'b)
end

module Implementation : sig
  type 'env t
  val create : ('a,'b) Spec.t -> ('env -> 'a -> 'b) -> 'env t

  (** [lift query extract_env] Takes a query against one environment, and lifts it to
      another environment from which the needed environment can be extracted, using
      [extract_env] *)
  val lift : 'env t -> ('source_env -> 'env) -> 'source_env t
end

module Bundle : sig
  type 'env t

  val create : unit -> 'env t
  val register   : 'env t -> 'env Implementation.t -> unit Or_error.t
  val unregister : 'env t -> name:string -> unit Or_Error.t

  val handle_query : 'env t -> 'env -> Sexp.t -> Sexp.t Or_error.t
end
