
(* Module type for a query specification *)
module Spec = struct
  module type S = sig
    type query   with sexp
    type response with sexp
    type t

    val description : t -> string
    val examples : (query * response) list
  end

  type ('a,'b) t = (module S with type query = 'a
                             and  type response = 'b)
end

module Implementation : sig
  type 'env t
  val create : ('a,'b) Spec.t -> ('env -> 'a -> 'b) -> 'env t
  val
end


module Bundle : sig
  type
end
