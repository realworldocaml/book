module type Service = sig
  module Config : Sexpable

  module Request : Sexpable
  module Response : Sexpable

  module State : sig
    type t
    val create : Config.t -> t Or_error.t
    val update : t -> Request.t -> unit Or_error.t
  end
end
