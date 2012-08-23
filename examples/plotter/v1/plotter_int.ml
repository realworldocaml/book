module Source = struct
  module type S = sig
    type config with sexp
    type t

    val create : config -> t
    val read : t -> (Time.t * float) Pipe.Reader.t Or_error.t
  end

  type 'a t = (module S with type config = 'a)
end

module Plotter = struct
  module type S = sig
    type config with sexp
    type t
    val create : config -> t Deferred.t
    val extend : t -> Time.t -> float -> unit Deferred.t
  end

  type 'a t = (module S with type config = 'a)

  module type Configured = sig
    module Plotter : S
    val config : Plotter.config
  end

  type configured = (module Configured)

  val configure : 'a t -> 'a -> (module Configured)

end

val render_sources : (module Plotter.Configured) list -> Source.t list -> ....
