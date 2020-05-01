
(** The [Make] functor glues together a {! Cohttp.S.IO } implementation
    to send requests down a connection that is established by the  {! Net }
    module.  The resulting module satisfies the {! Client } module type. *)

module Make (IO:S.IO) (Net:S.Net with module IO = IO) : S.Client
  with type ctx = Net.ctx
