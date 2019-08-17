
(** The [Make] functor glues together a {! Cohttp.S.IO } implementation
    to send requests down a connection that is established by the  {! Net }
    module.  The resulting module satisfies the {! Server } module type. *)
module Make (IO:S.IO) : S.Server with module IO = IO
