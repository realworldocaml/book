module Conv          = Sexplib0.Sexp_conv
module Conv_error    = Sexplib0.Sexp_conv_error
module Lazy_group_id = Sexplib0.Private.Lazy_group_id
module Sexp          = Sexplib0.Sexp
module Sexpable      = Sexplib0.Sexpable

module Option = struct
  type 'a t = 'a option =
    | None
    | Some of 'a
end
