open! Import
include Identifiable_intf

module Make (T : Arg) = struct
  include T
  include Comparable.Make (T)
  include Pretty_printer.Register (T)

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
end

module Make_using_comparator (T : Arg_with_comparator) = struct
  include T
  include Comparable.Make_using_comparator (T)
  include Pretty_printer.Register (T)

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
end
