type t = A [@@deriving_inline foo]
let _ = fun (_ : t) -> ()
let _ = [%foo ]
[@@@deriving.end]
