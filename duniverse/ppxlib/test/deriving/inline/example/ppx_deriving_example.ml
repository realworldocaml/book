type t = A [@@deriving_inline foo]

include struct
  [@@@ocaml.warning "-60"]

  let _ = fun (_ : t) -> ()

  module Foo = struct end

  let _ =
    ();
    ();
    [%foo]
end [@@ocaml.doc "@inline"]

[@@@deriving.end]
