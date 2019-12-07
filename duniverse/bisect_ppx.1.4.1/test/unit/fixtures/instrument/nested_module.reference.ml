module Bisect_visit___nested_module___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\n\000\000\000\004\000\000\000\r\000\000\000\r\176\160H@\160iA\160~B" in
      let `Staged cb =
        Bisect.Runtime.register_file "nested_module.ml" ~point_count:3
          ~point_definitions in
      cb
  end
open Bisect_visit___nested_module___ml
let x = ___bisect_visit___ 0; 3
module F = struct let y x = ___bisect_visit___ 1; x + 4 end
let z x = ___bisect_visit___ 2; x + 5
