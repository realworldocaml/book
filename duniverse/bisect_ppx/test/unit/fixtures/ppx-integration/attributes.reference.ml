module Bisect_visit___attributes___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000d\000\000\000\023\000\000\000Y\000\000\000Y\b\000\000X\000\160KU\160TQ\160YR\160jS\160uT\160\127N\160\000PO\160\000[P\160\000{L\160\001\000\134M\160\001\000\152I\160\001\000\164J\160\001\000\188K\160\001\000\202E\160\001\000\213F\160\001\000\235G\160\001\000\246H\160\001\001\014B\160\001\001\028C\160\001\0014D\160\001\001BA\160\001\001J@" in
      let `Staged cb =
        Bisect.Runtime.register_file "attributes.ml" ~point_count:22
          ~point_definitions in
      cb
  end
open Bisect_visit___attributes___ml
let () =
  ___bisect_visit___ 21;
  ((let x = ___bisect_visit___ 17; 1 in ___bisect_visit___ 18; x)[@testing ])
    |> ((___bisect_visit___ 19; ignore));
  ___bisect_visit___ 20;
  ((fun x -> ___bisect_visit___ 14; x)[@testing ]) |>
    ((___bisect_visit___ 15; ignore));
  ___bisect_visit___ 16;
  ((string_of_int 0)[@testing ]) |> ((___bisect_visit___ 12; ignore));
  ___bisect_visit___ 13;
  (((match 0 with
     | 0 -> (___bisect_visit___ 9; ())
     | _ -> (___bisect_visit___ 10; ())))
  [@testing ]);
  ___bisect_visit___ 11;
  ((function
    | 0 -> (___bisect_visit___ 5; 0)
    | x -> (___bisect_visit___ 6; x))[@testing ]) |>
    ((___bisect_visit___ 7; ignore));
  ___bisect_visit___ 8;
  (((try (string_of_int 0) |> (___bisect_visit___ 2; ignore)
     with | _ -> (___bisect_visit___ 3; ())))
  [@testing ]);
  ___bisect_visit___ 4;
  ((if true then (___bisect_visit___ 1; ()) else (___bisect_visit___ 0; ()))
  [@testing ])
