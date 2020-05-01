[@@@ocaml.text "/*"]
module Bisect_visit___attributes___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000J\000\000\000\016\000\000\000=\000\000\000=\b\000\000<\000\160uN\160\127L\160\000[M\160\000{J\160\001\000\134K\160\001\000\152H\160\001\000\164I\160\001\000\202E\160\001\000\213F\160\001\000\246G\160\001\001\014C\160\001\001\019D\160\001\001\028B\160\001\001BA\160\001\001J@" in
      let `Staged cb =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "attributes.ml" ~point_count:15 ~point_definitions in
      cb
  end
open Bisect_visit___attributes___ml
[@@@ocaml.text "/*"]
let () =
  (let ___bisect_result___ = ((let x = 1 in x)[@testing ]) |> ignore in
   ___bisect_visit___ 14; ___bisect_result___);
  (let ___bisect_result___ =
     ((fun x -> ___bisect_visit___ 12; x)[@testing ]) |> ignore in
   ___bisect_visit___ 13; ___bisect_result___);
  (let ___bisect_result___ =
     (let ___bisect_result___ = ((string_of_int 0)[@testing ]) in
      ___bisect_visit___ 10; ___bisect_result___) |> ignore in
   ___bisect_visit___ 11; ___bisect_result___);
  (((match 0 with
     | 0 -> (___bisect_visit___ 8; ())
     | _ -> (___bisect_visit___ 9; ())))
  [@testing ]);
  (let ___bisect_result___ =
     ((function
       | 0 -> (___bisect_visit___ 5; 0)
       | x -> (___bisect_visit___ 6; x))[@testing ]) |> ignore in
   ___bisect_visit___ 7; ___bisect_result___);
  (((try
       let ___bisect_result___ =
         (let ___bisect_result___ = string_of_int 0 in
          ___bisect_visit___ 3; ___bisect_result___) |> ignore in
       ___bisect_visit___ 4; ___bisect_result___
     with | _ -> (___bisect_visit___ 2; ())))
  [@testing ]);
  ((if true then (___bisect_visit___ 1; ()) else (___bisect_visit___ 0; ()))
  [@testing ])
