module Bisect_visit___expr_polyrec___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000$\000\000\000\t\000\000\000!\000\000\000!\b\000\000 \000\160_B\160lA\160u@\160\000FG\160\000gE\160\000vD\160\001\000\129C\160\001\000\139F" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_polyrec.ml" ~point_count:8
          ~point_definitions in
      cb
  end
open Bisect_visit___expr_polyrec___ml
let rec f : 'a . 'a -> unit =
  ___bisect_visit___ 2;
  (fun _ -> ___bisect_visit___ 1; f 0; ___bisect_visit___ 0; f "")
let () =
  ___bisect_visit___ 7;
  (let rec f : 'a . 'a -> unit =
     ___bisect_visit___ 5;
     (fun _ -> ___bisect_visit___ 4; f 0; ___bisect_visit___ 3; f "") in
   ___bisect_visit___ 6; f 0)
