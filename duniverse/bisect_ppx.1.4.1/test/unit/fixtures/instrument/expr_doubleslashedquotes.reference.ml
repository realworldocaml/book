module Bisect_visit___expr_doubleslashedquotes___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\011\000\000\000\004\000\000\000\r\000\000\000\r\176\160oB\160|@\160\000SA" in
      let `Staged cb =
        Bisect.Runtime.register_file "expr_doubleslashedquotes.ml"
          ~point_count:3 ~point_definitions in
      cb
  end
open Bisect_visit___expr_doubleslashedquotes___ml
type t =
  | Anthony 
  | Caesar 
let message =
  ___bisect_visit___ 2;
  (function
   | Anthony -> (___bisect_visit___ 0; "foo\\")
   | Caesar -> (___bisect_visit___ 1; "\\bar"))
