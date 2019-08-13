module Bisect_visit___blob___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000\012\000\000\000\004\000\000\000\r\000\000\000\r\176\160L@\160\000EB\160\000ZA"
         in
      let point_state = Array.make 3 0  in
      Bisect.Runtime.register_file "blob.ml" point_state point_definitions;
      (fun point_index  ->
         let current_count = point_state.(point_index)  in
         point_state.(point_index) <-
           (if current_count < Pervasives.max_int
            then Pervasives.succ current_count
            else current_count))
      
  end
open Bisect_visit___blob___ml
let me () =
  ___bisect_visit___ 0;
  Printf.printf "ME: %s\n"
    "let me () = Printf.printf \"ME: %s\\n\" [%blob \"blob.ml\"]\n\nlet me' () = print_endline \"foo\"; [%blob \"blob.ml\"]\n"
  
let me' () =
  ___bisect_visit___ 2;
  print_endline "foo";
  ___bisect_visit___ 1;
  "let me () = Printf.printf \"ME: %s\\n\" [%blob \"blob.ml\"]\n\nlet me' () = print_endline \"foo\"; [%blob \"blob.ml\"]\n"
  
