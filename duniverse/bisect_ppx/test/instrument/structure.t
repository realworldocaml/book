An empty file. Show the bare registration code.

  $ bash test.sh --include-registration <<'EOF'
  > 
  > EOF
  module Bisect_visit___test___ml = struct
    let ___bisect_visit___ =
      let point_definitions =
        "132149166190000000000001000000000000000000000000000000000000128"
      in
      let (`Staged cb) =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          "test.ml" ~point_count:0 ~point_definitions
      in
      cb
  
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index;
      result
  end
  
  open Bisect_visit___test___ml
  
  [@@@ocaml.text "/*"]
