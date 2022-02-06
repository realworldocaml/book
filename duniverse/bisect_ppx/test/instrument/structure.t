An empty file. Show the bare registration code.

  $ bash test.sh --include-registration <<'EOF'
  > 
  > EOF
  module Bisect_visit___test___ml = struct
    let ___bisect_visit___ =
      let points = [||] in
      let (`Visit visit) =
        Bisect.Runtime.register_file ~bisect_file:None ~bisect_silent:None
          ~filename:"test.ml" ~points ~bisect_sigterm:false
      in
      visit
  
    let ___bisect_post_visit___ point_index result =
      ___bisect_visit___ point_index;
      result
  end
  
  open Bisect_visit___test___ml
  
  [@@@ocaml.text "/*"]
