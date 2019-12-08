(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

let measure runs library source format f =
  let name = Printf.sprintf "%s: %s (%s)" library source format in

  let rec run = function
    | 0 -> ()
    | n -> f (); run (n - 1)
  in

  let start_time = Unix.gettimeofday () in

  run runs;

  let duration = (Unix.gettimeofday ()) -. start_time in
  let average = duration /. (float_of_int runs) *. 1000000. in

  Printf.printf "  %s: %.0f us\n" name average

let google_page = "test/pages/google"
let xml_spec = "test/pages/xml_spec"
