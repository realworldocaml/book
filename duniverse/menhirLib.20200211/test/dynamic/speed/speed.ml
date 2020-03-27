open Printf

(* Read one floating-point number in a file. *)
let read f =
  let c = open_in f in
  let s = input_line c in
  close_in c;
  float_of_string s

let code =
  read "src/code.time"
let table =
  read "src/table.time"
let ocamlyacc =
  read "src/ocamlyacc.time"

let () =
  printf "Parsing with the code back-end takes %.2f seconds.\n" code;
  printf "Parsing with the table back-end takes %.2f seconds.\n" table;
  printf "Parsing with ocamlyacc takes %.2f seconds.\n" ocamlyacc;
  printf "The table back-end is %.1f times slower than the code back-end.\n" (table /. code);
  printf "ocamlyacc is %.1f times slower than the code back-end.\n" (ocamlyacc /. code);
  printf "ocamlyacc is %.1f times faster than the table back-end.\n" (table /. ocamlyacc);
  flush stdout
