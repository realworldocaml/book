let () =
  ();
  ()
;;

[@@@part "toto"];;

let x = 34
let f = 42.3
let s = "toto"
let f x u = u x

let () =
  print_int x;
  print_float f
;;

[@@@part "zzz"];;

let () =
  print_string s
;;

[@@@part "42"];;

let () =
  f x print_int
