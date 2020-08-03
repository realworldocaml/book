let () =
  ();
  ()
;;

(* $MDX part-begin=toto *)
let x = 34
let f = 42.3
let s = "toto"
let f x u = u x

let () =
  print_int x;
  print_float f
;;

p
(* $MDX part-begin=z_zz *)
let () =
  print_string s
;;
