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

(* $MDX part-end *)
p
(* $MDX part-begin=z_zz *)
let () =
  print_string s
;;

(* $MDX part-end *)
(* $MDX part-begin=4-2 *)

let () =
  f x print_int;
(* $MDX part-end *)
  ()

let () =
(* $MDX part-begin=indented *)
  let () = fooooooooooooooooooooooooooooooooooooooooooo in
  if not fooooooooo then foooooooooooo  
(* $MDX part-end *)
