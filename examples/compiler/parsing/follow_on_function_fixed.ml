(* follow_on_function_fixed.ml *)
let concat_and_print x y =
  let v = x ^ y in
  print_endline v;
  v

let add_and_print x y =
  let v = x + y in
  print_endline (string_of_int v);
  v

let _ =
  let _ = add_and_print 1 2 in
  let _ = concat_and_print "a" "b" in
  ()
