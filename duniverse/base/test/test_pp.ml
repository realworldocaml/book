open! Import

let to_string pp v =
  pp Caml.Format.str_formatter v;
  Caml.Format.flush_str_formatter ()
;;

let print pp v = Caml.Printf.printf "%s\n" (to_string pp v)
let print_all pp vs = List.iter ~f:(print pp) vs

let%expect_test "pretty-printers" =
  print_all Char.pp [ '\000'; '\r'; 'a' ];
  [%expect {|
    '\000'
    '\r'
    'a' |}];
  print_all String.pp [ ""; "foo"; "abc\tdef" ];
  [%expect {|
    ""
    "foo"
    "abc\tdef" |}];
  print_all Sign.pp Sign.all;
  [%expect {|
    Neg
    Zero
    Pos |}];
  print_all Bool.pp Bool.all;
  [%expect {|
    false
    true |}];
  print_all Unit.pp Unit.all;
  [%expect {| () |}];
  print_all Nothing.pp Nothing.all;
  [%expect {| |}];
  print_all Float.pp [ 0.; 3.14; 1.0 /. 0.0 ];
  [%expect {|
    0.
    3.14
    inf |}];
  print_all Int.pp [ 0; 1 ];
  [%expect {|
    0
    1 |}];
  print Info.pp (Info.create_s [%sexp "hello", "world"]);
  [%expect {| (hello world) |}]
;;
