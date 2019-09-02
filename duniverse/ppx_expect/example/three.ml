
(* The idea behind this sequence of examples is as follows. Starting with the same [text],
   We explore various [%expect] declarations which match it. *)

let%expect_test _ =
  let text_no_final_nl() = print_string "one\ntwo\nthree" in

  text_no_final_nl(); [%expect {|
  one
  two
  three|}];

  let text() = print_string "one\ntwo\nthree\n" in

  (* Base example *)
  text(); [%expect {|
  one
  two
  three
|}];

  (* ok to omit space between "expect" and "{" *)
  text(); [%expect {|
  one
  two
  three
|}];

  (* indentation allowed *)
  text(); [%expect {|
  one
  two
  three
|}];

  text(); [%expect {|
  one
  two   (literal)
  three
|}];

  text(); [%expect {|
  one
  two   (regexp)
  three
|}];

  text(); [%expect {|
  one
  t.o   (regexp)
  three
|}];

  text(); [%expect {|
  one
  t.*o  (regexp)
  three
|}];

  text(); [%expect {|
  one
  t[wxy]o (regexp)
  three
|}]
