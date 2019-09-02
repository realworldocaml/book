open Import
open Parsexp

let assoc s =
  Lexer.assoc (Eager.Lexbuf_consumer.create ()) (Lexing.from_string s)
  |> [%sexp_of: (string * Sexp.t) list]
  |> print_s

let sexps s =
  let lexbuf = Lexing.from_string s in
  let p = Eager.Lexbuf_consumer.create () in
  let rec loop () =
    match Eager.Lexbuf_consumer.parse_opt p lexbuf with
    | None -> []
    | Some sexp -> sexp :: loop ()
  in
  loop () |> List.iter ~f:print_s

let%expect_test "simple test" =
  assoc {|
# Hello
x = (a b c)
y = 42
z = "blah"
a = 123
|};
  [%expect {|
    ((x (a b c))
     (y 42)
     (z blah)
     (a 123)) |}]

let%expect_test "empty lexing" =
  sexps "";
  [%expect {| |}]

let%expect_test "the lexer doesn't consume more than it should" =
  sexps {| abc"123" |};
  [%expect {|
    abc
    123
  |}];
  sexps {| abc() |};
  [%expect {|
    abc
    ()
  |}]

