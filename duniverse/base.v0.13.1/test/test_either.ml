open! Import

type t = (int, string) Either.t [@@deriving sexp_of]

let f : t = First 0
let s : t = Second "str"

let%expect_test "First.Monad.map" =
  let open Either.First.Let_syntax in
  let inc x =
    let%map v = x in
    v + 1
  in
  let f' = inc f in
  let s' = inc s in
  print_s [%message (f' : t) (s' : t)];
  [%expect {|
    ((f' (First  1))
     (s' (Second str))) |}]
;;

let%expect_test "Second.Monad.map" =
  let open Either.Second.Let_syntax in
  let add x =
    let%map v = x in
    String.concat [ v; "1" ]
  in
  let f' = add f in
  let s' = add s in
  print_s [%message (f' : t) (s' : t)];
  [%expect {|
    ((f' (First  0))
     (s' (Second str1))) |}]
;;

let%expect_test "First.Monad.bind" =
  let open Either.First.Let_syntax in
  let inc x =
    let%bind v = x in
    return (v + 1)
  in
  let f' = inc f in
  let s' = inc s in
  print_s [%message (f' : t) (s' : t)];
  [%expect {|
    ((f' (First  1))
     (s' (Second str))) |}]
;;

let%expect_test "Second.Monad.bind" =
  let open Either.Second.Let_syntax in
  let add x =
    let%bind v = x in
    return (String.concat [ v; "1" ])
  in
  let f' = add f in
  let s' = add s in
  print_s [%message (f' : t) (s' : t)];
  [%expect {|
    ((f' (First  0))
     (s' (Second str1))) |}]
;;
