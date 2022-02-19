open Base

let greeting = "Hello World"

let%test "size" =
  String.length greeting = 11
