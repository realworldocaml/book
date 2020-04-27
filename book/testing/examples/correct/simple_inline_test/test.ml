open! Base

let%test "rev" =
  List.equal Int.equal (List.rev [3;2;1]) [1;2;3]
