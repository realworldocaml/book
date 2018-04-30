open! Base

let%test "rev" =
  List.equal ~equal:Int.equal (List.rev [3;2;1]) [3;2;1]
