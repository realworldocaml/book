open! Core_kernel

let%test "rev" =
  List.equal ~equal:Int.equal (List.rev [3;2;1]) [1;2;3]
