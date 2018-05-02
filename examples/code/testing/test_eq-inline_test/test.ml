open! Base

let%test_eq "rev" =
  [%test_eq: int list] (List.rev [3;2;1]) [3;2;1]
