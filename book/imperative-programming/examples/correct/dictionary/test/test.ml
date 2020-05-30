open! Core

let to_list d =
  let l = ref [] in
  Dictionary.iter d ~f:(fun ~key ~data -> l := (key,data) :: !l);
  List.rev !l

let%expect_test "" =
  let d = Dictionary.create ~hash:Int.hash ~equal:Int.equal in
  let p () = print_s [%sexp (to_list d : (int * string) list)] in
  p ();
  [%expect{| () |}];
  Dictionary.add d ~key:1 ~data:"foo";
  p ();
  [%expect{| ((1 foo)) |}];
  Dictionary.add d ~key:2 ~data:"bar";
  p ();
  [%expect{| ((2 bar) (1 foo)) |}];
  Dictionary.add d ~key:2 ~data:"snoo";
  p ();
  [%expect{| ((2 snoo) (1 foo)) |}]
