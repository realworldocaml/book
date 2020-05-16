open! Import

let%expect_test "[sexp_array]" =
  let module M = struct
    type t = { x : int sexp_array [@ocaml.warning "-3"] } [@@deriving sexp_of]
  end
  in
  List.iter [ [||]; [| 13 |] ] ~f:(fun x -> print_s [%sexp ({ x } : M.t)]);
  [%expect {|
    ()
    ((x (13))) |}]
;;

let%expect_test "[sexp_list]" =
  let module M = struct
    type t = { x : int sexp_list [@ocaml.warning "-3"] } [@@deriving sexp_of]
  end
  in
  List.iter [ []; [ 13 ] ] ~f:(fun x -> print_s [%sexp ({ x } : M.t)]);
  [%expect {|
    ()
    ((x (13))) |}]
;;

let%expect_test "[sexp_opaque]" =
  let module M = struct
    type t = { x : int sexp_opaque [@ocaml.warning "-3"] } [@@deriving sexp_of]
  end
  in
  print_s [%sexp ({ x = 13 } : M.t)];
  [%expect {|
    ((x <opaque>)) |}]
;;

let%expect_test "[sexp_option]" =
  let module M = struct
    type t = { x : int sexp_option [@ocaml.warning "-3"] } [@@deriving sexp_of]
  end
  in
  List.iter [ None; Some 13 ] ~f:(fun x -> print_s [%sexp ({ x } : M.t)]);
  [%expect {|
    ()
    ((x 13)) |}]
;;
