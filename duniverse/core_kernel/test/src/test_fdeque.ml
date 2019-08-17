open! Core_kernel
open! Import
open! Fdeque

let%test_unit _ =
  let open Front_to_back in
  [%test_result: int list] ~expect:[ 1; 2; 3 ] (to_list (of_list [ 1; 2; 3 ]))
;;

let%test_unit _ =
  let open Back_to_front in
  [%test_result: int list] ~expect:[ 1; 2; 3 ] (to_list (of_list [ 1; 2; 3 ]))
;;

let%test_unit _ =
  [%test_result: int list]
    ~expect:[ 1; 2; 3; 4 ]
    (bind (of_list [ [ 1; 2 ]; [ 3; 4 ] ]) ~f:of_list |> to_list)
;;

let%test_unit _ =
  [%test_result: int list]
    ~expect:[ 2; 3; 4; 5 ]
    (map (of_list [ 1; 2; 3; 4 ]) ~f:succ |> to_list)
;;

let%test_unit _ =
  let open Stable.V1 in
  [%test_result: int list]
    ~expect:[ 1; 2; 3 ]
    (to_list (t_of_sexp Int.t_of_sexp (sexp_of_t Int.sexp_of_t (of_list [ 1; 2; 3 ]))))
;;
