open! Core_kernel
open Poly
open! Import
open! Univ
module Id = Type_equal.Id

let%test_module _ =
  (module struct
    let c1 = Id.create ~name:"c1" Int.sexp_of_t
    let c2 = Id.create ~name:"c2" Int.sexp_of_t
    let t1 = create c1 13
    let t2 = create c2 13

    let%test_unit _ = ignore ([%sexp_of: _ Id.t] c1 : Sexp.t)
    let%test_unit _ = ignore ([%sexp_of: t] t1 : Sexp.t)
    let%test _ = type_id_name t1 = Id.name c1
    let%test _ = does_match t1 c1
    let%test _ = not (does_match t1 c2)
    let%test _ = not (does_match t2 c1)
    let%test _ = does_match t2 c2

    let%test _ =
      match match_ t1 c1 with
      | None -> false
      | Some v -> v = 13
    ;;

    let%test _ = Option.is_none (match_ t1 c2)
    let%test _ = match_exn t1 c1 = 13
    let%test _ = Result.is_error (Result.try_with (fun () -> match_exn t1 c2))
  end)
;;
