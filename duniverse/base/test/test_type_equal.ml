open! Import
open! Type_equal

let%expect_test "[Id.sexp_of_t]" =
  let id = Id.create ~name:"some-type-id" [%sexp_of: unit] in
  print_s [%sexp (id : _ Id.t)];
  [%expect {|
    some-type-id |}]
;;

let%test_module "Type_equal.Id" =
  (module struct
    open Type_equal.Id

    let t1 = create ~name:"t1" [%sexp_of: _]
    let t2 = create ~name:"t2" [%sexp_of: _]

    let%test _ = same t1 t1
    let%test _ = not (same t1 t2)
    let%test _ = Option.is_some (same_witness t1 t1)
    let%test _ = Option.is_none (same_witness t1 t2)
    let%test_unit _ = ignore (same_witness_exn t1 t1 : (_, _) Type_equal.equal)
    let%test _ = Result.is_error (Result.try_with (fun () -> same_witness_exn t1 t2))

    let%expect_test "to_sexp allocation" =
      require_no_allocation [%here] (fun () -> ignore (to_sexp t1 : 'a -> Sexp.t))
    ;;
  end)
;;

(* This test shows that we need [conv] even though [Type_equal.T] is exposed. *)
let%test_module "Type_equal" =
  (module struct
    open Type_equal

    let id = Id.create ~name:"int" [%sexp_of: int]

    module A : sig
      type t

      val id : t Id.t
    end = struct
      type t = int

      let id = id
    end

    module B : sig
      type t

      val id : t Id.t
    end = struct
      type t = int

      let id = id
    end

    let _a_to_b (a : A.t) =
      let eq = Id.same_witness_exn A.id B.id in
      (conv eq a : B.t)
    ;;

    (* the following is rejected by the compiler *)
    (* let _a_to_b (a : A.t) =
     *   let T = Id.same_witness_exn A.id B.id in
     *   (a : B.t)
    *)

    module C = struct
      type 'a t
    end

    module Liftc = Lift (C)

    let _ac_to_bc (ac : A.t C.t) =
      let eq = Liftc.lift (Id.same_witness_exn A.id B.id) in
      (conv eq ac : B.t C.t)
    ;;
  end)
;;
