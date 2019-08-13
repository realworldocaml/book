open! Import
open! Lazy

let%test_unit _ =
  let r = ref 0 in
  let t = return () >>= fun () -> Int.incr r; return () in
  assert (!r = 0);
  force t;
  assert (!r = 1);
  force t;
  assert (!r = 1)
;;

let%test_unit _ =
  let r = ref 0 in
  let t = return () >>= fun () -> lazy (Int.incr r) in
  assert (!r = 0);
  force t;
  assert (!r = 1);
  force t;
  assert (!r = 1)
;;

let%test_module _ =
  (module struct

    module M1 = struct
      type nonrec t = { x : int t } [@@deriving sexp_of]
    end

    module M2 = struct
      type t = { x : int T_unforcing.t } [@@deriving sexp_of]
    end

    let%test_unit _ =
      let v = lazy 42 in
      let (_ : int) =
        (* no needed, but the purpose of this test is not to test this compiler
           optimization *)
        force v
      in
      assert (is_val v);
      let t1 = { M1. x = v } in
      let t2 = { M2. x = v } in
      assert (Sexp.equal (M1.sexp_of_t t1) (M2.sexp_of_t t2))
    ;;

    let%test_unit _ =
      let t1 = { M1. x = lazy (40 + 2) } in
      let t2 = { M2. x = lazy (40 + 2) } in
      assert (not (Sexp.equal (M1.sexp_of_t t1) (M2.sexp_of_t t2)));
      assert (is_val t1.x);
      assert (not (is_val t2.x))
    ;;
  end)
