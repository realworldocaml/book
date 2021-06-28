open! Core_kernel
open Poly
open! Import
open! Univ_map

let%test_module _ =
  (module struct
    let size = Key.create ~name:"size" Int.sexp_of_t
    let name = Key.create ~name:"name" String.sexp_of_t
    let foo = Key.create ~name:"foo" Float.sexp_of_t
    let kids = Key.create ~name:"kids" (List.sexp_of_t sexp_of_t)

    let%test _ = is_empty empty

    let test_contains t k v =
      assert (not (is_empty t));
      assert (mem t k);
      (* these do not raise *)
      ignore (change_exn t k ~f:Fn.id : t);
      ignore
        (change t k ~f:(function
           | None -> assert false
           | o -> o)
         : t);
      match find t k with
      | None -> assert false
      | Some v' -> assert (phys_equal v v')
    ;;

    let test_add t k v = test_contains (set t k v) k v

    let test_find t k =
      let f1 = find t k in
      let f2 = Option.try_with (fun () -> find_exn t k) in
      match f1, f2 with
      | None, None -> ()
      | Some v1, Some v2 -> assert (phys_equal v1 v2)
      | Some _, None -> assert false
      | None, Some _ -> assert false
    ;;

    let test_change t k v =
      let t_minus = change t k ~f:(fun _ -> None) in
      assert (not (mem t_minus k));
      let t_plus = change t k ~f:(fun _ -> Some v) in
      test_contains t_plus k v;
      ()
    ;;

    let test_remove t k v =
      let t_minus = remove t k in
      assert (not (mem t_minus k));
      let t_plus = set t k v in
      test_contains t_plus k v;
      let t_minus = remove t_plus k in
      assert (not (mem t_minus k))
    ;;

    let test_remove_by_id t k v =
      let t_minus = remove_by_id t (Key.uid k) in
      assert (not (mem t_minus k));
      let t_plus = set t k v in
      test_contains t_plus k v;
      let t_minus = remove_by_id t_plus (Key.uid k) in
      assert (not (mem t_minus k))
    ;;

    let test t =
      (* add *)
      test_add t size 12;
      test_add t name "hank";
      test_add t kids [ t; empty ];
      (* find *)
      test_find t size;
      test_find t name;
      test_find t kids;
      (* change *)
      test_change t size 33;
      test_change t name "frank";
      test_change t kids [];
      (* remove *)
      test_remove t size 33;
      test_remove t name "frank";
      test_remove t kids [];
      (* remove_by_id *)
      test_remove_by_id t size 33;
      test_remove_by_id t name "frank";
      test_remove_by_id t kids [];
      ()
    ;;

    let t0 = empty
    let t1 = set t0 size 9
    let t2 = set t1 foo 13.25
    let t3 = set t2 size 15

    let%test_unit _ = test t0
    let%test_unit _ = test t1
    let%test_unit _ = test t2
    let%test_unit _ = test t3
    let%test _ = sexp_of_t t3 = Sexp.of_string "((foo 13.25)(size 15))"
  end)
;;

module With_default = struct
  open! With_default

  let%test_unit _ =
    let key = Key.create ~default:0 ~name:"default 0" Int.sexp_of_t in
    assert (find empty key = 0);
    let t = set empty key 1 in
    assert (find t key = 1);
    let t = set empty key 2 in
    assert (find t key = 2);
    let t = change t key ~f:( ~- ) in
    assert (find t key = -2)
  ;;

  let%test _ =
    let key = Key.create ~default:1 ~name:"default 1" Int.sexp_of_t in
    find (change empty key ~f:( ~- )) key = -1
  ;;
end

module With_fold = struct
  open! With_fold

  let%test_unit _ =
    let key = Key.create ~init:5 ~f:( + ) ~name:"init 5" Int.sexp_of_t in
    assert (find empty key = 5);
    let t = add empty key 3 in
    assert (find t key = 8);
    let t = change t key ~f:( ~- ) in
    assert (find t key = -8)
  ;;

  let%test_unit _ =
    let key =
      Key.create ~init:0 ~f:(fun _ -> assert false) ~name:"don't fold this" Int.sexp_of_t
    in
    assert (find empty key = 0);
    let t = set empty key 1 in
    assert (find t key = 1);
    let t = change t key ~f:( ~- ) in
    assert (find t key = -1)
  ;;
end

module Multi = struct
  open! Multi

  let%test_unit _ =
    let key = Key.create ~name:"int list" Int.sexp_of_t in
    assert (find empty key = []);
    let t = add empty key 1 in
    assert (find t key = [ 1 ]);
    let t = set t key [ 2; 3 ] in
    assert (find t key = [ 2; 3 ]);
    let t = change t key ~f:(List.map ~f:( ~- )) in
    assert (find t key = [ -2; -3 ])
  ;;
end
