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

let%expect_test "specified key module" =
  (* incorrect use *)
  let module Key_incorrect = struct
    type _ t =
      | Foo : int t
      | Bar : string t
    [@@deriving sexp_of]

    let to_type_id (type a) (t : a t) : a Type_equal.Id.t =
      match t with
      | Foo -> Type_equal.Id.create ~name:"foo" [%sexp_of: int]
      | Bar -> Type_equal.Id.create ~name:"bar" [%sexp_of: string]
    ;;
  end
  in
  let module U_incorrect =
    Make
      (Key_incorrect)
      (struct
        type 'a t = 'a [@@deriving sexp_of]
      end)
  in
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         U_incorrect.find
           (U_incorrect.of_alist_exn [ T (Foo, 3); T (Bar, "three") ])
           Foo)
       : int option Or_error.t)];
  [%expect
    {|
    (Error (
      "[Key.to_type_id] must not provide different type ids when called on the same input"
      (key Foo)
      (type_id1 ((name foo) (uid <uid>)))
      (type_id2 ((name foo) (uid <uid>))))) |}];
  (* correct use *)
  let module Key_correct = struct
    type _ t =
      | Foo : int t
      | Bar : string t
    [@@deriving sexp_of]

    let foo_id = Type_equal.Id.create ~name:"foo" [%sexp_of: int]
    let bar_id = Type_equal.Id.create ~name:"bar" [%sexp_of: string]

    let to_type_id (type a) (t : a t) : a Type_equal.Id.t =
      match t with
      | Foo -> foo_id
      | Bar -> bar_id
    ;;
  end
  in
  let module U_correct =
    Make
      (Key_correct)
      (struct
        type 'a t = 'a [@@deriving sexp_of]
      end)
  in
  print_s
    [%sexp
      (U_correct.find (U_correct.of_alist_exn [ T (Foo, 3); T (Bar, "three") ]) Foo
       : int option)];
  [%expect {| (3) |}]
;;

let%expect_test "merge" =
  let module Key = struct
    type _ t =
      | Foo : int t
      | Bar : string t
      | Baz : char t
    [@@deriving sexp_of]

    let foo_id = Type_equal.Id.create ~name:"foo" [%sexp_of: int]
    let bar_id = Type_equal.Id.create ~name:"bar" [%sexp_of: string]
    let baz_id = Type_equal.Id.create ~name:"baz" [%sexp_of: char]

    let to_type_id (type a) (t : a t) : a Type_equal.Id.t =
      match t with
      | Foo -> foo_id
      | Bar -> bar_id
      | Baz -> baz_id
    ;;
  end
  in
  let module Input_data1 = struct
    type (_, 'a) t = 'a option [@@deriving sexp_of]
  end
  in
  let module Input1 = Make1 (Key) (Input_data1) in
  let module Input_data2 = struct
    type (_, 'a) t = 'a list [@@deriving sexp_of]
  end
  in
  let module Input2 = Make1 (Key) (Input_data2) in
  let module Output_data = struct
    type ('s, 'a) t =
      { key : 'a Key.t
      ; merge_result :
          [ `Left of ('s, 'a) Input_data1.t
          | `Right of ('s, 'a) Input_data2.t
          | `Both of ('s, 'a) Input_data1.t * ('s, 'a) Input_data2.t
          ]
      }
    [@@deriving sexp_of]
  end
  in
  let module Output = Make1 (Key) (Output_data) in
  let module Merge = Merge (Key) (Input_data1) (Input_data2) (Output_data) in
  let merged =
    Merge.merge
      (Input1.of_alist_exn [ T (Foo, Some 3); T (Bar, Some "three") ])
      (Input2.of_alist_exn [ T (Foo, [ 4; 5; 6 ]); T (Baz, [ 'a'; 'b'; 'c' ]) ])
      ~f:{ f = (fun ~key merge_result -> Some { key; merge_result }) }
  in
  print_s [%sexp (merged : _ Output.t)];
  [%expect
    {|
    ((bar ((key Bar) (merge_result (Left (three)))))
     (baz ((key Baz) (merge_result (Right (a b c)))))
     (foo ((key Foo) (merge_result (Both ((3) (4 5 6))))))) |}]
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
