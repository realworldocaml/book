open! Core_kernel
open Poly
open! Bounded_int_table

(* test [exists{,i}], [for_all{,i}] *)
let%test_module _ =
  (module struct
    let of_list keys =
      let t = create ~num_keys:10 ~key_to_int:Fn.id ~sexp_of_key:Int.sexp_of_t () in
      List.iter keys ~f:(fun key -> add_exn t ~key ~data:key);
      t
    ;;

    let test_exists_like_function exists =
      exists (of_list []) ~f:(fun _ -> assert false) = false
      && exists (of_list [ 1 ]) ~f:(fun _ -> false) = false
      && exists (of_list [ 1 ]) ~f:(fun _ -> true) = true
      && exists (of_list [ 1 ]) ~f:(fun data -> data = 1) = true
      && exists (of_list [ 1; 2; 3 ]) ~f:(fun _ -> false) = false
      && exists (of_list [ 1; 2; 3 ]) ~f:(fun _ -> true) = true
      && exists (of_list [ 1; 2; 3 ]) ~f:(fun data -> data = 3) = true
    ;;

    let%test _ =
      test_exists_like_function (fun t ~f -> existsi t ~f:(fun ~key:_ ~data -> f data))
    ;;

    let%test _ = test_exists_like_function exists

    let%test _ =
      test_exists_like_function (fun t ~f ->
        not (for_alli t ~f:(fun ~key:_ ~data -> not (f data))))
    ;;

    let%test _ =
      test_exists_like_function (fun t ~f ->
        not (for_all t ~f:(fun data -> not (f data))))
    ;;

    let equal_of_list l1 l2 = equal Int.equal Int.equal (of_list l1) (of_list l2)

    let%test _ = equal_of_list [] [] = true
    let%test _ = equal_of_list [] [ 1 ] = false
    let%test _ = equal_of_list [ 1 ] [] = false
    let%test _ = equal_of_list [ 1 ] [ 1 ] = true
    let%test _ = equal_of_list [ 1 ] [ 1; 2 ] = false
    let%test _ = equal_of_list [ 1; 2 ] [ 1; 2 ] = true
    let%test _ = equal_of_list [ 1; 2 ] [ 2; 1 ] = true

    (* test [equal] between tables that have different [to_int] functions. *)
    let%test_unit _ =
      let of_list ~offset keys =
        let t = create ~num_keys:10 ~key_to_int:(fun i -> i + offset) () in
        List.iter keys ~f:(fun key -> add_exn t ~key ~data:key);
        t
      in
      let t0 = of_list [ 1; 2 ] ~offset:0 in
      let t1 = of_list [ 1; 2 ] ~offset:1 in
      let t2 = of_list [ 1; 2 ] ~offset:2 in
      let t3 = of_list [ 2; 3 ] ~offset:0 in
      let equal = equal Int.equal Int.equal in
      assert (equal t0 t1);
      assert (equal t0 t2);
      assert (equal t1 t2);
      assert (not (equal t0 t3));
      assert (not (equal t1 t3));
      assert (not (equal t2 t3))
    ;;
  end)
;;

(* test [With_key] *)
let%test_module _ =
  (module struct
    include With_key (Int)

    let%test _ = is_empty (create ~num_keys:1)
    let%test _ = Result.is_ok (of_alist [])
    let%test _ = Result.is_ok (of_alist [ 1, 1 ])
    let%test _ = Result.is_error (of_alist [ 1, 1; 1, 2 ])
    let%test _ = is_empty (of_alist_exn [])

    let%test_unit _ =
      let t = of_alist_exn [ 1, 2 ] in
      assert (length t = 1);
      assert (keys t = [ 1 ]);
      assert (data t = [ 2 ])
    ;;

    let%test_unit _ =
      let t = of_alist_exn [ 1, 2; 3, 4 ] in
      assert (length t = 2);
      assert (keys t = [ 1; 3 ] || keys t = [ 3; 1 ]);
      assert (data t = [ 2; 4 ] || data t = [ 4; 2 ])
    ;;
  end)
;;

let%test_module _ =
  (module struct
    include With_key (Int)

    let equal = equal Int.equal Int.equal

    let test_filter_map input ~f expect =
      equal (filter_map (of_alist_exn input) ~f) (of_alist_exn expect)
    ;;

    let%test _ = test_filter_map [] ~f:(fun _ -> assert false) []
    let%test _ = test_filter_map [ 1, 2 ] ~f:(fun _ -> None) []
    let%test _ = test_filter_map [ 1, 2 ] ~f:(fun x -> Some x) [ 1, 2 ]
    let%test _ = test_filter_map [ 1, 2 ] ~f:(fun x -> Some (x + 1)) [ 1, 3 ]

    let%test _ =
      test_filter_map [ 1, 2; 3, 4 ] ~f:(fun x -> if x = 2 then Some x else None) [ 1, 2 ]
    ;;

    let test_map_like map =
      let test input ~f expect =
        equal (map (of_alist_exn input) ~f) (of_alist_exn expect)
      in
      test [] ~f:(fun _ -> assert false) []
      && test [ 1, 2 ] ~f:(( + ) 3) [ 1, 5 ]
      && test [ 1, 2; 3, 4 ] ~f:(( + ) 5) [ 1, 7; 3, 9 ]
    ;;

    let%test _ = test_map_like (fun t ~f -> mapi t ~f:(fun ~key:_ ~data -> f data))
    let%test _ = test_map_like map
  end)
;;

let%test_module _ =
  (module struct
    let () = debug := true

    let%test_unit _ =
      (* Check that [set] replaces the key. *)
      let t = create ~num_keys:1 ~key_to_int:(fun _ -> 0) () in
      set t ~key:13 ~data:();
      set t ~key:14 ~data:();
      assert (keys t = [ 14 ])
    ;;

    let create ~num_keys : (int, _) t = create ~num_keys ~key_to_int:Fn.id ()

    let assert_empty t =
      assert (length t = 0);
      assert (to_alist t = []);
      assert (keys t = []);
      assert (data t = [])
    ;;

    let%test_unit _ =
      try
        ignore (create ~num_keys:(-1));
        assert false
      with
      | _ -> ()
    ;;

    let%test_unit _ = ignore (create ~num_keys:0)
    let%test_unit _ = ignore (create ~num_keys:1)
    let%test_unit _ = ignore (create ~num_keys:10_000)

    let%test_unit _ =
      let num_keys = 10 in
      let t = create ~num_keys in
      let key_is_valid key =
        try
          ignore (find t key);
          true
        with
        | _ -> false
      in
      assert (not (key_is_valid (-1)));
      for key = 0 to num_keys - 1 do
        assert (key_is_valid key);
        assert (is_none (find t key))
      done;
      assert (not (key_is_valid num_keys));
      assert_empty t
    ;;

    let table_data = data

    let%test_unit _ =
      let num_keys = 10 in
      let t = create ~num_keys in
      let key = 0 in
      let data = "zero" in
      add_exn t ~key ~data;
      assert (length t = 1);
      assert (find t key = Some data);
      for key = 1 to num_keys - 1 do
        assert (find t key = None)
      done;
      assert (to_alist t = [ key, data ]);
      assert (keys t = [ key ]);
      assert (table_data t = [ data ]);
      remove t key;
      assert_empty t
    ;;

    let%test_unit _ =
      let num_keys = 10 in
      let t = create ~num_keys in
      let key = 0 in
      let data = "zero" in
      add_exn t ~key ~data:"bad";
      set t ~key ~data;
      assert (find t key = Some data);
      for key = 1 to num_keys - 1 do
        assert (find t key = None)
      done;
      assert (to_alist t = [ key, data ]);
      assert (keys t = [ key ]);
      assert (table_data t = [ data ])
    ;;

    let%test_unit _ =
      let num_keys = 10 in
      let t = create ~num_keys in
      for key = 1 to 5 do
        add_exn t ~key ~data:(Int.to_string key)
      done;
      assert (length t = 5);
      for key = 1 to 5 do
        remove t key
      done;
      assert_empty t
    ;;

    let%test_unit _ =
      let num_keys = 10 in
      let t = create ~num_keys in
      for key = 0 to num_keys - 1 do
        add_exn t ~key ~data:(Int.to_string key)
      done;
      assert (length t = num_keys);
      for key = 0 to num_keys - 1 do
        remove t key
      done;
      assert_empty t
    ;;

    (* Additional tests for [[@@deriving binio]], [[@@deriving sexp]], [t_of_sexp],
       [filter_map{,i}], [map{,i}]. *)
    let%test_unit _ =
      let outer_sexp_of_t = sexp_of_t in
      let module M = struct
        module Table = With_key (Int)

        type alist = (int * int) list [@@deriving sexp_of]
        type t = int Table.t [@@deriving sexp_of]
      end
      in
      let open M in
      let empty = Table.of_alist_exn [] in
      let equal = equal Int.equal Int.equal in
      for n = 0 to 5 do
        let alist = List.init n ~f:(fun i -> i, i) in
        let t = Table.of_alist_exn alist in
        assert (equal t t);
        List.iter alist ~f:(fun (key', data') ->
          assert (existsi t ~f:(fun ~key ~data -> key = key' && data = data'));
          assert (exists t ~f:(fun data -> data = data')));
        assert (for_alli t ~f:(fun ~key ~data -> key = data));
        assert (for_all t ~f:(fun data -> 0 <= data && data < n));
        let sort alist = List.sort alist ~compare:(fun (i, _) (i', _) -> compare i i') in
        let alist' = sort (to_alist t) in
        if alist <> alist'
        then
          failwiths
            ~here:[%here]
            "Bounded_int_table alist bug"
            (t, alist, alist')
            [%sexp_of: t * alist * alist];
        let sexp = sexp_of_t t in
        let sexp' = outer_sexp_of_t Int.sexp_of_t Int.sexp_of_t t in
        if sexp <> sexp'
        then
          failwiths
            ~here:[%here]
            "Bounded_int_table sexp bug"
            (t, sexp, sexp')
            [%sexp_of: t * Sexp.t * Sexp.t];
        let ensure_equal message t t' =
          if not (equal t t')
          then
            failwiths
              ~here:[%here]
              "Bounded_int_table bug"
              (message, t, t')
              [%sexp_of: string * t * t]
        in
        ensure_equal "t_of_sexp" t (Table.t_of_sexp Int.t_of_sexp sexp);
        ensure_equal "filter_mapi" t (filter_mapi t ~f:(fun ~key ~data:_ -> Some key));
        ensure_equal "filter_map" t (filter_map t ~f:(fun data -> Some data));
        ensure_equal "filter_map None" empty (filter_map t ~f:(fun _ -> None));
        ensure_equal "filter_i true" t (filteri t ~f:(fun ~key:_ ~data:_ -> true));
        ensure_equal "filter_i false" empty (filteri t ~f:(fun ~key:_ ~data:_ -> false));
        ensure_equal "filter true" t (filter t ~f:(fun _ -> true));
        ensure_equal "filter false" empty (filter t ~f:(fun _ -> false));
        ensure_equal "filter_keys true" t (filter_keys t ~f:(fun _ -> true));
        ensure_equal "filter_keys false" empty (filter_keys t ~f:(fun _ -> false));
        ensure_equal "map" t (map t ~f:Fn.id);
        ensure_equal "mapi" t (mapi t ~f:(fun ~key:_ ~data -> data));
        ensure_equal
          "map and mapi"
          (map t ~f:(fun x -> x + 1))
          (mapi t ~f:(fun ~key:_ ~data -> data + 1));
        let module T = struct
          type t = int Table.t [@@deriving bin_io, sexp]
        end
        in
        let binable_m = (module T : Binable.S with type t = T.t) in
        ensure_equal
          "binio"
          t
          (Binable.of_string binable_m (Binable.to_string binable_m t))
      done
    ;;

    (* Test [clear] *)
    let%test_unit _ =
      let num_keys = 10 in
      let t = create ~num_keys in
      clear t;
      add_exn t ~key:5 ~data:"five";
      assert (length t = 1);
      assert (find t 5 = Some "five");
      clear t;
      assert_empty t;
      for key = 0 to num_keys - 1 do
        add_exn t ~key ~data:(Int.to_string key)
      done;
      assert (length t = num_keys);
      clear t;
      assert_empty t
    ;;
  end)
;;
