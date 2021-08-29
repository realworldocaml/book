open! Core_kernel
open! Int.Replace_polymorphic_compare

let%test_module _ =
  (module struct
    module Hashtbl = Pooled_hashtbl.Make (struct
        include Int

        let hash x = x
      end)

    let%test_unit "growing test/copy test" =
      let n = 100_000 in
      let tbl = Hashtbl.create ~size:16 () in
      for i = 0 to n - 1 do
        Hashtbl.set tbl ~key:i ~data:i
      done;
      let first_n = ref 0 in
      for _ = 1 to 2 do
        let loop_end = !first_n + n - 1 in
        for i = !first_n to loop_end do
          let x = n + i in
          Hashtbl.set tbl ~key:x ~data:x;
          Hashtbl.remove tbl i
        done;
        first_n := !first_n + n
      done;
      for i = !first_n to !first_n + n - 1 do
        assert (Hashtbl.find_exn tbl i = i)
      done;
      let tbl_copy = Hashtbl.copy tbl in
      assert (Hashtbl.length tbl = Hashtbl.length tbl_copy);
      for i = !first_n to !first_n + n - 1 do
        assert (Hashtbl.find_exn tbl_copy i = i)
      done;
      (* assert no sharing *)
      Hashtbl.clear tbl;
      assert (Hashtbl.length tbl = 0);
      for i = !first_n to !first_n + n - 1 do
        assert (not (Hashtbl.mem tbl i));
        assert (Hashtbl.find_exn tbl_copy i = i)
      done
    ;;

    let%test_unit "adding elements when growth_allowed=false" =
      let n = 1_000 in
      let tbl = Hashtbl.create ~size:16 ~growth_allowed:false () in
      for i = 0 to n - 1 do
        Hashtbl.set tbl ~key:i ~data:i
      done
    ;;

    (* [large_int] creates an integer constant that is not representable on 32bits
       systems. *)
    let large_int a b c d = (a lsl 48) lor (b lsl 32) lor (c lsl 16) lor d

    (* Test for past bugs with very small tables and collisions *)
    let%test_unit "tiny map, colliding keys" =
      let tbl = Hashtbl.create ~size:3 () in
      let s1 = large_int 0x000b 0x2da5 0xbf2d 0xc34b in
      let s2 = large_int 0x0013 0x61e3 0x7f2d 0xc34b in
      let s3 = large_int 0 0x8995 0xff2d 0xc34b in
      Hashtbl.set tbl ~key:s1 ~data:0;
      assert (Hashtbl.find_exn tbl s1 = 0);
      Hashtbl.set tbl ~key:s2 ~data:1;
      assert (Hashtbl.find_exn tbl s2 = 1);
      Hashtbl.set tbl ~key:s3 ~data:2;
      assert (Hashtbl.find_exn tbl s3 = 2)
    ;;

    let%test_unit "collision test" =
      if Int.num_bits >= 63
      then (
        (* Under identity hashing, this creates a collision and chain on bucket zero... *)
        let t = Hashtbl.create ~size:20 () in
        let s1 = large_int 0 0x7490 0x3800 0x0000 in
        let s2 = large_int 0 0x020c 0x1800 0x0000 in
        Hashtbl.add_exn t ~key:s1 ~data:0;
        assert (Hashtbl.find_exn t s1 = 0);
        Hashtbl.add_exn t ~key:s2 ~data:1;
        assert (Hashtbl.find_exn t s1 = 0);
        assert (Hashtbl.find_exn t s2 = 1))
    ;;

    let%test_unit "simple iter/fold" =
      if Int.num_bits >= 63
      then (
        let tbl = Hashtbl.create ~size:64 () in
        let n = 100 in
        for i = 1 to 100 do
          Hashtbl.set tbl ~key:i ~data:i
        done;
        let isum = ref 0 in
        Hashtbl.iteri tbl ~f:(fun ~key ~data ->
          assert (key = data);
          isum := !isum + key);
        let fsum =
          Hashtbl.fold tbl ~init:0 ~f:(fun ~key ~data acc ->
            assert (key = data);
            acc + key)
        in
        assert (n = Hashtbl.length tbl);
        assert (fsum = !isum);
        let expected_sum = n * (n + 1) / 2 in
        assert (fsum = expected_sum))
    ;;
  end)
;;

let%test_module "unit tests from core" =
  (module Core_kernel_test.Hashtbl_unit_tests.Make (Pooled_hashtbl))
;;
