(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test

exception Dummy_error

let state_is =
  Lwt.debug_state_is

let suite =
  suite "lwt_result" [
    test "maps"
      (fun () ->
         let x = Lwt_result.return 0 in
         let correct = Lwt_result.return 1 in
         Lwt.return (Lwt_result.map ((+) 1) x = correct)
      );

    test ">|= is a variant of map"
      (fun () ->
         let x = Lwt_result.return 0 in
         let correct = Lwt_result.return 1 in
         Lwt.return (Lwt_result.(>|=) x ((+) 1) = correct)
      );

    test "map, error case"
      (fun () ->
         let x = Lwt_result.fail 0 in
         Lwt.return (Lwt_result.map ((+) 1) x = x)
      );

    test "map_error"
      (fun () ->
         let x = Lwt_result.return 0 in
         Lwt.return (Lwt_result.map_error ((+) 1) x = x)
      );

    test "map_error, error case"
      (fun () ->
         let x = Lwt_result.fail 0 in
         let correct = Lwt_result.fail 1 in
         Lwt.return (Lwt_result.map_error ((+) 1) x = correct)
      );

    test "bind"
      (fun () ->
         let x = Lwt_result.return 0 in
         let correct = Lwt_result.return 1 in
         let actual = Lwt_result.bind x (fun y -> Lwt_result.return (y + 1)) in
         Lwt.return (actual = correct)
      );

    test "bind, error case"
      (fun () ->
         let x = Lwt_result.fail 0 in
         let actual = Lwt_result.bind x (fun y -> Lwt_result.return (y + 1)) in
         Lwt.return (actual = x)
      );
      
    test "bind_error"
      (fun () ->
         let x = Lwt_result.return 0 in
         let actual = Lwt_result.bind_error x (fun y -> Lwt_result.return (y + 1)) in
         Lwt.return (actual = x)
      );

    test "bind_error, error case"
      (fun () ->
         let x = Lwt_result.fail 0 in
         let correct = Lwt_result.return 1 in
         let actual = Lwt_result.bind_error x (fun y -> Lwt_result.return (y + 1)) in
         Lwt.return (actual = correct)
      );
      
    test "ok"
      (fun () ->
         let x = Lwt.return 0 in
         Lwt.return (Lwt_result.ok x = Lwt_result.return 0)
      );

    test "error"
      (fun () ->
        let x = Lwt.return 0 in
        Lwt.return (Lwt_result.error x = Lwt_result.fail 0)
      );

    test "catch"
      (fun () ->
         let x = Lwt.return 0 in
         Lwt.return (Lwt_result.catch x = Lwt_result.return 0)
      );

    test "catch, error case"
      (fun () ->
         let x = Lwt.fail Dummy_error in
         Lwt.return (Lwt_result.catch x = Lwt_result.fail Dummy_error)
      );

    test "get_exn"
      (fun () ->
         let x = Lwt_result.return 0 in
         Lwt.return (Lwt_result.get_exn x = Lwt.return 0)
      );

    test "get_exn, error case"
      (fun () ->
         let x = Lwt_result.fail Dummy_error in
         Lwt.return (Lwt_result.get_exn x = Lwt.fail Dummy_error)
      );

    test "bind_lwt"
      (fun () ->
         let x = Lwt_result.return 0 in
         let f y = Lwt.return (y + 1) in
         Lwt.return (Lwt_result.bind_lwt x f = Lwt_result.return 1)
      );

    test "bind_lwt, error case"
      (fun () ->
         let x = Lwt_result.fail 0 in
         let f y = Lwt.return (y + 1) in
         Lwt.return (Lwt_result.bind_lwt x f = Lwt_result.fail 0)
      );

    test "bind_lwt_error"
      (fun () ->
         let x = Lwt_result.return 0 in
         let f y = Lwt.return (y + 1) in
         Lwt.return (Lwt_result.bind_lwt_error x f = Lwt_result.return 0)
      );

    test "bind_lwt_error, error case"
      (fun () ->
         let x = Lwt_result.fail 0 in
         let f y = Lwt.return (y + 1) in
         Lwt.return (Lwt_result.bind_lwt_error x f = Lwt_result.fail 1)
      );

    test "bind_result"
      (fun () ->
         let x = Lwt_result.return 0 in
         let f y = Result.Ok (y + 1) in
         Lwt.return (Lwt_result.bind_result x f = Lwt_result.return 1)
      );

    test "bind_result, error case"
      (fun () ->
         let x = Lwt_result.fail 0 in
         let f y = Result.Ok (y + 1) in
         Lwt.return (Lwt_result.bind_result x f = Lwt_result.fail 0)
      );

    test "both ok"
      (fun () ->
         let p =
           Lwt_result.both
             (Lwt_result.return 0)
             (Lwt_result.return 1)
         in
         state_is (Lwt.Return (Result.Ok (0,1))) p
      );

    test "both only fst error"
      (fun () ->
         let p =
           Lwt_result.both
             (Lwt_result.fail 0)
             (Lwt_result.return 1)
         in
         state_is (Lwt.Return (Result.Error 0)) p
      );

    test "both only snd error"
      (fun () ->
         let p =
           Lwt_result.both
             (Lwt_result.return 0)
             (Lwt_result.fail 1)
         in
         state_is (Lwt.Return (Result.Error 1)) p
      );

    test "both error, fst"
      (fun () ->
         let p2, r2 = Lwt.wait () in
         let p =
           Lwt_result.both
             (Lwt_result.fail 0)
             p2
         in
         Lwt.wakeup_later r2 (Result.Error 1);
         Lwt.bind p (fun x -> Lwt.return (x = Result.Error 0))
      );

    test "both error, snd"
      (fun () ->
         let p1, r1 = Lwt.wait () in
         let p =
           Lwt_result.both
             p1
             (Lwt_result.fail 1)
         in
         Lwt.wakeup_later r1 (Result.Error 0);
         Lwt.bind p (fun x -> Lwt.return (x = Result.Error 1))
      );

    test "iter"
      (fun () ->
        let x = Lwt_result.return 1 in
        let actual = ref 0 in
        Lwt.bind
          (Lwt_result.iter (fun y -> actual := y + 1; Lwt.return_unit) x)
          (fun () -> Lwt.return (!actual = 2))
      );

    test "iter, error case"
      (fun () ->
        let x = Lwt_result.fail 1 in
        let actual = ref 0 in
        Lwt.bind
          (Lwt_result.iter (fun y -> actual := y + 1; Lwt.return_unit) x)
          (fun () -> Lwt.return (!actual <> 2))
      );

    test "iter_error"
      (fun () ->
        let x = Lwt_result.fail 1 in
        let actual = ref 0 in
        Lwt.bind
          (Lwt_result.iter_error (fun y -> actual := y + 1; Lwt.return_unit) x)
          (fun () -> Lwt.return (!actual = 2))
      );

    test "iter_error, success case"
      (fun () ->
        let x = Lwt_result.return 1 in
        let actual = ref 0 in
        Lwt.bind
          (Lwt_result.iter_error (fun y -> actual := y + 1; Lwt.return_unit) x)
          (fun () -> Lwt.return (!actual <> 2))
      );

    test "let*"
      (fun () ->
        let p1, r1 = Lwt.wait () in
        let p2, r2 = Lwt.wait () in
        let p' =
          let open Lwt_result.Syntax in
          let* s1 = p1 in
          let* s2 = p2 in
          Lwt.return (Result.Ok (s1 ^ s2))
        in
        Lwt.wakeup r1 (Result.Ok "foo");
        Lwt.wakeup r2 (Result.Ok "bar");
        state_is (Lwt.Return (Result.Ok "foobar")) p'
      );

    test "and*"
      (fun () ->
        let p1, r1 = Lwt.wait () in
        let p2, r2 = Lwt.wait () in
        let p' =
          let open Lwt_result.Syntax in
          let* s1 = p1
          and* s2 = p2 in
          Lwt.return (Result.Ok (s1 ^ s2))
        in
        Lwt.wakeup r1 (Result.Ok "foo");
        Lwt.wakeup r2 (Result.Ok "bar");
        state_is (Lwt.Return (Result.Ok "foobar")) p'
      );

    test "let+/and+"
      (fun () ->
        let p1, r1 = Lwt.wait () in
        let p2, r2 = Lwt.wait () in
        let p' =
          let open Lwt_result.Syntax in
          let+ s1 = p1
          and+ s2 = p2 in
          s1 ^ s2
        in
        Lwt.wakeup r1 (Result.Ok "foo");
        Lwt.wakeup r2 (Result.Ok "bar");
        state_is (Lwt.Return (Result.Ok "foobar")) p'
      );
  ]
