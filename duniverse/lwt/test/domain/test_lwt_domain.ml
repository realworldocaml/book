(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test
open Lwt.Infix

let lwt_domain_test = [
  test "run_in_domain" begin fun () ->
    let pool = Lwt_domain.setup_pool ~name:"pool_1" 4 in
    let f () = 40 + 2 in
    Lwt_domain.detach pool f () >>= fun x ->
    Lwt.return (x = 42)
  end;
  test "run_in_main_domain" begin fun () ->
    let pool = Option.get (Lwt_domain.lookup_pool "pool_1") in
    let f () =
      Lwt_domain.run_in_main (fun () ->
        Lwt_unix.sleep 0.01 >>= fun () ->
        Lwt.return 42)
    in
    Lwt_domain.detach pool f () >>= fun x ->
    Lwt.return (x = 42)
  end;
  test "run_in_main_domain_exception" begin fun () ->
    let pool = Option.get (Lwt_domain.lookup_pool "pool_1") in
    let f () = Lwt_domain.detach pool (fun () ->
      Lwt_domain.run_in_main (fun () ->
        Lwt_unix.sleep 0.01 >>= fun () ->
        Lwt.return (5/0))) ()
    in
    Lwt.try_bind f
    (fun _ -> Lwt.return_false)
    (fun exn -> Lwt.return (exn = Division_by_zero))
  end;
  test "fib_domain" begin fun () ->
    let pool = Option.get (Lwt_domain.lookup_pool "pool_1") in
    let rec fib n =
      if n < 2 then n
      else fib (n - 1) + fib (n - 2)
    in
    let l1 =
      List.init 10 (fun i -> Lwt_domain.detach pool fib i) in
    let l2 =
      List.init 10 (fun i -> Lwt.return (fib i)) in
    let s1 = Lwt.all l1 in
    let s2 = Lwt.all l2 in
    Lwt_unix.sleep 0.01 >>= fun () ->
    Lwt.return (s1 = s2)
  end;
  test "invalid_num_domains" begin fun () ->
    let set () =
      let _ = Lwt_domain.setup_pool (-1) in
      Lwt.return_true
    in
    Lwt.try_bind (fun () -> set ())
      (fun _ -> Lwt.return_false)
      (fun exn ->
        Lwt.return (exn = Invalid_argument
        "Task.setup_pool: num_additional_domains must be at least 0"))
  end;
  test "detach_exception" begin fun () ->
    let pool = Option.get (Lwt_domain.lookup_pool "pool_1") in
    let r = Lwt_domain.detach pool (fun () -> 10 / 0) () in
    Lwt.try_bind (fun () -> r)
      (fun _ -> Lwt_domain.teardown_pool pool; Lwt.return_false)
      (fun exn -> Lwt_domain.teardown_pool pool;
          Lwt.return (exn = Division_by_zero))
  end;
  test "one_domain" begin fun () ->
    let p2 = Lwt_domain.setup_pool 1 ~name:"pool2" in
    let f n = n * 10 in
    Lwt_domain.detach p2 f 100 >>= fun x ->
      Lwt.return (x = 1000)
  end;
  test "pool_already_shutdown" begin fun () ->
    let p2 = Option.get (Lwt_domain.lookup_pool "pool2") in
    Lwt_domain.teardown_pool p2;
    Lwt.try_bind (fun () -> Lwt_domain.detach p2 (fun () -> Lwt.return_true) ())
      (fun _ -> Lwt.return_false)
      (fun exn -> Lwt.return
        (exn = Invalid_argument "pool already torn down"))
  end
]

let suite =
  suite "lwt_domain"
    (
     lwt_domain_test
    )
