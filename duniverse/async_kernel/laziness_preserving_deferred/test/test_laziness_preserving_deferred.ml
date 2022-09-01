open Core
open Async
module L = Laziness_preserving_deferred

let () = Backtrace.elide := true
let value = "foo"

let sexp_of_test = function
  | None -> Sexp.Atom "Undetermined"
  | Some result -> [%sexp_of: string Or_error.t] result
;;

let%expect_test "Simple [of_eager] test" =
  let t = L.of_eager (Deferred.return value) in
  let result1 = L.weak_run t in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (Deferred.peek result1 : test)];
  [%expect {| (Ok foo) |}];
  let result2 = L.force t in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (Deferred.peek result1 : test)];
  print_s [%sexp (Deferred.peek result2 : test)];
  [%expect {|
    (Ok foo)
    (Ok foo) |}];
  return ()
;;

let%expect_test "Simple [of_lazy] test" =
  let t = L.of_lazy (Lazy_deferred.create (fun () -> Deferred.return value)) in
  let result1 = L.weak_run t in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (Deferred.peek result1 : test)];
  [%expect {| Undetermined |}];
  let result2 = L.force t in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (Deferred.peek result1 : test)];
  print_s [%sexp (Deferred.peek result2 : test)];
  [%expect {|
    (Ok foo)
    (Ok foo) |}];
  return ()
;;

let%expect_test "[of_lazy] with forced lazy deferred" =
  let lazy_deferred = Lazy_deferred.create (fun () -> Deferred.return value) in
  let t = L.of_lazy lazy_deferred in
  let result1 = L.weak_run t in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (Deferred.peek result1 : test)];
  [%expect {| Undetermined |}];
  let (_ : _) = Lazy_deferred.force lazy_deferred in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (Deferred.peek result1 : test)];
  [%expect {| (Ok foo) |}];
  return ()
;;

let%expect_test "Absent laziness, monad works just like deferred monad when weakly run" =
  let t =
    let open L.Let_syntax in
    let%bind x = return 1 in
    let%bind y = return 2 in
    return (x + y)
  in
  let%bind result = L.weak_run t in
  print_s [%sexp (result : int Or_error.t)];
  [%expect {| (Ok 3) |}];
  return ()
;;

let lazy_encounters_test ~run =
  let return_lazy x ~force =
    let lazy_deferred = Lazy_deferred.create (fun () -> Deferred.return x) in
    upon (Ivar.read force) (fun () ->
      ignore (Lazy_deferred.force lazy_deferred : _ Deferred.Or_error.t));
    L.of_lazy lazy_deferred
  in
  let force_y = Ivar.create () in
  let force_prod = Ivar.create () in
  let t =
    let open L.Let_syntax in
    let%bind x = return 1 in
    print_s [%message "" (x : int)];
    let%bind y = return_lazy 2 ~force:force_y in
    print_s [%message "" (y : int)];
    let%map () =
      let%bind sum = return (x + y) in
      print_s [%message "" (sum : int)];
      return ()
    and () =
      let%bind prod = return_lazy (x * y) ~force:force_prod in
      print_s [%message "" (prod : int)];
      return ()
    in
    print_s [%message "Done!"]
  in
  print_s [%message "*** Starting run ***"];
  let (_ : _) = run t in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%message "*** Forcing [y] ***"];
  Ivar.fill force_y ();
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%message "*** Forcing [prod] ***"];
  Ivar.fill force_prod ();
  Scheduler.yield_until_no_jobs_remain ()
;;

let%expect_test "[weak_run] stops when it encounters unforced lazy values" =
  let%map () = lazy_encounters_test ~run:L.weak_run in
  [%expect
    {|
    "*** Starting run ***"
    (x 1)
    "*** Forcing [y] ***"
    (y 2)
    (sum 3)
    "*** Forcing [prod] ***"
    (prod 2)
    Done! |}]
;;

let%expect_test "[force] forces lazy values it encounters" =
  let%map () = lazy_encounters_test ~run:L.force in
  [%expect
    {|
    "*** Starting run ***"
    (x 1)
    (y 2)
    (sum 3)
    (prod 2)
    Done!
    "*** Forcing [y] ***"
    "*** Forcing [prod] ***" |}]
;;

(* This test ensures that the type of run in progress isn't cached. *)
let%expect_test "[force] is able to complete even if a [weak_run] has already started" =
  let t =
    let open L.Let_syntax in
    let%bind () = return () in
    let%bind () = L.of_lazy (Lazy_deferred.create Deferred.return) in
    return ()
  in
  let result1 = L.weak_run t in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  let%bind results =
    with_timeout
      (Time.Span.of_int_sec 3)
      (let%map result1 = result1
       and result2 = L.force t in
       Or_error.all_unit [ result1; result2 ])
  in
  print_s [%sexp (results : [ `Timeout | `Result of unit Or_error.t ])];
  [%expect {| (Result (Ok ())) |}];
  return ()
;;

exception E_for_test

let%expect_test "When binding, if [f] raises, the error is joined" =
  let t =
    let open L.Let_syntax in
    let%bind () = return () in
    raise E_for_test
  in
  let%bind result = L.weak_run t in
  print_s [%sexp (result : unit Or_error.t)];
  [%expect
    {|
    (Error
     (Laziness_preserving_deferred_test__Test_laziness_preserving_deferred.E_for_test)) |}];
  return ()
;;

let%expect_test "When binding, if [f] returns a deferred that raises, the error is joined"
  =
  let t =
    let open L.Let_syntax in
    let%bind () = return () in
    L.of_eager (Deferred.create (fun _ -> raise E_for_test))
  in
  let%bind result = L.weak_run t in
  print_s [%sexp (result : unit Or_error.t)];
  [%expect
    {|
    (Error
     (Laziness_preserving_deferred_test__Test_laziness_preserving_deferred.E_for_test)) |}];
  return ()
;;

let%expect_test "When binding, [f] is only invoked once across multiple readers" =
  let called_f = Set_once.create () in
  let t =
    let open L.Let_syntax in
    let%bind () = L.of_lazy (Lazy_deferred.create Deferred.return) in
    Set_once.set_exn called_f [%here] ();
    return ()
  in
  let%bind result1 = L.weak_run t
  and result2 = L.weak_run t
  and result3 = L.force t
  and result4 = L.force t in
  let results = Or_error.all_unit [ result1; result2; result3; result4 ] in
  print_s [%sexp (results : unit Or_error.t)];
  [%expect {| (Ok ()) |}];
  return ()
;;

module Monad_laws_test = struct
  open L.Let_syntax

  module Case = struct
    type t =
      { forced : bool
      ; behavior :
          [ `Eager_that_finishes
          | `Eager_that_never_finishes
          | `Lazy_that_finishes
          | `Lazy_that_raises
          | `Lazy_that_never_finishes
          ]
      }
    [@@deriving enumerate, sexp_of]
  end

  let run ~run =
    let test t1 t2 =
      let open Deferred.Let_syntax in
      let result1 = run t1 in
      let result2 = run t2 in
      let%map () = Scheduler.yield_until_no_jobs_remain () in
      let result1 = Deferred.peek result1 in
      let result2 = Deferred.peek result2 in
      match [%equal: string Or_error.t option] result1 result2 with
      | true -> `Passed
      | false -> `Failed (result1, result2)
    in
    (* This function and the [sexp_of_result] pattern below gives nicer error messages
       with each side of the monad law clearly labeled beside its computed value. *)
    let failure result _ = [%sexp_of: test] result in
    let make case x =
      let t =
        let of_lazy f = L.of_lazy (Lazy_deferred.create f) in
        match case.Case.behavior with
        | `Eager_that_finishes -> L.of_eager (Deferred.return x)
        | `Eager_that_never_finishes -> L.of_eager (Deferred.never ())
        | `Lazy_that_finishes -> of_lazy (fun () -> Deferred.return x)
        | `Lazy_that_raises -> of_lazy (fun () -> raise E_for_test)
        | `Lazy_that_never_finishes -> of_lazy Deferred.never
      in
      if case.forced then ignore (L.force t : _ Deferred.Or_error.t);
      t
    in
    let left_id_tests =
      let%map.List case = Case.all in
      let f = make case in
      match%map.Deferred test (return value >>= f) (f value) with
      | `Passed -> None
      | `Failed (result1, result2) ->
        let sexp_of_result1 = failure result1 in
        let sexp_of_result2 = failure result2 in
        Some
          [%message
            ""
              ~law:"Left identity"
              (case : Case.t)
              (return value >>= f : result1)
              (f value : result2)]
    in
    let right_id_tests =
      let%map.List case = Case.all in
      let m = make case value in
      match%map.Deferred test (m >>= return) m with
      | `Passed -> None
      | `Failed (result1, result2) ->
        let sexp_of_result1 = failure result1 in
        let sexp_of_result2 = failure result2 in
        Some
          [%message
            ""
              ~law:"Right identity"
              (case : Case.t)
              (m >>= return : result1)
              (m : result2)]
    in
    let associativity_tests =
      let%bind.List m_case = Case.all in
      let%bind.List f_case = Case.all in
      let%map.List g_case = Case.all in
      let m = make m_case value in
      let f = make f_case in
      let g = make g_case in
      match%map.Deferred test (m >>= f >>= g) (m >>= fun x -> f x >>= g) with
      | `Passed -> None
      | `Failed (result1, result2) ->
        let sexp_of_result1 = failure result1 in
        let sexp_of_result2 = failure result2 in
        Some
          [%message
            ""
              ~law:"Associativity"
              ~cases:
                ([%message
                  "" ~m:(m_case : Case.t) ~f:(f_case : Case.t) ~g:(g_case : Case.t)]
                 : Sexp.t)
              (m >>= f >>= g : result1)
              (m >>= fun x -> f x >>= g : result2)]
    in
    let%map.Deferred failures =
      [ left_id_tests; right_id_tests; associativity_tests ]
      |> List.concat
      |> Deferred.all
      |> Deferred.map ~f:List.filter_opt
    in
    match failures with
    | [] -> print_s [%message "All monad laws are respected."]
    | _ :: _ -> print_s [%message "Monad laws violated" ~_:(failures : Sexp.t list)]
  ;;
end

let%expect_test "[force] respects the monad laws" =
  let%map () = Monad_laws_test.run ~run:L.force in
  [%expect {| "All monad laws are respected." |}]
;;

let%expect_test "[weak_run] respects the monad laws" =
  let%map () = Monad_laws_test.run ~run:L.weak_run in
  [%expect {| "All monad laws are respected." |}]
;;
