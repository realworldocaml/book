open! Core_kernel
open! Expect_test_helpers_core
open! Moption

let print t =
  invariant ignore t;
  print_s
    [%message
      ""
        ~_:(t : int t)
        ~is_none:(is_none t : bool)
        ~is_some:(is_some t : bool)
        ~get:(get t : int option)
        ~get_some_exn:(Or_error.try_with (fun () -> get_some_exn t) : int Or_error.t)]
;;

let%expect_test "[create], [is_none], [is_some], [get], [get_some_exn], [set], \
                 [set_none], [set_some]"
  =
  let t = create () in
  print t;
  [%expect
    {|
    (()
     (is_none true)
     (is_some false)
     (get ())
     (get_some_exn (Error Moption.get_some_exn))) |}];
  set_some t 13;
  print t;
  [%expect
    {|
    ((13)
     (is_none false)
     (is_some true)
     (get (13))
     (get_some_exn (Ok 13))) |}];
  set_none t;
  print t;
  [%expect
    {|
    (()
     (is_none true)
     (is_some false)
     (get ())
     (get_some_exn (Error Moption.get_some_exn))) |}];
  set t (Some 13);
  print t;
  [%expect
    {|
    ((13)
     (is_none false)
     (is_some true)
     (get (13))
     (get_some_exn (Ok 13))) |}];
  set t None;
  print t;
  [%expect
    {|
    (()
     (is_none true)
     (is_some false)
     (get ())
     (get_some_exn (Error Moption.get_some_exn))) |}]
;;

let%expect_test "unsafe_get" =
  let t = create () in
  set_some t 42;
  print_s [%message (unsafe_get t : int)];
  [%expect {| ("unsafe_get t" 42) |}]
;;

let%test_unit "Optional syntax" =
  let open Optional_syntax in
  let t = create () in
  assert (
    match%optional t with
    | None -> true
    | Some _ -> false);
  set_some t 13;
  assert (
    match%optional t with
    | None -> false
    | Some num -> num = 13)
;;
