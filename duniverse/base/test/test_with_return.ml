open! Import
open! With_return

let test_loop loop_limit jump_out =
  with_return (fun { return } ->
    for i = 0 to loop_limit do begin
      if i = jump_out then return (`Jumped_out i);
    end done;
    `Normal)
;;

let ( = ) = Poly.equal

let%test _ = test_loop 5 10 = `Normal
let%test _ = test_loop 10 5 = `Jumped_out 5
let%test _ = test_loop 5 5  = `Jumped_out 5

let test_nested outer inner =
  with_return (fun { return = return_outer } ->
    if outer = `Outer_jump then return_outer `Outer_jump;
    let inner_res =
      with_return (fun { return = return_inner } ->
        if inner = `Inner_jump_out_completely then return_outer `Inner_jump;
        if inner = `Inner_jump then return_inner `Inner_jump;
        `Inner_normal)
    in
    if outer = `Jump_with_inner then return_outer (`Outer_later_jump inner_res);
    `Outer_normal inner_res)
;;

let%test _ = test_nested `Outer_jump `Inner_jump                = `Outer_jump
let%test _ = test_nested `Outer_jump `Inner_jump_out_completely = `Outer_jump
let%test _ = test_nested `Outer_jump `Foo                       = `Outer_jump

let%test _ = test_nested `Jump_with_inner `Inner_jump_out_completely = `Inner_jump
let%test _ = test_nested `Jump_with_inner `Inner_jump = `Outer_later_jump `Inner_jump
let%test _ = test_nested `Jump_with_inner `Foo        = `Outer_later_jump `Inner_normal

let%test _ = test_nested `Foo `Inner_jump_out_completely = `Inner_jump
let%test _ = test_nested `Foo `Inner_jump = `Outer_normal `Inner_jump
let%test _ = test_nested `Foo `Foo = `Outer_normal `Inner_normal

let test_loop loop_limit jump_out =
  with_return_option (fun { return } ->
    for i = 0 to loop_limit do begin
      if i = jump_out then return (`Jumped_out i);
    end done)
;;

let ( = ) = Poly.equal

let%test _ = test_loop 5 10 = None
let%test _ = test_loop 10 5 = Some (`Jumped_out 5)
let%test _ = test_loop 5 5  = Some (`Jumped_out 5)
