open OUnit2

module M : sig
  type a = {
    a1  : int option;
    a2  : int list;
    a3  : int [@default 42];
    a4s : (int * int list) [@split];
    a5  : int;
  } [@@deriving show, create]

  type b = {
    b1  : int option;
    b2  : int list;
    b3  : int [@default 42];
    b4s : (int * int list) [@split];
    b5  : int [@main];
  } [@@deriving show, create]
end = struct
  type a = {
    a1  : int option;
    a2  : int list;
    a3  : int [@default 42];
    a4s : (int * int list) [@split];
    a5  : int;
  } [@@deriving show, create]

  type b = {
    b1  : int option;
    b2  : int list;
    b3  : int [@default 42];
    b4s : (int * int list) [@split];
    b5  : int [@main];
  } [@@deriving show, create]
end

let test_no_main ctxt =
  assert_equal ~printer:M.show_a
               { M.a1 = None; a2 = []; a3 = 42; a4s = 2, []; a5 = 1 }
               (M.create_a ~a4:2 ~a5:1 ());
  assert_equal ~printer:M.show_a
               { M.a1 = Some 1; a2 = [2]; a3 = 3; a4s = 4, [5]; a5 = 6 }
               (M.create_a ~a1:1 ~a2:[2] ~a3:3 ~a4:4 ~a4s:[5] ~a5:6 ())

let test_main ctxt =
  assert_equal ~printer:M.show_b
               { M.b1 = None; b2 = []; b3 = 42; b4s = 2, []; b5 = 1 }
               (M.create_b ~b4:2 1);
  assert_equal ~printer:M.show_b
               { M.b1 = Some 1; b2 = [2]; b3 = 3; b4s = 4, [5]; b5 = 6 }
               (M.create_b ~b1:1 ~b2:[2] ~b3:3 ~b4:4 ~b4s:[5] 6)

let suite = "Test deriving(create)" >::: [
    "test_no_main" >:: test_no_main;
    "test_main"    >:: test_main;
  ]

let _ = run_test_tt_main suite
