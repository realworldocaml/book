open OUnit2

let get o = match o with Some v -> v | None -> assert false

type va = Aa | Ba | Ca [@@deriving enum, show]
let test_auto ctxt =
  assert_equal ~printer:string_of_int 0  (va_to_enum Aa);
  assert_equal ~printer:string_of_int 1  (va_to_enum Ba);
  assert_equal ~printer:string_of_int 2  (va_to_enum Ca);
  assert_equal ~printer:show_va       Aa (get (va_of_enum 0));
  assert_equal ~printer:show_va       Ba (get (va_of_enum 1));
  assert_equal ~printer:show_va       Ca (get (va_of_enum 2));
  assert_equal ~printer:string_of_int 0 min_va;
  assert_equal ~printer:string_of_int 2 max_va

type vm = Am [@value 1] | Bm [@value 3] | Cm [@@deriving enum, show]
let test_manual ctxt =
  assert_equal ~printer:string_of_int 1  (vm_to_enum Am);
  assert_equal ~printer:string_of_int 3  (vm_to_enum Bm);
  assert_equal ~printer:string_of_int 4  (vm_to_enum Cm);
  assert_equal ~printer:show_vm       Am (get (vm_of_enum 1));
  assert_equal ~printer:show_vm       Bm (get (vm_of_enum 3));
  assert_equal ~printer:show_vm       Cm (get (vm_of_enum 4));
  assert_equal ~printer:string_of_int 1 min_vm;
  assert_equal ~printer:string_of_int 4 max_vm

type pv = [ `A | `B | `C ] [@@deriving enum, show]
let test_poly ctxt =
  assert_equal ~printer:string_of_int 0  (pv_to_enum `A);
  assert_equal ~printer:string_of_int 1  (pv_to_enum `B);
  assert_equal ~printer:string_of_int 2  (pv_to_enum `C);
  assert_equal ~printer:show_pv       `A (get (pv_of_enum 0));
  assert_equal ~printer:show_pv       `B (get (pv_of_enum 1));
  assert_equal ~printer:show_pv       `C (get (pv_of_enum 2));
  assert_equal ~printer:string_of_int 0 min_pv;
  assert_equal ~printer:string_of_int 2 max_pv

let suite = "Test deriving(enum)" >::: [
    "test_auto"   >:: test_auto;
    "test_manual" >:: test_manual;
    "test_poly"   >:: test_poly;
  ]
