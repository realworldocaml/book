(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

[@@@warning "-33"]

open OUnit2
open Ctypes


module Build_enum_stub_tests
    (S : Cstubs.Types.TYPE
          with type 'a typ = 'a Ctypes.typ
           and type ('a, 's) field = ('a, 's) Ctypes.field) =
struct
  module M = Types.Struct_stubs(S)
  open M

  let test_enum_struct_members _ =
    let reverse cell =
      let rec loop prev cell =
        match cell with
          None -> prev
        | Some c ->
          let n = getf !@c next in
          let () = setf !@c next prev in
          loop cell n
      in loop None cell
    in
    let as_list cell =
      let rec loop l = function
          None -> List.rev l
        | Some c ->
          loop (getf !@c frt :: l) (getf !@c next)
      in loop [] cell
    in
    let rec of_list l =
      match l with
        [] -> None
      | f :: fs ->
        let c = make fruit_cell in
        let n = of_list fs in
        let () = setf c frt f in
        let () = setf c next n in
        Some (addr c)
    in
    begin
      let open Types in
      let l = of_list [Apple; Apple; Pear; Banana] in
      assert_equal [Apple; Apple; Pear; Banana] (as_list l);
      assert_equal [Banana; Pear; Apple; Apple] (as_list (reverse l));
      assert_equal [] (as_list None);
    end

  let test_enum_arrays _ =
    let module Array = CArray in
    let a = Array.make bears 4 in
    begin
      a.(0) <- `Edward;
      a.(1) <- `Winnie;
      a.(2) <- `Paddington;
      a.(3) <- `Edward;
      assert_equal [`Edward; `Winnie; `Paddington; `Edward]
        (Array.to_list a)
    end

  module Build_call_tests
      (F : Cstubs.FOREIGN with type 'a result = 'a
                           and type 'a return = 'a) =
  struct
    module F = Functions.Stubs(F)
    open F
    open M

    let test_passing_returning_enums _ =
      let open Types in
      begin
        assert_equal Apple  (next_fruit Orange);
        assert_equal Banana (next_fruit Apple);
        assert_equal Pear   (next_fruit Banana);
        assert_equal Orange (next_fruit Pear);
      end

    let test_signed_enums _ =
      begin
        assert_equal (-1)  (classify_integer (-3));
        assert_equal 1     (classify_integer 4);
      end

    let test_default_enums _ =
      begin
        assert_equal 0 (out_of_range ())
      end

  end

end

module Enum_stubs_tests = Build_enum_stub_tests(Generated_struct_bindings)
module Combined_stub_tests = Enum_stubs_tests.Build_call_tests(Generated_bindings)


let suite = "Enum tests" >:::
  [
    "passing and returning enums"
    >:: Combined_stub_tests.test_passing_returning_enums;

    "enums with signed values"
    >:: Combined_stub_tests.test_signed_enums;

    "enums with default values"
    >:: Combined_stub_tests.test_default_enums;

    "enums as struct members"
    >:: Enum_stubs_tests.test_enum_struct_members;

    "arrays of enums"
    >:: Enum_stubs_tests.test_enum_arrays;
  ]


let _ =
  run_test_tt_main suite
