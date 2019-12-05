(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(*
  Simple finalisation test for arrays.
*)
let test_array_finaliser _ =
  let module Array = CArray in
  let finaliser_completed = ref false in
  let finalise a =
    begin
      assert_equal 10 (Array.length a);
      assert_equal [1;2;3;4;5;6;7;8;9;10] (Array.to_list a);
      finaliser_completed := true;
    end
  in
  let () =
    let p =
      let a = Array.make ~finalise int 10 in begin
        for i = 0 to 9 do a.(i) <- i + 1 done;
        Array.start a
      end in
    begin
      Gc.full_major ();
      assert_equal ~msg:"The finaliser was not run"
        false !finaliser_completed;
      assert_equal 1 !@p;
    end in
  begin
    Gc.full_major ();
    assert_equal ~msg:"The finaliser was run"
      true !finaliser_completed;
  end


(*
  Simple finalisation test for structs.
*)
let test_struct_finaliser _ =
  let module M = struct
    type s
    let s : s structure typ = structure "s"
    let i = field s "i" int32_t
    let c = field s "c" char
    let () = seal s

    let finaliser_completed = ref false

    let finalise s =
      begin
        assert_equal 10l (getf s i);
        assert_equal 'e' (getf s c);
        finaliser_completed := true;
      end

    let () =
      let p =
        let s = make ~finalise s in begin
          setf s i 10l;
          setf s c 'e';
          addr s
        end in
      begin
        Gc.full_major ();
        assert_equal ~msg:"The finaliser was not run"
          false !finaliser_completed;
        assert_equal 10l !@(from_voidp int32_t (to_voidp p));
      end

    let () =
      begin
        Gc.full_major ();
        assert_equal ~msg:"The finaliser was run"
          true !finaliser_completed;
      end
  end in ()


let suite = "Finaliser tests" >:::
  ["array finalisation"
    >:: test_array_finaliser;

   "struct finalisation"
    >:: test_struct_finaliser;
  ]


let _ =
  run_test_tt_main suite
