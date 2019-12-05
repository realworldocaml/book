(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

[@@@warning "-32-33-34"]
open OUnit2
open Ctypes


let testlib = Dl.(dlopen ~filename:"../clib/dlltest_functions_stubs.so" ~flags:[RTLD_NOW])


module Build_foreign_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                               and type 'a return = 'a) =
struct
  module M = Functions.Common(S)
  open M

  (*
    Call a function of type

       void (struct simple)

    where

       struct simple {
         int i;
         double f;
         struct simple *self;
       };
  *)
  let test_passing_struct _ =
    let module M = struct
      let s = make simple

      let () = begin
        setf s i 10;
        setf s f 14.5;
        setf s self (from_voidp simple null)
      end

      let v = accept_struct s

      let () = assert_equal 25 v
        ~printer:string_of_int

    end in ()


  (*
    Call a function of type

       struct simple(void)

    where

       struct simple {
         int i;
         double f;
         struct simple *self;
       };
  *)
  let test_returning_struct _ =
    let module M = struct
      let s = return_struct ()

      let () = assert_equal 20 (getf s i)
      let () = assert_equal 35.0 (getf s f)

      let t = getf s self

      let () = assert_equal 10 !@(t |-> i)
        ~printer:string_of_int
      let () = assert_equal 12.5 !@(t |-> f)
        ~printer:string_of_float

      let () = assert_equal (to_voidp !@(t |-> self)) (to_voidp t)

    end in ()
end

(*
  Check that attempts to use incomplete types for struct members are rejected.
*)
let test_incomplete_struct_members _ =
  let s = structure "s" in begin

    assert_raises IncompleteType
      (fun () -> field s "_" void);

    assert_raises IncompleteType
      (fun () -> field s "_" (structure "incomplete"));

    assert_raises IncompleteType
      (fun () -> field s "_" (union "incomplete"));
  end


(*
  Test that fields can be added to views over structs.
*)
let test_adding_fields_through_views _ =
  let module M = struct
    let struct_s = structure "struct_s"
    let s = typedef struct_s "s"
    let i = field s "i" int
    let j = field s "j" float
    let () = seal s
  end in ()


(*
  Test that OCaml types cannot be used as struct or union fields.
*)
let test_ocaml_types_rejected_as_fields _ =
  let module M = struct
    let s = structure "s"
    let () = assert_raises IncompleteType
      (fun () -> field s "o" ocaml_string)

    let u = union "u"
    let () = assert_raises IncompleteType
      (fun () ->
       let _ =  field u "o" ocaml_string in
       (* The error is currently only caught on sealing the union *)
       seal u)
  end in ()


(*
  Test reading and writing pointers to struct members.
*)
let test_pointers_to_struct_members _ =
  let module M = struct
    type s

    let styp : s structure typ = structure "s"
    let (-:) ty label = field styp label ty
    let i = int     -: "i"
    let j = int     -: "j"
    let k = ptr int -: "k"
    let () = seal styp

    let s = make styp

    let () = begin
      let sp = addr s in
      sp |-> i <-@ 10;
      sp |-> j <-@ 20;
      (sp |-> k) <-@ (sp |-> i);
      assert_equal ~msg:"sp->i = 10" ~printer:string_of_int
        10 (!@(sp |-> i));
      assert_equal ~msg:"sp->j = 20" ~printer:string_of_int
        20 (!@(sp |-> j));
      assert_equal ~msg:"*sp->k = 10" ~printer:string_of_int
        10 (!@(!@(sp |-> k)));
      (sp |-> k) <-@ (sp |-> j);
      assert_equal ~msg:"*sp->k = 20" ~printer:string_of_int
        20 (!@(!@(sp |-> k)));
      sp |-> i <-@ 15;
      sp |-> j <-@ 25;
      assert_equal ~msg:"*sp->k = 25" ~printer:string_of_int
        25 (!@(!@(sp |-> k)));
      (sp |-> k) <-@ (sp |-> i);
      assert_equal ~msg:"*sp->k = 15" ~printer:string_of_int
        15 (!@(!@(sp |-> k)));
    end
  end in ()


(*
  Test structs with union members.
*)
let test_structs_with_union_members _ =
  let module M = struct
    type u and s

    let complex64_eq =
      let open Complex in
      let eps = 1e-12 in
      fun { re = lre; im = lim } { re = rre; im = rim } ->
        abs_float (lre -. rre) < eps && abs_float (lim -. rim) < eps

    let utyp : u union typ = union "u"
    let (-:) ty label = field utyp label ty
    let uc = char      -: "uc"
    let ui = int       -: "ui"
    let uz = complex64 -: "uz"
    let () = seal utyp

    let u = make utyp

    let () = begin
      setf u ui 14;
      assert_equal ~msg:"u.ui = 14" ~printer:string_of_int
        14 (getf u ui);

      setf u uc 'x';
      assert_equal ~msg:"u.uc = 'x'" ~printer:(String.make 1)
        'x' (getf u uc);

      setf u uz { Complex.re = 5.55; im = -3.3 };
      assert_equal ~msg:"u.uz = 5.55 - 3.3i" ~cmp:complex64_eq
        ~printer:(fun z -> Printf.sprintf "{re=%f; im=%f}" z.Complex.re z.Complex.im)
        { Complex.re = 5.55; im = -3.3 } (getf u uz);
    end

    let styp : s structure typ = structure "s"
    let (-:) ty label = field styp label ty
    let si = int  -: "si"
    let su = utyp -: "su"
    let sc = char -: "sc"
    let () = seal styp

    let s = make styp

    let () = begin
      setf s si 22;
      setf s su u;
      setf s sc 'z';

      assert_equal ~msg:"s.si = 22" ~printer:string_of_int
        22 (getf s si);
      
      assert_equal ~msg:"s.su.uc = 0.0 - 3.3i" ~cmp:complex64_eq
        { Complex.re = 5.55; im = -3.3 } (getf (getf s su) uz);

      assert_equal ~msg:"s.sc = 'z'" ~printer:(String.make 1)
        'z' (getf s sc);
    end
  end in ()


(*
  Test structs with array members.
*)
let test_structs_with_array_members _ =
  let module M = struct
    type u and s

    let styp : s structure typ = structure "s"
    let (-:) ty label = field styp label ty
    let i = int            -: "i"
    let a = array 3 double -: "a"
    let c = char           -: "c"
    let () = seal styp

    let s = make styp

    module Array = CArray
    let arr = Array.of_list double [3.3; 4.4; 5.5]

    let () = begin
      setf s i 22;
      setf s a arr;
      setf s c 'z';

      assert_equal ~msg:"s.i = 22" ~printer:string_of_int
        22 (getf s i);
      
      assert_equal ~msg:"s.a[0] = 3.3" ~printer:string_of_float
        3.3 (getf s a).(0);

      assert_equal ~msg:"s.a[0] = 3.3" ~printer:string_of_float
        3.3 (getf s a).(0);

      assert_equal ~msg:"s.a[1] = 4.4" ~printer:string_of_float
        4.4 (getf s a).(1);

      assert_equal ~msg:"s.a[2] = 5.5" ~printer:string_of_float
        5.5 (getf s a).(2);

      assert_raises (Invalid_argument "index out of bounds")
        (fun () -> (getf s a).(3));

      assert_equal ~msg:"s.c = 'z'" ~printer:(String.make 1)
        'z' (getf s c);

      (* References to the array member should alias the original *)
      let arr' = getf s a in
      
      arr'.(0) <- 13.3;
      arr'.(1) <- 24.4;
      arr'.(2) <- 35.5;

      assert_equal ~msg:"s.a[0] = 13.3" ~printer:string_of_float
        13.3 (getf s a).(0);

      assert_equal ~msg:"s.a[1] = 24.4" ~printer:string_of_float
        24.4 (getf s a).(1);

      assert_equal ~msg:"s.a[2] = 35.5" ~printer:string_of_float
        35.5 (getf s a).(2);
    end
  end in ()


(*
  Test that attempting to update a sealed struct is treated as an error.
*)
let test_updating_sealed_struct _ =
  let styp = structure "sealed" in
  let _ = field styp "_" int in
  let () = seal styp in

  assert_raises (ModifyingSealedType "sealed")
    (fun () -> field styp "_" char)


(*
  Test that attempting to seal an empty struct is treated as an error.
*)
let test_sealing_empty_struct _ =
  let empty = structure "empty" in

  assert_raises (Unsupported "struct with no fields")
    (fun () -> seal empty)


(* 
   Check that references to fields aren't garbage collected while they're
   still needed.
*)
let test_field_references_not_invalidated _ =
  let module M = struct
    type s1 and s2

    (*
      struct s1 {
        struct s2 {
          int i;
        } s2;
      };
    *)
    let s1 : s1 structure typ = structure "s1"
    let () = (fun () ->
      let s2 : s2 structure typ = structure "s2" in
      let _ = field s2 "i" int in
      let () = seal s2 in
      let _ = field s1 "_" s2 in
      ()
    ) ()
    let () = begin
      Gc.full_major ();
      seal s1;
      assert_equal ~printer:string_of_int
        (sizeof int) (sizeof s1)
    end
  end in ()


(* 
   Check that references to ffi_type values for structs aren't collected while
   they're still needed
*)
let test_struct_ffi_type_lifetime _ =
  let module M = struct
    let f =
      let t = 
        void @->
        returning
          (begin
            let s = structure "one_int" in
            let _ = field s "i" int in
            let () = seal s in
            s
           end)
      in
      Foreign.foreign ~from:testlib "return_struct_by_value" t

    let () = Gc.full_major()
    let x = f ()
  end in ()


module Build_stub_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                            and type 'a return = 'a) =
struct
  open Functions
  include Build_foreign_tests(S)
  module N = Functions.Stubs(S)
  open N

  (*
    Test passing structs with union members.
  *)
  let test_passing_structs_with_union_members _ =
    let mkInt v =
      let t = make tagged in
      t @. tag <-@ 'i';
      (t @. num |-> i) <-@ v;
      t
    and mkDbl v =
      let t = make tagged in
      t @. tag <-@ 'd';
      (t @. num |-> d) <-@ v;
      t
    and readDbl t =
      assert_equal 'd' !@(t @. tag);
      !@(t @. num |-> d)
    in
    begin
      assert_equal 10.0 (readDbl (add_tagged_numbers (mkInt 3) (mkInt 7)));
      assert_equal 10.0 (readDbl (add_tagged_numbers (mkInt 3) (mkDbl 7.0)));
      assert_equal 10.0 (readDbl (add_tagged_numbers (mkDbl 3.0) (mkInt 7)));
      assert_equal 10.0 (readDbl (add_tagged_numbers (mkDbl 3.0) (mkDbl 7.0)));
    end


  (*
    Test passing structs with array members.
  *)
  let test_passing_structs_with_array_members _ =
    let mkTriple (x, y, z) =
      let t = make triple in
      t @. elements <-@ CArray.of_list double [x; y; z];
      t
    and readTriple t =
      match CArray.to_list (getf t elements) with
      | [x; y; z] -> (x, y, z)
      | _ -> assert false
    in
    begin
      assert_equal
        (10.0, 20.0, 30.0)
        (readTriple
           (add_triples
              (mkTriple (5.0, 12.0, 17.0))
              (mkTriple (5.0,  8.0, 13.0))))
    end
end

module Foreign_tests = Build_foreign_tests(Tests_common.Foreign_binder)
module Stub_tests = Build_stub_tests(Generated_bindings)


module Build_struct_stub_tests
    (S : Ctypes.TYPE
          with type 'a typ = 'a Ctypes.typ
           and type ('a, 's) field = ('a, 's) Ctypes.field) =
struct
  module M = Types.Struct_stubs(S)

  let retrieve_size name = 
    let f = Foreign.foreign ~from:testlib name (void @-> returning size_t) in
    Unsigned.Size_t.to_int (f ())
  let sizeof_s1 = retrieve_size "sizeof_s1"
  let alignmentof_s1 = retrieve_size "alignmentof_s1"
  let offsetof_x1 = retrieve_size "offsetof_x1"
  let offsetof_x2 = retrieve_size "offsetof_x2"
  let offsetof_x3 = retrieve_size "offsetof_x3"
  let offsetof_x4 = retrieve_size "offsetof_x4"
  let sizeof_s2 = retrieve_size "sizeof_s2"
  let alignmentof_s2 = retrieve_size "alignmentof_s2"
  let offsetof_y1 = retrieve_size "offsetof_y1"
  let offsetof_y2 = retrieve_size "offsetof_y2"
  let offsetof_y3 = retrieve_size "offsetof_y3"
  let offsetof_y4 = retrieve_size "offsetof_y4"
  let sizeof_s3 = retrieve_size "sizeof_s3"
  let alignmentof_s3 = retrieve_size "alignmentof_s3"
  let offsetof_z1 = retrieve_size "offsetof_z1"
  let offsetof_z2 = retrieve_size "offsetof_z2"
  let sizeof_s4 = retrieve_size "sizeof_s4"
  let alignmentof_s4 = retrieve_size "alignmentof_s4"
  let offsetof_z3 = retrieve_size "offsetof_z3"
  let offsetof_z4 = retrieve_size "offsetof_z4"
  let sizeof_s6 = retrieve_size "sizeof_s6"
  let alignmentof_s6 = retrieve_size "alignmentof_s6"
  let offsetof_v1 = retrieve_size "offsetof_v1"
  let offsetof_v2 = retrieve_size "offsetof_v2"

  (*
    Test that struct layout retrieved from C correctly accounts for missing
    fields.
  *)
  let test_missing_fields _ =
    begin
      assert_equal sizeof_s1
        (sizeof M.s1);

      assert_equal alignmentof_s1
        (alignment M.s1);

      assert_equal offsetof_x1
        (offsetof M.x1);

      assert_equal offsetof_x4
        (offsetof M.x4);
    end

  (*
    Test that struct layout retrieved from C correctly accounts for reordered
    fields.
  *)
  let test_reordered_fields _ =
    begin
      assert_equal sizeof_s2
        (sizeof M.s2);

      assert_equal alignmentof_s2
        (alignment M.s2);

      assert_equal offsetof_y1
        (offsetof M.y1);

      assert_equal offsetof_y2
        (offsetof M.y2);
    end

  (* Test that we can retrieve information about multiple structs with
     dependencies between them. *)
  let test_struct_dependencies _ =
    begin
      assert_equal sizeof_s3
        (sizeof M.s3);

      assert_equal alignmentof_s3
        (alignment M.s3);

      assert_equal offsetof_z1
        (offsetof M.z1);

      assert_equal offsetof_z2
        (offsetof M.z2);

      assert_equal sizeof_s4
        (sizeof M.s4);

      assert_equal alignmentof_s4
        (alignment M.s4);

      assert_equal offsetof_z3
        (offsetof M.z3);

      assert_equal offsetof_z4
        (offsetof M.z4);
    end


  (* Test that we can retrieve information for structs without tags that are
     identified through typedefs, e.g.
         typedef struct { int x; float y; } t;
   *)
  let test_tagless_structs _ =
    begin
      assert_equal sizeof_s6
        (sizeof M.s6);

      assert_equal alignmentof_s6
        (alignment M.s6);

      assert_equal offsetof_v1
        (offsetof M.v1);

      assert_equal offsetof_v2
        (offsetof M.v2);
    end


  module Build_call_tests
      (F : Cstubs.FOREIGN with type 'a result = 'a
                           and type 'a return = 'a) =
  struct
    module F = Functions.Common(F)
    open F
    open M

    let callback p = !@(p |-> x1) + !@(p |-> x4)

    (* Call a function passing two structs, one of which contains a function
       pointer which accepts an argument to the other.

       This is mostly testing that we can support complex dependencies together
       with retrieved layout.
    *)
    let test_struct_dependencies _ =
      let v5 = make s5 in
      let v1 = make s1 in
      begin
        setf v1 x1 10;
        setf v1 x4 20;
        setf v5 w1 callback;
        assert_equal 30
          (call_s5 (addr v1) (addr v5))
          ~printer:string_of_int;
      end
  end

end

module Struct_stubs_tests = Build_struct_stub_tests(Generated_struct_bindings)
module Combined_foreign_tests =
  Struct_stubs_tests.Build_call_tests(Tests_common.Foreign_binder)
module Combined_stub_tests =
  Struct_stubs_tests.Build_call_tests(Generated_bindings)


let suite = "Struct tests" >:::
  ["passing struct (foreign)"
   >:: Foreign_tests.test_passing_struct;

   "passing struct (stubs)"
   >:: Stub_tests.test_passing_struct;

   "returning struct (foreign)"
   >:: Foreign_tests.test_returning_struct;

   "returning struct (stubs)"
   >:: Stub_tests.test_returning_struct;

   "struct dependencies (foreign)"
   >:: Combined_foreign_tests.test_struct_dependencies;

   "struct dependencies (stubs)"
   >:: Combined_stub_tests.test_struct_dependencies;

   "incomplete struct members rejected"
   >:: test_incomplete_struct_members;

   "fields can be added to views over structs"
   >:: test_adding_fields_through_views;

   "ocaml_string cannot be used as a structure field"
   >:: test_ocaml_types_rejected_as_fields;

   "pointers to struct members"
   >:: test_pointers_to_struct_members;

   "structs with union members"
   >:: test_structs_with_union_members;

   "passing structs with union members (stubs)"
   >:: Stub_tests.test_passing_structs_with_union_members;

   "passing structs with array members (stubs)"
   >:: Stub_tests.test_passing_structs_with_array_members;

   "structs with array members"
   >:: test_structs_with_array_members;

   "updating sealed struct"
   >:: test_updating_sealed_struct;

   "sealing empty struct"
   >:: test_sealing_empty_struct;

   "field references not invalidated"
   >:: test_field_references_not_invalidated;

   "test struct ffi_type lifetime"
   >:: test_struct_ffi_type_lifetime;

   "test layout of structs with missing fields"
   >:: Struct_stubs_tests.test_missing_fields;

   "test layout of structs with reordered fields"
   >:: Struct_stubs_tests.test_reordered_fields;

   "test retrieving information about structs with dependencies"
   >:: Struct_stubs_tests.test_struct_dependencies;

   "test adding fields to tagless structs"
   >:: Struct_stubs_tests.test_tagless_structs;
  ]


let _ =
  run_test_tt_main suite
