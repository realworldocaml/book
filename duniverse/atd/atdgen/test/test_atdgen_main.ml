open Atd.Import

let current_section = ref ""

let section =
  let first_section = ref true in
  fun name ->
    current_section := name;
    if !first_section then
      first_section := false

let errors = ref []

exception Failed

let fail () =
  errors := !current_section :: !errors;
  raise Failed

let check b =
  if not b then fail ()

let check_valid = function
  | None -> ()
  | Some _ -> fail ()

let check_invalid = function
  | None -> fail ()
  | Some _ -> ()

let expect_error f x =
  try
    ignore (f x);
    eprintf "Did not get expected error\n%!";
    fail ()
  with
    Atdgen_runtime.Ob_run.Error _
  | Atdgen_runtime.Oj_run.Error _ -> ()

let test_missing_record = {
  Test.b0 = 123;
  b1 = true
}

let test_extended_record = {
  Test.b0x = 55;
  b1x = false;
  b2x = "abc";
  b3x = Some "def";
  b4x = Some "ghi";
  b5x = 1.1;
}

let test_missing_tuple = (123, 4.56)


type internals1 = { int : int }
type internals2 = { float : float }

  (* Obj.magic 0.0, opaque_identity, and record fields

     Instead of using options (which may allocate), atdgen uses
     a default value for references that denote record fields that may
     not yet have been deserialized.

     For example, consider the following example in the test.ml
     generated code:

type extended = {
  b0x: int;
  b1x: bool;
  b2x: string;
  b3x: string option;
  b4x: string option;
  b5x: float
}

let get_extended_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_b0x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b1x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b2x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b3x = ref (None) in
        let field_b4x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_b5x = ref (0.5) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 21902 ->
              field_b0x := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 21903 ->
              field_b1x := (
                (
                  Atdgen_runtime.Ob_run.read_bool
                ) ib
              );
              bits0 := !bits0 lor 0x2;
           (* ... CODE ELIDED HERE ... *)
            | 21907 ->
              field_b5x := (
                (
                  Atdgen_runtime.Ob_run.read_float64
                ) ib
              );
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0xf then Atdgen_runtime.Ob_run.missing_fields
          [| !bits0 |] [| "b0"; "b1"; "b2"; "b4" |];
        (
          {
            b0x = !field_b0x;
            b1x = !field_b1x;
            b2x = !field_b2x;
            b3x = !field_b3x;
            b4x = !field_b4x;
            b5x = !field_b5x;
          }
         : extended)

     # Why Obj.magic?

     At code generation time we do not have a default
     value for the type of this field (we don't know what the type
     is), so we create one out of thin air with Obj.magic

     # Why 0.0?

     Atdgen does not run the type-checker, so it does not a-priori
     know if the field type is float (it may be a type alias of
     "float" or even depend on a functor parameter).

     If the type *is* float and the type-checker notices it
     statically, then it may allocate an unboxed float reference, and
     in particular unbox the default value passed at reference create
     time. If this default value was *not* a float, then the code could
     segfault. So in this case we must use a float value.

     If the type is *not* float, then passing a float value is still
     correct: the compiler will not try to unbox it, so a (word-sized)
     pointer will be stored in the reference.

     # Why Sys.opaque_identity?

     Starting from 4.03, the compiler is more clever at assuming
     things from values. When it sees the constant 0.0, it will infer
     in particular that the reference contains a float (so it may
     decide to unbox it!), etc. Notice that the compiler makes just
     the same assumptions about (Obj.magic 0.0) than about 0.0, the
     magic changes the type but not the value.

     Also in 4.03, the Sys.opaque_identity function was added in the
     Sys module; it is a compiler primitive of type ('a -> 'a) that
     prevents the compiler from assuming anything about its return value.

     In practice, using Sys.opaque_identity here avoids the segfault
     that happened without it on 4.03. Note that this may not be
     enough; in particular, (Sys.opaque_identity 0.0) is still
     recognizeably a value of "float" type to the compiler (only the
     value is unknown), so it would be legal for the compiler to still
     decide to unbox in the future!

     The long-term solution would be to stop using these unsafe
     Obj.magic and use an option type to store the reference fields in
     this case. This would be a more invasive change to the
     implementation.
  *)

let test_ocaml_internals () =
  section "ocaml internals";

  let opaque_identity =
    (* neat trick to fallback to just the identity if we are using
       a <4.03 version and Sys.opaque_identity is not available; found
       in
         https://github.com/LaurentMazare/tensorflow-ocaml/commit/111b4727cec992bab8bc67c22ccc8c31942ffbb2 *)
    let opaque_identity x = x in
    (* avoiding unused function warning and partial application warning. *)
    opaque_identity ();
    opaque_identity in

  let int = ref (Obj.magic (opaque_identity 0.0)) in
  Gc.compact ();
  int := 123;
  Gc.compact ();
  check ({ int = !int }.int = 123);

  let float = ref (Obj.magic 0) in
  Gc.compact ();
  float := 4.5;
  Gc.compact ();
  check ({ float = !float }.float = 4.5)

let test_biniou_missing_field () =
  section "biniou missing record fields";
  expect_error
    Test.extended_of_string (Test.string_of_base test_missing_record)

let test_biniou_missing_cell () =
  section "biniou missing tuple fields";
  expect_error
    Test.extended_tuple_of_string
    (Test.string_of_base_tuple test_missing_tuple)

let test_json_missing_field () =
  section "json missing record fields";
  expect_error
    Testj.extended_of_string (Testj.string_of_base test_missing_record)

let test_json_missing_cell () =
  section "json missing tuple fields";
  expect_error
    Testj.extended_tuple_of_string
    (Testj.string_of_base_tuple test_missing_tuple)

let test_json_extra_field_warning () =
  section "json extra field warning";
  ignore (Testj.base_of_string (Testj.string_of_extended test_extended_record))

let test_json_assoc_list () =
  section "json association list";
  let f l =
    let s = Testj.string_of_int_assoc_list l in
    check (Testj.int_assoc_list_of_string s = l)
  in
  f [];
  f [ ("a", 0) ];
  f [ ("a", 0); ("b", 1) ]

let test_json_assoc_array () =
  section "json association array";
  let f a =
    let s = Testj.string_of_int_assoc_array a in
    check (Testj.int_assoc_array_of_string s = a)
  in
  f [| |];
  f [| ("a", 0) |];
  f [| ("a", 0); ("b", 1) |]

let test_json_int_ocaml_float_gen of_json to_json kind () =
  section ("json ints derived from ocaml floats: " ^ kind);
  let l1 = [0.; 0.1; -0.1; 0.6; -0.6] in
  check (of_json (to_json l1) = [0.; 0.; 0.; 1.; -1.]);

  let l2 = [ 12345678901234567890.; -12345678901234567890. ] in
  let l2' = of_json (to_json l2) in
  List.iter2 (fun x x' -> assert (abs_float (1. -. x /. x') < 1e-15)) l2 l2';

  expect_error to_json [infinity];
  expect_error to_json [neg_infinity];
  expect_error to_json [nan]

let test_json_int_ocaml_float () =
  test_json_int_ocaml_float_gen
    Test3j_j.unixtime_list_of_string
    Test3j_j.string_of_unixtime_list
    "int <ocaml repr=\"float\">" ();
  test_json_int_ocaml_float_gen
    Testj.unixtime_list_of_string
    Testj.string_of_unixtime_list
    "float <json repr=\"int\">" ()



let make_mixed_record_array n =
  Array.init n (
    fun i ->
      {
        Test.field0 = Some i;
        field1 = Some 0.555;
        field2 = Some "abcdefghijklmnopqrstuvwxyz";
        field3 = 12345678L;
        field4 = [| 1.23; 3.45; 4.56 |];
        field5 = None;
        field6 = None;
        field7 = `Case4 [ `Case1; `Case2 999; `Case3 "abcdefghij"; `Case4 [] ];
        field8 = [| "a"; "bc"; "def"; "ghij"; "klmno";
                    "pqrstu"; "vwxyz01"; "23456789" |];
        field9 = (
          1_000_000,
          0xff,
          '\xff',
          0xffff,
          0xffffffffl,
          0xffffffffffffffffL
        );
        field10 = true;
        field11 = false;
        field12 = [ (); () ];
        field13 = [ Some "abc"; None ];
        field14 = (2012, Some 3, None);
      }
  )

let make_mixed ~top_len ~tab_len ~ar_len =
  List.init top_len (fun _ ->
    (make_mixed_record_array tab_len, make_mixed_record_array ar_len)
  )


let test_correctness_data = {
  Test.x0 = Some 123;
  x1 = Some 1.23;
  x2 = make_mixed ~top_len:2 ~tab_len:2 ~ar_len:2;
  x3 = [
    {
      Test.field0 = Some 1234;
      field1 = Some 1e6;
      field2 = Some "Hello";
      field3 = 12345678L;
      field4 = [| 1.23; 3.45; 5.67 |];
      field5 = None;
      field6 = None;
      field7 = `Case4 [ `Case1; `Case2 999; `Case3 "abcdefghij"; `Case4 [] ];
      field8 = [| "abcdef"; "0123456789" |];
      field9 = (
        1_000_000,
        0xff,
        '\xff',
        0xffff,
        0xffffffffl,
        0xffffffffffffffffL
      );
      field10 = true;
      field11 = false;
      field12 = [ (); () ];
      field13 = [ Some "abc"; None ];
      field14 = (2012, Some 3, None);
    }
  ];
  x4 = 0x0807060504030201L;
}

let save file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let test_biniou_correctness () =
  section "biniou correctness";
  let x = test_correctness_data in
  let s = Test.string_of_test x in
  save "test.bin" s;
  let x' = Test.test_of_string s in
  let s' = Test.string_of_test x' in
  let x'' = Test.test_of_string s' in
  save "test-2.bin" s';
  if x <> x' then (
    eprintf "%s\n" (Bi_io.view s);
    eprintf "Data don't match\n";
    if s = s' then
      eprintf "Strings match\n"
    else
      eprintf "Strings don't match either\n";
    if x' = x'' then
      eprintf "2nd and 3rd generation data match\n"
    else
      eprintf "2nd and 3rd generation data differ\n";
    fail ()
  )

let test_json_correctness () =
  section "json correctness";
  let x = test_correctness_data in
  let s = Testj.string_of_test x in
  save "test.json" s;
  let x' = Testj.test_of_string s in
  let s' = Testj.string_of_test x' in
  let x'' = Testj.test_of_string s' in
  save "test-2.json" s';
  let std_x' = Testjstd.test_of_string s in
  let std_s' = Testjstd.string_of_test std_x' in
  let std_x'' = Testjstd.test_of_string std_s' in
  save "test-std.json" std_s';
  if x <> x' then (
    eprintf "%s\n" (Yojson.Safe.prettify s);
    eprintf "Data don't match\n";
    if s = s' then
      eprintf "Strings match\n"
    else
      eprintf "Strings don't match either\n";
    if x' = x'' then
      eprintf "2nd and 3rd generation data match\n"
    else
      eprintf "2nd and 3rd generation data differ\n";
    fail ()
  );
  check (std_x' = std_x'');
  assert (x = std_x')

let test_json_space () =
  section "json space";
  let s = Testj.string_of_test test_correctness_data in
  let pp = Yojson.Safe.prettify s in
  ignore (Testj.test_of_string pp)


let test_validators0 () =
  section "validators0";
  check_valid (Testv.validate_test [] test_correctness_data)

let test_validators1 () =
  section "validators1";
  let valid = (0, 1.) in
  let invalid = (1, 0.) in
  check_valid (Testv.validate_base_tuple [] valid);
  check_invalid (Testv.validate_base_tuple [] invalid);

  let x1 = {
    Test.b0x = 1;
    b1x = true;
    b2x = "abc";
    b3x = Some "def";
    b4x = Some "ghi";
    b5x = 1.1;
  }
  in
  check_invalid (Testv.validate_extended [] x1);
  let x2 = { x1 with Test.b1x = false } in
  check_valid (Testv.validate_extended [] x2);
  let x3 = { x2 with Test.b0x = -1 } in
  check_invalid (Testv.validate_extended [] x3)

let test_validators2 () =
  section "validators2";
  let v1 = `A in
  check_invalid (Testv.validate_p [] v1);
  let v2 = `B { Test.a = 0; b = true; c = `C } in
  check_valid (Testv.validate_p [] v2)

let test_validators3 () =
  section "validators3";
  let o = Some 0 in
  check_invalid (Testv.validate_option_validation [] o)

let test_validators4 () =
  section "validators4";
  let x = { Test.val2_x = { Test.val1_x = 0 };
            val2_y = Some { Test.val1_x = 1 } } in
  check_invalid (Testv.validate_val2 [] x)

let test_json_files () =
  section "json files";
  let x = Some 123 in
  let s = Atdgen_runtime.Util.Json.to_string Testj.write_intopt x in
  let x' = Atdgen_runtime.Util.Json.from_string Testj.read_intopt s in
  check (x = x');
  Atdgen_runtime.Util.Json.to_file Testj.write_intopt "test-json-files.json" x;
  let x'' = Atdgen_runtime.Util.Json.from_file Testj.read_intopt "test-json-files.json" in
  check (x = x'')

let test_json_streams () =
  section "json streams";
  let l = [ Some 1; None; Some 2; Some 3 ] in
  let s = Atdgen_runtime.Util.Json.list_to_string Testj.write_intopt l in
  let l' = Atdgen_runtime.Util.Json.list_from_string Testj.read_intopt s in
  check (l = l');
  Atdgen_runtime.Util.Json.list_to_file Testj.write_intopt "test-json-streams.json" l;
  let l'' =
    Atdgen_runtime.Util.Json.list_from_file Testj.read_intopt "test-json-streams.json"
  in
  check (l = l'')

let test_raw_json () =
  section "raw json";
  let x = { Test3j_t.foo = 12345;
            bar = `List [ `Int 12; `String "abc" ];
            baz = `Bool false }
  in
  let s = Test3j_j.string_of_t x in
  let x' = Test3j_j.t_of_string s in
  check (x = x')

let test_wrapping_ints () =
  section "ocaml wrapping - ints";
  let x = Test_lib.Natural.wrap 7 in
  let json = Testj.string_of_natural x in
  let x' = Testj.natural_of_string json in
  check (x = x');

  let biniou = Test.string_of_natural x in
  let x'' = Test.natural_of_string biniou in
  check (x = x'');

  try ignore (Testj.natural_of_string "-1"); check false
  with Failure _ -> ()

let test_double_wrapping () =
  section "ocaml wrapping - double wrapping";
  let x = Test_lib.Even_natural.wrap (Test_lib.Natural.wrap 10) in
  let json = Testj.string_of_even_natural x in
  let x' = Testj.even_natural_of_string json in
  check (x = x')

[@@@warning "-52"]
let test_wrapping_with_validation () =
  section "ocaml wrapping - with validation";
  let x = `Id "" in
  try ignore (Testv.validate_id [] x); check false
  with Failure "empty" -> ()

let test_ignored_wrap () =
  section "ocaml wrapping - wrap constructor without wrapper";
  let x = { Test.some_field = 0 } in
  try ignore (Testv.validate_no_real_wrap [] x); check false
  with Failure "passed" -> ()
[@@@warning "+52"]

let test_biniou_float32 () =
  section "check length of floats serialized as float32";
  let x = { Test.f32 = 1.23456789; Test.f64 = 1.98765432 } in
  let s = Test.string_of_floats x in
  let x' = Test.floats_of_string s in
  check Test.(abs_float (x.f32 -. x'.f32) < 1e-6);
  check (String.length s = 24)

let test_json_float_decimals () =
  section "print JSON floats with maximum number of decimal places";
  let x = {
    Testj.sqrt2_5 = sqrt 2.;
    small_2 = 0.000123456789;
    large_2 = 1234567890123.;
  } in
  let s = Testj.string_of_precision x in
  check (s = "{\"sqrt2_5\":1.4142,\"small_2\":0.00012,\"large_2\":1.2e+12}")

let test_patch () =
  section "read json record with null fields meaning (Some None)";
  let json = {| { "patch1": 1, "patch2": null } |} in
  let open Test3j_t in
  let x = Test3j_j.patch_of_string json in
  check (x.patch1 = Some (Some 1));
  check (x.patch2 = Some None);
  check (x.patch3 = None)

let test_adapted () =
  section "read and write a variant represented as a json object \
           with a `type` field";
  let json_a = {| {
    "type": "a",
    "thing": "abc",
    "other_thing": true,
    "ignored": 555
  } |} in
  let json_b =  {| {
    "thing": 123,
    "type": "b",
    "other_thing": true
  } |} in
  let expected_a = {|{"type":"a","thing":"abc","other_thing":true}|} in
  let expected_b = {|{"type":"b","thing":123}|} in
  let a = Test3j_j.adapted_of_string json_a in
  let b = Test3j_j.adapted_of_string json_b in
  let rewritten_a = Test3j_j.string_of_adapted a in
  let rewritten_b = Test3j_j.string_of_adapted b in
  check (expected_a = rewritten_a);
  check (expected_b = rewritten_b)

let test_one_field () =
  section "test variants represented with single-field json objects";
  let a_json = {| {"a": true} |} in
  let b_json = {| {"b": 17 } |} in
  let a = Test3j_j.sf_adapted_of_string a_json in
  let b = Test3j_j.sf_adapted_of_string b_json in
  check (a = `A true);
  check (b = `B 17);
  let a_json2 = Test3j_j.string_of_sf_adapted a in
  let b_json2 = Test3j_j.string_of_sf_adapted b in
  check (Test3j_j.sf_adapted_of_string a_json2 = a);
  check (Test3j_j.sf_adapted_of_string b_json2 = b)

let test_tag_field_emulation () =
  section "emulate the retired tag_field feature";
  let json_in = {| { "the_type": "a", "the_value": 123, "etc": "blah" } |} in
  let x = Test3j_j.tf_record_of_string json_in in
  check (x = { the_value = `A 123; etc = "blah" });
  let json_out = Test3j_j.string_of_tf_record x in
  let x2 = Test3j_j.tf_record_of_string json_out in
  check (x2 = x)

let test_tag_field_emulation_with_catchall () =
  section "emulate the retired tag_field feature, with a catch-all \
           for unknown tags";
  let json_in = {| { "the_type": "x", "the_value2": 3, "etc2": "blah" } |} in
  let x = Test3j_j.tf_record2_of_string json_in in
  check (x = { the_value2 = `Unknown ("x", Some (`Int 3)); etc2 = "blah" });
  let json_out = Test3j_j.string_of_tf_record2 x in
  let x2 = Test3j_j.tf_record2_of_string json_out in
  check (x2 = x)

let test_json_open_enum () =
  section "test <json open_enum>";
  let json_in = {| ["Alpha", "Gamma"] |} in
  let x = Test3j_j.sample_open_enums_of_string json_in in
  check (x = [`Alpha; `Other "Gamma"]);
  let json_out = Test3j_j.string_of_sample_open_enums x in
  let x2 = Test3j_j.sample_open_enums_of_string json_out in
  check (x2 = x)

let test_ambiguous_record () =
  section "test ambiguous record with json adapters";
  let json_in = {|{ambiguous:"x", not_ambiguous1:0}|} in
  let json_in' = {|{ambiguous:"x'", not_ambiguous2:1}|} in
  let x, x' =
    Test_ambiguous_record_j.ambiguous_of_string json_in,
    Test_ambiguous_record_j.ambiguous'_of_string json_in' in
  let json_out, json_out' =
    Test_ambiguous_record_j.string_of_ambiguous x,
    Test_ambiguous_record_j.string_of_ambiguous' x' in
  let x2, x2' =
    Test_ambiguous_record_j.ambiguous_of_string json_out,
    Test_ambiguous_record_j.ambiguous'_of_string json_out' in
  check ((x, x') = (x2, x2'))

let test_polymorphic_wrap () =
  section "test wrapping of polymorphic types";
  let json_in = {|["1", ["2"]]|} in
  let x =
    Test_polymorphic_wrap_j.t_of_string Yojson.Safe.read_string json_in in
  let json_out =
    Test_polymorphic_wrap_j.string_of_t Yojson.Safe.write_string x in
  let x2 =
    Test_polymorphic_wrap_j.t_of_string Yojson.Safe.read_string json_out in
  check (x = x2)

let all_tests = [
  test_ocaml_internals;
  test_biniou_missing_field;
  test_biniou_missing_cell;
  test_json_missing_field;
  test_json_missing_cell;
  test_json_extra_field_warning;
  test_json_assoc_list;
  test_json_assoc_array;
  test_json_int_ocaml_float;
  test_biniou_correctness;
  test_json_correctness;
  test_json_space;
  test_validators0;
  test_validators1;
  test_validators2;
  test_validators3;
  test_validators4;
  test_json_files;
  test_json_streams;
  test_raw_json;
  test_wrapping_ints;
  test_double_wrapping;
  test_wrapping_with_validation;
  test_ignored_wrap;
  test_biniou_float32;
  test_json_float_decimals;
  test_patch;
  test_adapted;
  test_one_field;
  test_tag_field_emulation;
  test_tag_field_emulation_with_catchall;
  test_json_open_enum;
  test_ambiguous_record;
]

(* TODO: use Alcotest to run the test suite. *)
let quality_test () =
  List.iter (fun f ->
    try f ()
    with Failed -> ())
    all_tests;
  match List.rev !errors with
  | [] ->
      ()
  | l ->
      eprintf "\nThe following tests failed:\n%s\n"
        (String.concat "\n" l);
      eprintf "*** FAILURE ***\n";
      exit 1

let () = quality_test ()





