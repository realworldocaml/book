(* Check that v of type [typ] matches with itself *)
let id_case typ typ_str v =
  Alcotest.test_case typ_str `Quick (fun () -> Alcotest.check typ typ_str v v)

let make_checks () =
  let open Alcotest in
  check unit "unit" () ();
  check bool "bool" true true;
  check int "int " 0 0;
  check int32

exception Zero
exception One of int
exception Two of int * char

let checked_exceptions () =
  let check_refl name exn =
    Alcotest.check_raises name exn (fun () -> raise exn)
  in
  check_refl "0-tuple" Zero;
  check_refl "1-tuple" (One 1);
  check_refl "2-tuple" (Two (1, 'a'))

let negated_testables () =
  Alcotest.(check (neg reject)) "!reject" () ();
  Alcotest.(check (neg @@ neg pass)) "!!pass" () ();
  Alcotest.(check (neg @@ neg @@ neg reject)) "!!!reject" () ();
  Alcotest.(check (neg char)) "!different" 'a' 'b'

let float_threshold () =
  Alcotest.(check (float 1.0) "within threshold") 0.1 0.2;
  Alcotest.(check (neg (float 0.01)) "not within threshold") 0.1 0.2

let sorted_list () =
  let int_compare : int -> int -> int = compare in
  Alcotest.(check (slist int int_compare)) "sorted" [ 1; 2; 3 ] [ 3; 2; 1 ]

let labeled_check () = Alcotest.(check' int) ~msg:"Foo" ~expected:1 ~actual:1

let () =
  let open Alcotest in
  run __FILE__
    [
      ( "reflexive basic",
        [
          id_case unit "unit" ();
          id_case bool "bool" true;
          id_case int "int" 1;
          id_case int32 "int32" Int32.max_int;
          id_case int64 "int64" Int64.max_int;
          id_case (float 0.0) "float" 1.0;
          id_case char "char" 'a';
          id_case string "string" "Lorem ipsum dolor sit amet.";
          id_case bytes "bytes" (Bytes.of_string "\x01\x02\x03");
        ] );
      ( "reflexive composite",
        [
          id_case (list int) "empty list" [];
          id_case (list char) "non-empty list" [ 'a'; 'b'; 'c' ];
          id_case (array unit) "empty array" [||];
          id_case (array int64) "non-empty array" Int64.[| zero; max_int |];
          id_case (option int) "option some" (Some 1);
          id_case (option int) "option none" None;
          id_case (result int unit) "result ok" (Ok 1);
          id_case (result int unit) "result error" (Error ());
          id_case (pair int char) "pair" (1, 'a');
          id_case (triple int bool string) "triple" (1, true, "a");
        ] );
      ( "negation",
        [
          test_case "checked exceptions" `Quick checked_exceptions;
          test_case "negated testables" `Quick negated_testables;
        ] );
      ( "fuzzy equality",
        [
          test_case "float thresholds" `Quick float_threshold;
          test_case "sorted list" `Quick sorted_list;
        ] );
      ("labeled check", [ test_case "passing" `Quick labeled_check ]);
    ]
