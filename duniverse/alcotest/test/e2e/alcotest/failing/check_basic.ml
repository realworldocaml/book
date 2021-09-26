(* Check that v of type [typ] matches with itself *)
let id_case typ typ_str v1 v2 =
  Alcotest.test_case typ_str `Quick (fun () -> Alcotest.check typ typ_str v1 v2)

let () =
  let open Alcotest in
  run ~verbose:true __FILE__
    [
      ( "different basic",
        [
          id_case bool "bool" true false;
          id_case int "int" 1 2;
          id_case int32 "int32" Int32.max_int Int32.min_int;
          id_case int64 "int64" Int64.max_int Int64.min_int;
          id_case (float 0.0) "float" 1.0 2.0;
          id_case char "char" 'a' 'b';
          id_case string "string" "Lorem ipsum" "dolor sit amet.";
          id_case bytes "bytes"
            (Bytes.of_string "\x01\x02\x03")
            (Bytes.of_string "\x01\x00\x03");
        ] );
      ( "different composite",
        [
          id_case (list char) "list" [ 'a'; 'b' ] [ 'a'; 'c' ];
          id_case (array int64) "array"
            Int64.[| zero; max_int |]
            Int64.[| zero; one; max_int |];
          id_case (option int) "option some" (Some 1) (Some 2);
          id_case (result int unit) "result" (Ok 1) (Error ());
          id_case (pair int char) "pair" (1, 'a') (1, 'b');
          id_case (triple int bool string) "triple" (1, true, "a")
            (1, false, "a");
        ] );
    ]
