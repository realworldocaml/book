(*
   Unit tests for the Unique_name module.
*)

module U = Atd.Unique_name

let test_reserved_identifiers () =
  let u =
    U.init
      ~reserved_identifiers:["y"]
      ~reserved_prefixes:[]
      ~safe_prefix:"x_"
  in
  Alcotest.(check (option string)) "equal" None (U.translate_only u "a");
  Alcotest.(check string) "equal" "a" (U.translate u "a");
  Alcotest.(check string) "equal" "a" (U.translate u "a");
  Alcotest.(check (option string)) "equal" (Some "a") (U.translate_only u "a");
  Alcotest.(check string) "equal" "y_" (U.translate u "y_");
  Alcotest.(check string) "equal" "y2" (U.translate u "y");
  let u =
    U.init
      ~reserved_identifiers:["y"]
      ~reserved_prefixes:["y_"]
      ~safe_prefix:"x_"
  in
  Alcotest.(check string) "equal" "x_y_" (U.translate u "y")

let test_reserved_prefixes () =
  (try
     U.init
       ~reserved_identifiers:[]
       ~reserved_prefixes:[""]
       ~safe_prefix:"x_"
     |> ignore;
     assert false
   with _ -> ());
  (try
     U.init
       ~reserved_identifiers:[]
       ~reserved_prefixes:["x"]
       ~safe_prefix:"x_"
     |> ignore;
     assert false
   with _ -> ());
  let u =
    U.init
      ~reserved_identifiers:[]
      ~reserved_prefixes:["y_"]
      ~safe_prefix:"x_"
  in
  Alcotest.(check string) "equal" "a" (U.translate u "a");
  Alcotest.(check string) "equal" "x_y_a" (U.translate u "y_a");
  Alcotest.(check string) "equal" "x_y_a" (U.translate u "y_a");
  Alcotest.(check string) "equal" "x_y_a_" (U.translate u "x_y_a_");
  Alcotest.(check string) "equal" "x_y_a2" (U.translate u "x_y_a");
  Alcotest.(check (option string)) "equal"
    (Some "x_y_a") (U.reverse_translate u "x_y_a2")

let test_identifier_creation () =
  let u =
    U.init
      ~reserved_identifiers:[]
      ~reserved_prefixes:["y_"]
      ~safe_prefix:"x_"
  in
  Alcotest.(check string) "equal" "y" (U.translate u "y");
  Alcotest.(check string) "equal" "y_" (U.create u "y");
  Alcotest.(check string) "equal" "x_y_" (U.translate u "y_");
  Alcotest.(check string) "equal" "y2" (U.create u "y");
  Alcotest.(check string) "equal" "y2" (U.translate u "y2")

let test : unit Alcotest.test = "Unique_name", [
  "reserved identifiers", `Quick, test_reserved_identifiers;
  "reserved prefixes", `Quick, test_reserved_prefixes;
  "identifier creation", `Quick, test_identifier_creation;
]
