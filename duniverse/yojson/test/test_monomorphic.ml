let null = `Null
let bool = `Bool true
let other_bool = `Bool false
let int = `Int 42
let other_int = `Int 23
let float = `Float 42.0
let other_float = `Float 23.0
let string = `String "kameloso"
let other_string = `String "syggelekokle"

let scalar_equal () =
  let open Testable in
  Alcotest.(check yojson) "Equal Null" null null;
  Alcotest.(check (neg yojson)) "Unequal Null" null int;
  Alcotest.(check yojson) "Equal bool" bool bool;
  Alcotest.(check (neg yojson)) "Unequal bool" bool other_bool;
  Alcotest.(check (neg yojson)) "Not a bool" bool int;
  Alcotest.(check yojson) "Equal int" int int;
  Alcotest.(check (neg yojson)) "Unequal int" int other_int;
  Alcotest.(check (neg yojson)) "Not an int" int float;
  Alcotest.(check yojson) "Equal Float" float float;
  Alcotest.(check (neg yojson)) "Unequal Float" float other_float;
  Alcotest.(check yojson) "Equal strings" string string;
  Alcotest.(check (neg yojson)) "Unequal strings" string other_string;
  Alcotest.(check (neg yojson)) "Unequal strings" string float

let list_equal () =
  let open Testable in
  let list = `List [int; int; float] in
  let other_list = `List [int; other_int; float] in
  let empty_list = `List [] in
  Alcotest.(check yojson) "Equal lists" list list;
  Alcotest.(check (neg yojson)) "Unequal lists" list other_list;
  Alcotest.(check (neg yojson)) "Empty lists" list empty_list

let assoc_equal () =
  let open Testable in
  let assoc = `Assoc [("a", int); ("b", float)] in
  let other_assoc = `Assoc [("a", int); ("c", string)] in
  let empty_assoc = `Assoc [] in
  Alcotest.(check yojson) "Equal assocs" assoc assoc;
  Alcotest.(check (neg yojson)) "Unequal assocs" assoc other_assoc;
  Alcotest.(check (neg yojson)) "Empty assoc" assoc empty_assoc;
  let simple_key = `Assoc [("a", int)] in
  let duplicate_key = `Assoc [("a", int); ("a", int)] in
  let different_values_duplicate = `Assoc [("a", int); ("a", float)] in
  let flipped_values_duplicate = `Assoc [("a", float); ("a", int)] in
  Alcotest.(check (neg yojson)) "Duplicate keys don't unify" simple_key duplicate_key;
  Alcotest.(check yojson)
    "Duplicate keys should still be equal"
    different_values_duplicate
    different_values_duplicate;
  Alcotest.(check (neg yojson))
    "Duplicate keys not equal when different order"
    different_values_duplicate
    flipped_values_duplicate

let equality = [
  "Scalar equality", `Quick, scalar_equal;
  "List equality", `Quick, list_equal;
  "Assoc equality", `Quick, assoc_equal;
]
