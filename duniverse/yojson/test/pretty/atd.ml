let massive_json =
  `List [
    `Assoc [("r1", `String "testing")];
    `Assoc [("r2", `List [`String "Some"; `Int 2])];
    `Assoc [("r2", `String "None")];
    `Assoc [("r3", `List [`String "Some"; `Int 3])];
    `Assoc [];
    `Assoc [("r4", `Bool true) ];
    `Assoc [("r5", `List [`String "Some"; `Int 5])];
    `Assoc [];
    `Assoc [("r6", `Int 6)];
    `Assoc [];
    `Assoc [("r7", `Int (-1_000))];
    `Assoc [("r8", `List [`Int 1; `Int 2; `Int 3])];
    `List [`String "foo"; `String "bar"];
    `List [];
    `Null;
    `List [`Int 1; `Int 2; `Int 3];
    `Int 99;
    `Assoc [("foo", `Int 7); ("bar", `Int 8); ("baz", `Int 43)];
    `Assoc [("foo2", `Int 5); ("bar2", `Int 6); ("baz2", `Int 41); ("42", `Int 42)];
    `List [`Int 100; `String "foo"];
    `List [`Int 100; `Int 200; `Int 42];
    `List [`Int 100; `Int 200; `Int (-1)];
    `List [
      `String "V1";
      `String "v22";
      `List [`String "V3"; `String "testing"];
      `List [`String "V44"; `Int 255];
      `List [`String "V5"; `String "None"];
      `List [`String "V5"; `List [`String "Some"; `Bool true]]
    ];
    `Assoc [("v2", `String "A")];
    `Assoc [("v2", `List [`String "B"; `Int 100])];
    `List [
      `String "C1";
      `List [`String "C2"; `Bool true];
      `List [`String "C2"; `Bool false]];
    `List[`Int 50; `Int 30; `Int (-1); `Int 400];
  ]

let pp_json fmt json =
  Format.pp_print_string fmt (Yojson.Safe.pretty_to_string ~std:true json)

let () =
  Format.printf "%a\n" pp_json massive_json
