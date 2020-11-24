module Testable = struct
  let library_set =
    let open Mdx.Library in
    let equal = Set.equal in
    let pp fmt set =
      let l = Set.elements set in
      Fmt.string fmt "[ ";
      Fmt.(list ~sep:(const string "; ") pp) fmt l;
      Fmt.string fmt " ]"
    in
    Alcotest.testable pp equal
end

let test_require_from_line =
  let make_test ~line ~expected () =
    let open Mdx.Util.Result.Infix in
    let test_name = Printf.sprintf "require_from_line: %S" line in
    let expected = expected >>| Mdx.Library.Set.of_list in
    let test_fun () =
      let actual = Mdx.Block.require_from_line line in
      Alcotest.(check (result Testable.library_set string))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~line:"let x = 2 + 2" ~expected:(Ok []) ();
    make_test ~line:"#require \"a\""
      ~expected:(Ok [ { base_name = "a"; sub_lib = None } ])
      ();
    make_test ~line:"# #require \"a\";;"
      ~expected:(Ok [ { base_name = "a"; sub_lib = None } ])
      ();
    make_test ~line:"#require \"a,b.c,d\""
      ~expected:
        (Ok
           [
             { base_name = "a"; sub_lib = None };
             { base_name = "b"; sub_lib = Some "c" };
             { base_name = "d"; sub_lib = None };
           ])
      ();
  ]

let suite = ("Block", test_require_from_line)
