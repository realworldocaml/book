open Parsetree
open OUnit2

let string_of_tyvar tyvar =
#if OCAML_VERSION >= (4, 05, 0)
  tyvar.Location.txt
#else
  tyvar
#endif

let test_free_vars ctxt =
  let free_vars = Ppx_deriving.free_vars_in_core_type in
  let (!!) li = List.map string_of_tyvar li in
  let printer li =
    List.map (Printf.sprintf "%S") li |> String.concat ", " in
  assert_equal ~printer
    !!(free_vars [%type: int]) [];
  assert_equal ~printer
    !!(free_vars [%type: 'a option]) ["a"];
  assert_equal ~printer
    !!(free_vars [%type: ('a, 'b) result]) ["a"; "b"];
  assert_equal ~printer
    !!(free_vars [%type: ('a, 'b * 'a) result]) ["a"; "b"];
  ()

let suite = "Test API" >::: [
    "test_free_vars" >:: test_free_vars;
  ]

let () = run_test_tt_main suite
