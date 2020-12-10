(* build_info is not available when building tests.
   This test makes sure that one can parse the default value of build_info *)
let%test_unit _ = assert (Version_util.ocaml_version = "")
