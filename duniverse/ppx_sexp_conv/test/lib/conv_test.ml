open Ppx_sexp_conv_lib
open Conv

module Exceptions = struct

  let check_sexp exn string =
    match sexp_of_exn_opt exn with
    | None -> raise exn
    | Some sexp ->
      let sexp_as_string = Ppx_sexp_conv_lib.Sexp.to_string sexp in
      if sexp_as_string <> string
      then failwith sexp_as_string

  (* first global exceptions, checking different arities since they
     don't have the same representation *)
  exception Arg0 [@@deriving sexp]
  exception Arg1 of int [@@deriving sexp]
  exception Arg2 of int * int [@@deriving sexp]
  let%test_unit _ = check_sexp Arg0 "conv_test.ml.Exceptions.Arg0"
  let%test_unit _ = check_sexp (Arg1 1) "(conv_test.ml.Exceptions.Arg1 1)"
  let%test_unit _ = check_sexp (Arg2 (2, 3)) "(conv_test.ml.Exceptions.Arg2 2 3)"

  (* now local exceptions *)
  let exn (type a) a sexp_of_a =
    let module M = struct exception E of a [@@deriving sexp] end in
    M.E a

  let%test_unit "incompatible exceptions with the same name" =
    let e_int = exn 1 sexp_of_int in
    let e_string = exn "a" sexp_of_string in
    check_sexp e_int "(conv_test.ml.Exceptions.E 1)";
    check_sexp e_string "(conv_test.ml.Exceptions.E a)"

  let%test_unit "sexp converters are finalized properly for local exceptions" =
    Gc.compact ();
    Gc.compact ();
    let size_before = Ppx_sexp_conv_lib.Conv.Exn_converter.For_unit_tests_only.size () in
    let e = exn 2.5 sexp_of_float in
    let size_after_local_exn = Ppx_sexp_conv_lib.Conv.Exn_converter.For_unit_tests_only.size () in
    let e_finalized = ref false in
    Gc.finalise (fun _ -> e_finalized := true) e;
    check_sexp e         "(conv_test.ml.Exceptions.E 2.5)";
    Gc.compact ();
    Gc.compact ();
    assert !e_finalized;
    let size_after_gc = Ppx_sexp_conv_lib.Conv.Exn_converter.For_unit_tests_only.size () in
    assert (size_before + 1 = size_after_local_exn);
    assert (size_before = size_after_gc)
end
