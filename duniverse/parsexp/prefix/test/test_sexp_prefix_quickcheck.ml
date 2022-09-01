open! Core
open! Import

let round_trip_prefix prefix ~len ~parser_input ~verbose =
  let prefix_signifier = Sexp_prefix.get_a_signifier prefix ~parser_input in
  let suffix = String.drop_prefix parser_input len in
  if verbose
  then
    print_s
      [%message (prefix : Sexp_prefix.t) (prefix_signifier : string) (suffix : string)];
  prefix_signifier ^ suffix
;;

let test_prefix state stack ~len ~parser_input ~verbose =
  Option.iter (Sexp_prefix.create state stack) ~f:(fun prefix ->
    let observed = round_trip_prefix prefix ~len ~parser_input ~verbose in
    if not ([%compare.equal: Sexp_string.Compare_sexps.t] parser_input observed)
    then raise_s [%message (len : int) (observed : string) (parser_input : string)])
;;

let test_input (saw_state : Coverage.Saw_state.t) parser_input =
  List.iter (state_after_every_prefix Many parser_input) ~f:(fun (len, state, stack) ->
    saw_state.f state;
    test_prefix state stack ~len ~parser_input ~verbose:false)
;;

(* Check that a value is unchanged after we round-trip some prefix of its string
   representation through [Prefix]. *)
let%expect_test _ =
  require_does_not_raise [%here] (fun () ->
    Coverage.with_state_coverage ~f:(fun saw_state ->
      Base_quickcheck.Test.run_exn (module Sexp_string) ~f:(test_input saw_state)));
  [%expect {| |}]
;;
