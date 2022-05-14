open! Base
open Option_array
open Expect_test_helpers_core

let%expect_test "[match get] returning [None] does not allocate" =
  let t = of_array [| None |] in
  assert (
    require_no_allocation [%here] (fun () ->
      match get t 0 with
      | None -> true
      | Some _ -> false));
  [%expect {||}]
;;

let%expect_test ("[match get] returning [Some] allocates" [@tags "no-js"]) =
  let t = of_array [| Some 0 |] in
  let get_some () =
    match get t 0 with
    | None -> false
    | Some _ -> true
  in
  (* After inlining, [match get t 0 with] is:

     {[
       match
         let cheap_option = Uniform_array.get t 0 in
         if Cheap_option.is_some cheap_option
         then Some (Cheap_option.value_unsafe cheap_option)
         else None
       with
     ]}

     This situation is called "match-in-match" (the inner [if] is essentially a match).
     The OCaml compiler and Flambda optimizer don't handle match-in-match well, and so
     cannot eliminate the allocation of [Some].  Flambda2 is expected to eliminate the
     allocation, at which point we can [require_no_allocation] (possibly annotating the
     test with [@tags "fast-flambda"]).
  *)
  let compiler_eliminates_the_allocation = Config.flambda2 in
  (if compiler_eliminates_the_allocation
   then assert (require_no_allocation [%here] get_some)
   else
     let module Gc = Core.Gc.For_testing in
     let _, { Gc.Allocation_report.minor_words_allocated; _ } =
       Gc.measure_allocation get_some
     in
     assert (minor_words_allocated = 2));
  [%expect {||}]
;;
