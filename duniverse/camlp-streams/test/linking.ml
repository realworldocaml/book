(* Test that we can link programs using libraries which use Stream.t but don't
   necessarily use camlp-streams. Test is most relevant on 4.02-4.06 before the
   Stdlib module was introduced. *)

let () =
  Stream.(empty (Stream.of_list Stream_stdlib.list))
