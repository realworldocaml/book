(* If the visit-marking function has the same name in each file, it will be
   shadowed by this open. Because module A has fewer points than this module B,
   initializing module B will cause an index out of bounds error as it tries to
   update A's points array. *)
open A

(* The sequence expressions are just so that this module has more instrumented
   points than module A. *)
let () =
  ();
  ();
  ()
