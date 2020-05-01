let%test "alloc-test-ok" [@tags "x-library-inlining-sensitive"] = true

(* Let's just pretend we have a test, say an alloc test, that only works with
   inlining, and is currently broken. *)
let%test "alloc-test-fail" [@tags "x-library-inlining-sensitive"] = false

let%test_module "alloc-test-module2" =
  (module struct
    let%test _ = true
    let%test _ [@tags "x-library-inlining-sensitive"] = true
  end)

let%test_module "alloc-test-module" [@tags "x-library-inlining-sensitive"] =
  (module struct
    let%test "ok" = true
    let%test "fail" = false
  end)

let%test_module "early-cutoff-module" [@tags "x-library-inlining-sensitive"] =
  (module struct
    (* the toplevel of this module should not even be run when we aren't running
       the inlining-sensitive tests. *)
    let () = assert false
  end)
