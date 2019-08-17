(* checking that the execution order is right (and that
   tests do run) *)

let count = ref 0
let check i =
  assert (
    match Sys.getenv "DONT_ASSUME_ALL_TESTS_RUN" with
    | (_ : string) -> true
    | exception Not_found -> !count = i
  );
  incr count

module F(X : sig val start : int end) = struct
  let () = check X.start
  let%test_unit _ = check (X.start + 1)
  let () = check (X.start + 2)
end

let () = check 0
let%test_unit _ = check 1
let () = check 2
let%test _ = check 3; true
let () = check 4

let%test_module _ = (module struct
  let () = check 5
  let%test_unit _ = check 6
  let () = check 7
  let%test _ = check 8; true
  let%test_module _ = (module struct
    let () = check 9
    module M = F(struct let start = 10 end)
    let () = check 13
  end)
  module M = F(struct let start = 14 end)
  let () = check 17
end)

let () = check 18
