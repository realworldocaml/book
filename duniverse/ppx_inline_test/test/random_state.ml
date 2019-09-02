let test list =
  ListLabels.iter list ~f:(fun expect ->
    let actual = Random.int 1000 in
    if actual <> expect
    then failwith (Printf.sprintf "%d <> %d" actual expect))

(* Random state is repeatable: *)
let in_fresh_inline_test = [220; 256; 600]
let%test_unit _ = test in_fresh_inline_test
let%test_unit _ = test in_fresh_inline_test
let%test_unit _ = test in_fresh_inline_test

(* Random state can be overridden: *)
let after_random_init_0 = [752; 190; 154]
let%test_unit _ = Random.init 0; test after_random_init_0
let%test_unit _ = Random.init 0; test after_random_init_0
let%test_unit _ = Random.init 0; test after_random_init_0

(* Tests inside a functor restore the existing random state after they run: *)
module Make () = struct let%test_unit _ = () end
let%test_unit _ = Random.init 0; let module M = Make () in test after_random_init_0
let%test_unit _ = Random.init 0; let module M = Make () in test after_random_init_0
let%test_unit _ = Random.init 0; let module M = Make () in test after_random_init_0
