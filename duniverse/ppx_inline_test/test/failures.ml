(* Checking failures are reported properly, and make the overall
   test fail. *)

let%test _ = false
let%test "name1" = raise Exit
let%test_module "name2" = (module struct
  let%test _ = false
  let%test _ = false
  let%test _ = raise Exit
  let%test_module "name3" = (module struct
    let () = raise Exit
  end)
end)
let%test_module _ = (module struct
  let () = raise Exit
end)
