let%test =
  let b = true in
  b
(* Above, a multi-line TEST (likewise BENCH) was indented wrong only when it
   started on the first line.  (That wasn't really a big problem.) *)

(* oUnit *)

module E = Example

let%test_module =
  (module struct
    let%test = false
    let%test =
      let b = true in
      b
    let%test "Name_test" =
      let b = true in                   (* tricky for Tuareg *)
      b
  end)

let%test_module "Name" =
  (module struct
    let%test_unit = ()
    let%test_unit =
      let () = () in
      ()
    let%test_unit "Name_unit" =
      let () = () in                    (* tricky for Tuareg *)
      ()

    let%test_unit =
      let msgcount = 10_000 in          (* tricky for Tuareg *)
      ()
  end)

let _ = printf "Hello, world!\n"
