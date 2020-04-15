(* Non-tail position. *)
let () =
  let module M = struct end in
  print_endline "foo"

(* Tail position. *)
let f () =
  let module M = struct end in
  print_endline "foo"

(* Non-trivial nested module. *)
let () =
  let module M =
    struct
      let () =
        print_endline "foo"
    end
  in
  ()
