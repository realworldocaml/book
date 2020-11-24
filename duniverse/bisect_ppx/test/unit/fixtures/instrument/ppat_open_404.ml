(* Basic. *)
let () =
  match `A with
  | List.(`A | `B) -> ()

(* List. *)
let () =
  match [`A] with
  | List.[`A | `B] -> ()
  | _ -> ()

(* Array. *)
let () =
  match [|`A|] with
  | List.[|`A | `B|] -> ()
  | _ -> ()

(* Record. *)
let () =
  match {contents = `A} with
  | List.{contents = `A | `B} -> ()
