let foo =
  object
    method bar =
      ()

    method baz () () =
      ()
  end

(* Basic. *)
let () =
  foo#bar

(* With arguments. *)
let () =
  foo#baz () ()

(* In tail position. *)
let f () =
  foo#bar

(* In tail position with arguments. *)
let f () =
  foo#baz () ()

(* Object subexpression. *)
let helper () =
  foo

let () =
  (helper ())#bar
