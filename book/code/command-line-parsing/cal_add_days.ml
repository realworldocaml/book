open Core

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Spec.(
      empty
      +> anon ("base" %: date)
      +> anon ("days" %: int)
    )
    (fun base span () ->
       Date.add_days base span
       |> Date.to_string
       |> print_endline
    )

let () = Command.run add
