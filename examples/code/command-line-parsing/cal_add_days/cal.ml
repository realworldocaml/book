open Core

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Let_syntax.(
      let%map_open
        base = anon ("base" %: date)
      and days = anon ("days" %: int)
      in
      fun () ->
       Date.add_days base days
       |> Date.to_string
       |> print_endline)

let () = Command.run add
