open Core

let add =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    [%map_open
      let base = anon ("base" %: date)
      and span = anon ("days" %: int) in
    (fun () ->
       Date.add_days base span
       |> Date.to_string
       |> print_endline
    )
    ]

let () = Command.run add
