open Core

let add =
  let open Command.Let_syntax in
  Command.basic ~summary:"Add [days] to the [base] date"
    [%map_open
      let base = anon ("base" %: date)
      and span = anon ("days" %: int) in
      (fun () ->
         Date.add_days base span
         |> Date.to_string
         |> print_endline
      )
    ]

let diff =
  let open Command.Let_syntax in
  Command.basic ~summary:"Show days between [date1] and [date2]"
    [%map_open
      let date1 = anon ("date1" %: date)
      and date2 = anon ("date2" %: date) in
      (fun () ->
         Date.diff date1 date2
         |> printf "%d days\n"
      )
    ]

let command =
  Command.group ~summary:"Manipulate dates"
    [ "add", add; "diff", diff ]

let () = Command.run command
