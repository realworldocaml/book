open Core

let ensure_days = function
  | Some span -> span
  | None ->
    print_endline "enter days: ";
    Int.of_string In_channel.(input_line_exn stdin)

let add_days base span () =
  ensure_days span
  |> Date.add_days base
  |> Date.to_string
  |> print_endline

let add =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    [%map_open
      let base = anon ("base" %: date)
      and span = anon (maybe ("days" %: int)) in
      add_days base span
    ]

let () = Command.run add
